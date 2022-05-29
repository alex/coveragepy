use pyo3::AsPyPointer;
use pyo3::IntoPy;
use std::collections::HashSet;
use std::os::raw::c_int;
use std::ptr;

#[pyo3::prelude::pyclass]
struct CTracer {
    started: bool,
    activity: bool,

    #[pyo3(get, set)]
    should_trace: Option<pyo3::PyObject>,

    #[pyo3(get, set)]
    warn: Option<pyo3::PyObject>,

    original_data: pyo3::Py<pyo3::PyAny>,

    should_trace_cache: pyo3::Py<pyo3::types::PyDict>,
    data_stack: Vec<DataStackEntry>,
}

struct DataStackEntry {
    disposition: Option<pyo3::Py<CFileDisposition>>,
    last_line: i32,
    started_context: bool,
}

#[pyo3::prelude::pymethods]
impl CTracer {
    #[new]
    fn new(py: pyo3::Python<'_>) -> CTracer {
        CTracer {
            started: false,
            activity: false,
            should_trace: None,
            warn: None,

            original_data: py.None(),

            should_trace_cache: pyo3::types::PyDict::new(py).into_py(py),
            data_stack: vec![],
        }
    }

    #[setter]
    fn data(&mut self, value: pyo3::Py<pyo3::PyAny>) {
        self.original_data = value;
    }

    #[setter]
    fn trace_arcs(&mut self, value: &pyo3::PyAny) {
        // XXX
        assert!(!value.is_true().unwrap());
    }

    #[setter]
    fn should_trace_cache(&mut self, value: &pyo3::PyAny) {
        // XXX
        assert!(!value.is_true().unwrap());
    }

    fn get_stats(&self) {}

    fn activity(&self) -> bool {
        self.activity
    }

    fn reset_activity(&mut self) {
        self.activity = false;
    }

    fn start(mut slf: pyo3::PyRefMut<'_, Self>) -> pyo3::PyRefMut<'_, Self> {
        unsafe {
            pyo3::ffi::PyEval_SetTrace(Some(trace_func), slf.as_ptr());
        }
        slf.started = true;

        slf
    }

    fn stop(&mut self, py: pyo3::Python<'_>) {
        self.started = false;

        // XXX: is this the right place for this?
        let original_data = self.original_data.as_ref(py);
        for (filename, disposition) in self.should_trace_cache.as_ref(py).iter() {
            let set: &pyo3::types::PySet = original_data
                .call_method1(
                    "setdefault",
                    (filename, pyo3::types::PySet::empty(py).unwrap()),
                )
                .unwrap()
                .extract()
                .unwrap();
            for value in disposition
                .downcast::<pyo3::PyCell<CFileDisposition>>()
                .unwrap()
                .borrow()
                .file_data
                .iter()
            {
                set.add(value).unwrap();
            }
        }
    }
}

impl CTracer {
    fn handle_trace(
        &mut self,
        py: pyo3::Python<'_>,
        event: c_int,
        frame: &PyFrame,
    ) -> pyo3::PyResult<()> {
        self.activity = true;

        match event {
            pyo3::ffi::PyTrace_CALL => self.handle_call(py, frame),
            pyo3::ffi::PyTrace_RETURN => self.handle_return(frame),
            pyo3::ffi::PyTrace_LINE => self.handle_line(py, frame),
            _ => Ok(()),
        }
    }

    fn handle_call(&mut self, py: pyo3::Python<'_>, frame: &PyFrame) -> pyo3::PyResult<()> {
        let code = frame.get_code();
        let filename: &pyo3::PyAny = unsafe { py.from_borrowed_ptr((*code).co_filename) };

        let disposition = match self.should_trace_cache.as_ref(py).get_item(filename) {
            Some(value) => value,
            None => {
                let d = self
                    .should_trace
                    .as_ref()
                    .unwrap()
                    .as_ref(py)
                    .call1((filename, frame))?;
                self.should_trace_cache.as_ref(py).set_item(filename, d)?;
                d
            }
        };

        let real_call = frame.get_lasti() < 0;
        let last_line = if real_call {
            unsafe { -(*code).co_firstlineno }
        } else {
            frame.get_line_number()
        };

        if !disposition.is_none() {
            let d = disposition.downcast::<pyo3::PyCell<CFileDisposition>>()?;
            self.data_stack.push(DataStackEntry {
                disposition: Some(d.extract().unwrap()),
                last_line,
                started_context: false,
            });
        } else {
            self.data_stack.push(DataStackEntry {
                disposition: None,
                last_line,
                started_context: false,
            });
        }

        Ok(())
    }

    fn handle_return(&mut self, _frame: &PyFrame) -> pyo3::PyResult<()> {
        if let Some(_entry) = self.data_stack.pop() {}
        Ok(())
    }

    fn handle_line(&mut self, py: pyo3::Python<'_>, frame: &PyFrame) -> pyo3::PyResult<()> {
        if let Some(mut entry) = self.data_stack.last_mut() {
            // if entry.file_tracer
            let (lineno_from, lineno_to) = {
                let l = frame.get_line_number();
                (l, l)
            };

            let mut disposition = entry.disposition.as_ref().unwrap().as_ref(py).borrow_mut();

            if lineno_from != -1 {
                for lineno in lineno_from..=lineno_to {
                    disposition.file_data.insert(lineno);
                    entry.last_line = lineno;
                }
            }
        }

        Ok(())
    }
}

struct PyFrame(*mut pyo3::ffi::PyFrameObject);

impl pyo3::IntoPy<pyo3::Py<pyo3::PyAny>> for &PyFrame {
    fn into_py(self, py: pyo3::Python<'_>) -> pyo3::Py<pyo3::PyAny> {
        unsafe {
            py.from_borrowed_ptr::<pyo3::PyAny>(self.0.cast::<pyo3::ffi::PyObject>())
                .into_py(py)
        }
    }
}

impl PyFrame {
    fn get_line_number(&self) -> i32 {
        unsafe { pyo3::ffi::PyFrame_GetLineNumber(self.0) }
    }

    fn get_lasti(&self) -> i32 {
        unsafe { (*self.0).f_lasti }
    }

    fn get_code(&self) -> *mut pyo3::ffi::PyCodeObject {
        #[cfg(Py_3_9)]
        unsafe { pyo3::ffi::PyFrame_GetCode(self.0) }
        #[cfg(not(Py_3_9))]
        unsafe { (*self.0).f_code }
    }
}

// This ought to be an `unsafe fn` but `PyEval_SetTrace` takes a regular `fn`
// so what can you do.
extern "C" fn trace_func(
    tracer: *mut pyo3::ffi::PyObject,
    frame: *mut pyo3::ffi::PyFrameObject,
    event: c_int,
    _arg: *mut pyo3::ffi::PyObject,
) -> c_int {
    let py = unsafe { pyo3::Python::assume_gil_acquired() };

    let mut slf = unsafe { py.from_borrowed_ptr::<pyo3::PyCell<CTracer>>(tracer) }.borrow_mut();

    if !slf.started {
        unsafe { pyo3::ffi::PyEval_SetTrace(None, ptr::null_mut()) };
        return 0;
    }

    let py_frame = PyFrame(frame);
    let result = match slf.handle_trace(py, event, &py_frame) {
        Ok(()) => 0,
        Err(e) => {
            e.restore(py);
            -1
        }
    };
    result
}

#[pyo3::prelude::pyclass]
struct CFileDisposition {
    #[pyo3(get, set)]
    original_filename: Option<pyo3::Py<pyo3::types::PyString>>,
    #[pyo3(get, set)]
    canonical_filename: Option<pyo3::Py<pyo3::types::PyString>>,
    #[pyo3(get, set)]
    source_filename: Option<pyo3::Py<pyo3::types::PyString>>,
    #[pyo3(get, set)]
    trace: bool,
    #[pyo3(get, set)]
    reason: Option<pyo3::Py<pyo3::types::PyString>>,
    #[pyo3(get, set)]
    file_tracer: Option<pyo3::Py<pyo3::PyAny>>,
    #[pyo3(get, set)]
    has_dynamic_filename: bool,

    file_data: HashSet<i32>,
}

#[pyo3::prelude::pymethods]
impl CFileDisposition {
    #[new]
    fn new() -> CFileDisposition {
        CFileDisposition {
            original_filename: None,
            canonical_filename: None,
            source_filename: None,
            trace: true,
            reason: None,
            file_tracer: None,
            has_dynamic_filename: false,

            file_data: HashSet::new(),
        }
    }
}

#[pyo3::prelude::pymodule]
fn tracer(_py: pyo3::Python<'_>, m: &pyo3::types::PyModule) -> pyo3::PyResult<()> {
    m.add_class::<CTracer>()?;
    m.add_class::<CFileDisposition>()?;

    Ok(())
}
