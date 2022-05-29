use pyo3::types::{PyBytes, PyCode, PyDict, PyFrame, PyModule, PySet, PyString};
use pyo3::AsPyPointer;
use pyo3::IntoPy;
use std::collections::HashSet;
use std::os::raw::c_int;
use std::ptr;

// TODO: These change by Python version!
const YIELD_VALUE: u8 = 86;
const YIELD_FROM: u8 = 72;

#[pyo3::prelude::pyclass]
struct CTracer {
    started: bool,
    activity: bool,

    #[pyo3(get, set)]
    should_trace: Option<pyo3::PyObject>,

    #[pyo3(get, set)]
    warn: Option<pyo3::PyObject>,

    #[pyo3(get, set)]
    should_trace_cache: pyo3::Py<PyDict>,

    #[pyo3(get, set)]
    trace_arcs: bool,

    original_data: pyo3::Py<pyo3::PyAny>,

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
            trace_arcs: false,

            original_data: py.None(),

            should_trace_cache: PyDict::new(py).into_py(py),
            data_stack: vec![],
        }
    }

    #[setter]
    fn data(&mut self, value: pyo3::Py<pyo3::PyAny>) {
        self.original_data = value;
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
            let set: &PySet = original_data
                .call_method1("setdefault", (filename, PySet::empty(py).unwrap()))
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
            pyo3::ffi::PyTrace_RETURN => self.handle_return(py, frame),
            pyo3::ffi::PyTrace_LINE => self.handle_line(py, frame),
            _ => Ok(()),
        }
    }

    fn handle_call(&mut self, py: pyo3::Python<'_>, frame: &PyFrame) -> pyo3::PyResult<()> {
        let code = frame_get_code(py, frame);
        let filename = code_get_filename(py, code);

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

        let real_call = frame_get_lasti(frame) < 0;
        let last_line = if real_call {
            -code_get_firstlineno(code)
        } else {
            frame_get_line_number(frame)
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

    fn handle_return(&mut self, py: pyo3::Python<'_>, frame: &PyFrame) -> pyo3::PyResult<()> {
        if let Some(entry) = self.data_stack.pop() {
            if let Some(disposition) = entry.disposition {
                if self.trace_arcs {
                    let code_obj = frame_get_code(py, frame);
                    let code = code_get_code(py, code_obj);
                    let lasti = frame_get_lasti(frame) as usize;

                    let mut is_yield = false;
                    let mut is_yield_from = false;
                    if lasti < code.len() {
                        is_yield = code[lasti] == YIELD_VALUE;
                        if lasti + 2 < code.len() {
                            is_yield_from = code[lasti + 2] == YIELD_FROM;
                        }
                    }
                    if !(is_yield || is_yield_from) {
                        let pair = pack_pair(entry.last_line, -code_get_firstlineno(code_obj));
                        disposition.as_ref(py).borrow_mut().file_data.insert(pair);
                    }
                }
            }
        }
        Ok(())
    }

    fn handle_line(&mut self, py: pyo3::Python<'_>, frame: &PyFrame) -> pyo3::PyResult<()> {
        if let Some(mut entry) = self.data_stack.last_mut() {
            let mut disposition = if let Some(ref d) = entry.disposition {
                d.as_ref(py).borrow_mut()
            } else {
                return Ok(());
            };

            let (lineno_from, lineno_to) = {
                let l = frame_get_line_number(frame);
                (l, l)
            };

            if lineno_from != -1 {
                for lineno in lineno_from..=lineno_to {
                    if self.trace_arcs {
                        let value = pack_pair(entry.last_line, lineno);
                        disposition.file_data.insert(value);
                    } else {
                        disposition.file_data.insert(lineno.into());
                    }
                    entry.last_line = lineno;
                }
            }
        }

        Ok(())
    }
}

fn pack_pair(v1: i32, v2: i32) -> i64 {
    let mut packed = 0i64;
    let v1 = if v1 < 0 {
        packed |= 1 << 40;
        -v1 as i64
    } else {
        v1 as i64
    };
    let v2 = if v2 < 0 {
        packed |= 1 << 41;
        -v2 as i64
    } else {
        v2 as i64
    };
    packed | (v2 << 20) | v1
}

fn frame_get_line_number(frame: &PyFrame) -> i32 {
    unsafe { pyo3::ffi::PyFrame_GetLineNumber(frame.as_ptr().cast::<pyo3::ffi::PyFrameObject>()) }
}

fn frame_get_code<'p>(py: pyo3::Python<'p>, frame: &'p PyFrame) -> &'p PyCode {
    unsafe {
        #[cfg(Py_3_9)]
        let code = pyo3::ffi::PyFrame_GetCode(frame.as_ptr());
        #[cfg(not(Py_3_9))]
        let code = (*frame.as_ptr().cast::<pyo3::ffi::PyFrameObject>()).f_code;

        py.from_borrowed_ptr(code.cast())
    }
}

fn frame_get_lasti(frame: &PyFrame) -> i32 {
    unsafe { (*frame.as_ptr().cast::<pyo3::ffi::PyFrameObject>()).f_lasti }
}

fn code_get_filename<'p>(py: pyo3::Python<'p>, code: &'p PyCode) -> &'p pyo3::PyAny {
    unsafe { py.from_borrowed_ptr((*code.as_ptr().cast::<pyo3::ffi::PyCodeObject>()).co_filename) }
}

fn code_get_firstlineno(code: &PyCode) -> i32 {
    unsafe { (*code.as_ptr().cast::<pyo3::ffi::PyCodeObject>()).co_firstlineno }
}

fn code_get_code<'p>(py: pyo3::Python<'p>, code: &'p PyCode) -> &'p [u8] {
    let py_code_bytes: &PyBytes =
        unsafe { py.from_owned_ptr((*code.as_ptr().cast::<pyo3::ffi::PyCodeObject>()).co_code) };
    py_code_bytes.as_bytes()
}

// This ought to be an `unsafe fn` but `PyEval_SetTrace` takes a regular `fn`
// so what can you do.
extern "C" fn trace_func(
    tracer: *mut pyo3::ffi::PyObject,
    frame: *mut pyo3::ffi::PyFrameObject,
    event: c_int,
    _arg: *mut pyo3::ffi::PyObject,
) -> c_int {
    // If we use `pyo3::Python::assume_gil_acquired()` then nothing ever
    // empties the `register_owned` stack.
    let gil = pyo3::Python::acquire_gil();
    let py = gil.python();

    let mut slf = unsafe { py.from_borrowed_ptr::<pyo3::PyCell<CTracer>>(tracer) }.borrow_mut();

    if !slf.started {
        unsafe { pyo3::ffi::PyEval_SetTrace(None, ptr::null_mut()) };
        return 0;
    }

    let py_frame = unsafe { py.from_borrowed_ptr::<PyFrame>(frame.cast()) };
    let result = match slf.handle_trace(py, event, py_frame) {
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
    original_filename: Option<pyo3::Py<PyString>>,
    #[pyo3(get, set)]
    canonical_filename: Option<pyo3::Py<PyString>>,
    #[pyo3(get, set)]
    source_filename: Option<pyo3::Py<PyString>>,
    #[pyo3(get, set)]
    trace: bool,
    #[pyo3(get, set)]
    reason: Option<pyo3::Py<PyString>>,
    #[pyo3(get, set)]
    file_tracer: Option<pyo3::Py<pyo3::PyAny>>,
    #[pyo3(get, set)]
    has_dynamic_filename: bool,

    file_data: HashSet<i64>,
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
fn tracer(_py: pyo3::Python<'_>, m: &PyModule) -> pyo3::PyResult<()> {
    m.add_class::<CTracer>()?;
    m.add_class::<CFileDisposition>()?;

    Ok(())
}
