# Licensed under the Apache License: http://www.apache.org/licenses/LICENSE-2.0
# For details: https://github.com/nedbat/coveragepy/blob/master/NOTICE.txt

"""Tests for coverage.py's code parsing."""

import textwrap

import pytest

from coverage import env
from coverage.misc import NotPython
from coverage.parser import PythonParser

from tests.coveragetest import CoverageTest
from tests.helpers import arcz_to_arcs


class PythonParserTest(CoverageTest):
    """Tests for coverage.py's Python code parsing."""

    run_in_temp_dir = False

    def parse_source(self, text):
        """Parse `text` as source, and return the `PythonParser` used."""
        if env.PY2:
            text = text.decode("ascii")
        text = textwrap.dedent(text)
        parser = PythonParser(text=text, exclude="nocover")
        parser.parse_source()
        return parser

    def test_exit_counts(self):
        parser = self.parse_source("""\
            # check some basic branch counting
            class Foo:
                def foo(self, a):
                    if a:
                        return 5
                    else:
                        return 7

            class Bar:
                pass
            """)
        assert parser.exit_counts() == {
            2:1, 3:1, 4:2, 5:1, 7:1, 9:1, 10:1
            }

    def test_generator_exit_counts(self):
        # https://github.com/nedbat/coveragepy/issues/324
        parser = self.parse_source("""\
            def gen(input):
                for n in inp:
                    yield (i * 2 for i in range(n))

            list(gen([1,2,3]))
            """)
        assert parser.exit_counts() == {
            1:1,    # def -> list
            2:2,    # for -> yield; for -> exit
            3:2,    # yield -> for;  genexp exit
            5:1,    # list -> exit
        }

    def test_try_except(self):
        parser = self.parse_source("""\
            try:
                a = 2
            except ValueError:
                a = 4
            except ZeroDivideError:
                a = 6
            except:
                a = 8
            b = 9
            """)
        assert parser.exit_counts() == {
            1: 1, 2:1, 3:2, 4:1, 5:2, 6:1, 7:1, 8:1, 9:1
            }

    def test_excluded_classes(self):
        parser = self.parse_source("""\
            class Foo:
                def __init__(self):
                    pass

            if len([]):     # nocover
                class Bar:
                    pass
            """)
        assert parser.exit_counts() == {
            1:0, 2:1, 3:1
            }

    def test_missing_branch_to_excluded_code(self):
        parser = self.parse_source("""\
            if fooey:
                a = 2
            else:   # nocover
                a = 4
            b = 5
            """)
        assert parser.exit_counts() == { 1:1, 2:1, 5:1 }
        parser = self.parse_source("""\
            def foo():
                if fooey:
                    a = 3
                else:
                    a = 5
            b = 6
            """)
        assert parser.exit_counts() == { 1:1, 2:2, 3:1, 5:1, 6:1 }
        parser = self.parse_source("""\
            def foo():
                if fooey:
                    a = 3
                else:   # nocover
                    a = 5
            b = 6
            """)
        assert parser.exit_counts() == { 1:1, 2:1, 3:1, 6:1 }

    def test_indentation_error(self):
        msg = (
            "Couldn't parse '<code>' as Python source: "
            "'unindent does not match any outer indentation level' at line 3"
        )
        with pytest.raises(NotPython, match=msg):
            _ = self.parse_source("""\
                0 spaces
                  2
                 1
                """)

    def test_token_error(self):
        msg = "Couldn't parse '<code>' as Python source: 'EOF in multi-line string' at line 1"
        with pytest.raises(NotPython, match=msg):
            _ = self.parse_source("""\
                '''
                """)

    @pytest.mark.xfail(
        env.PYPY3 and env.PYPYVERSION == (7, 3, 0),
        reason="https://bitbucket.org/pypy/pypy/issues/3139",
    )
    def test_decorator_pragmas(self):
        parser = self.parse_source("""\
            # 1

            @foo(3)                     # nocover
            @bar
            def func(x, y=5):
                return 6

            class Foo:      # this is the only statement.
                '''9'''
                @foo                    # nocover
                def __init__(self):
                    '''12'''
                    return 13

                @foo(                   # nocover
                    16,
                    17,
                )
                def meth(self):
                    return 20

            @foo(                       # nocover
                23
            )
            def func(x=25):
                return 26
            """)
        raw_statements = {3, 4, 5, 6, 8, 9, 10, 13, 15, 16, 17, 20, 22, 23, 25, 26}
        if env.PYBEHAVIOR.trace_decorated_def:
            raw_statements.update([11, 19])
        assert parser.raw_statements == raw_statements
        assert parser.statements == {8}

    def test_class_decorator_pragmas(self):
        parser = self.parse_source("""\
            class Foo(object):
                def __init__(self):
                    self.x = 3

            @foo                        # nocover
            class Bar(object):
                def __init__(self):
                    self.x = 8
            """)
        assert parser.raw_statements == {1, 2, 3, 5, 6, 7, 8}
        assert parser.statements == {1, 2, 3}

    def test_empty_decorated_function(self):
        parser = self.parse_source("""\
            def decorator(func):
                return func

            @decorator
            def foo(self):
                '''Docstring'''

            @decorator
            def bar(self):
                pass
            """)

        if env.PYBEHAVIOR.trace_decorated_def:
            expected_statements = {1, 2, 4, 5, 8, 9, 10}
            expected_arcs = set(arcz_to_arcs(".1 14 45 58 89 9.  .2 2.  -8A A-8"))
            expected_exits = {1: 1, 2: 1, 4: 1, 5: 1, 8: 1, 9: 1, 10: 1}
        else:
            expected_statements = {1, 2, 4, 8, 10}
            expected_arcs = set(arcz_to_arcs(".1 14 48 8.  .2 2.  -8A A-8"))
            expected_exits = {1: 1, 2: 1, 4: 1, 8: 1, 10: 1}

        if env.PYBEHAVIOR.docstring_only_function:
            # 3.7 changed how functions with only docstrings are numbered.
            expected_arcs.update(set(arcz_to_arcs("-46 6-4")))
            expected_exits.update({6: 1})

        assert expected_statements == parser.statements
        assert expected_arcs == parser.arcs()
        assert expected_exits == parser.exit_counts()


class ParserMissingArcDescriptionTest(CoverageTest):
    """Tests for PythonParser.missing_arc_description."""

    run_in_temp_dir = False

    def parse_text(self, source):
        """Parse Python source, and return the parser object."""
        parser = PythonParser(text=textwrap.dedent(source))
        parser.parse_source()
        return parser

    def test_missing_arc_description(self):
        # This code is never run, so the actual values don't matter.
        parser = self.parse_text(u"""\
            if x:
                print(2)
            print(3)

            def func5():
                for x in range(6):
                    if x == 7:
                        break

            def func10():
                while something(11):
                    thing(12)
                more_stuff(13)
            """)
        expected = "line 1 didn't jump to line 2, because the condition on line 1 was never true"
        assert expected == parser.missing_arc_description(1, 2)
        expected = "line 1 didn't jump to line 3, because the condition on line 1 was never false"
        assert expected == parser.missing_arc_description(1, 3)
        expected = (
            "line 6 didn't return from function 'func5', " +
            "because the loop on line 6 didn't complete"
        )
        assert expected == parser.missing_arc_description(6, -5)
        expected = "line 6 didn't jump to line 7, because the loop on line 6 never started"
        assert expected == parser.missing_arc_description(6, 7)
        expected = "line 11 didn't jump to line 12, because the condition on line 11 was never true"
        assert expected == parser.missing_arc_description(11, 12)
        expected = (
            "line 11 didn't jump to line 13, " +
            "because the condition on line 11 was never false"
        )
        assert expected == parser.missing_arc_description(11, 13)

    def test_missing_arc_descriptions_for_small_callables(self):
        parser = self.parse_text(u"""\
            callables = [
                lambda: 2,
                (x for x in range(3)),
                {x:1 for x in range(4)},
                {x for x in range(5)},
            ]
            x = 7
            """)
        expected = "line 2 didn't finish the lambda on line 2"
        assert expected == parser.missing_arc_description(2, -2)
        expected = "line 3 didn't finish the generator expression on line 3"
        assert expected == parser.missing_arc_description(3, -3)
        expected = "line 4 didn't finish the dictionary comprehension on line 4"
        assert expected == parser.missing_arc_description(4, -4)
        expected = "line 5 didn't finish the set comprehension on line 5"
        assert expected == parser.missing_arc_description(5, -5)

    def test_missing_arc_descriptions_for_exceptions(self):
        parser = self.parse_text(u"""\
            try:
                pass
            except ZeroDivideError:
                print("whoops")
            except ValueError:
                print("yikes")
            """)
        expected = (
            "line 3 didn't jump to line 4, " +
            "because the exception caught by line 3 didn't happen"
        )
        assert expected == parser.missing_arc_description(3, 4)
        expected = (
            "line 5 didn't jump to line 6, " +
            "because the exception caught by line 5 didn't happen"
        )
        assert expected == parser.missing_arc_description(5, 6)

    def test_missing_arc_descriptions_for_finally(self):
        parser = self.parse_text(u"""\
            def function():
                for i in range(2):
                    try:
                        if something(4):
                            break
                        elif something(6):
                            x = 7
                        else:
                            if something(9):
                                continue
                            else:
                                continue
                        if also_this(13):
                            return 14
                        else:
                            raise Exception(16)
                    finally:
                        this_thing(18)
                that_thing(19)
            """)
        if env.PYBEHAVIOR.finally_jumps_back:
            expected = "line 18 didn't jump to line 5, because the break on line 5 wasn't executed"
            assert expected == parser.missing_arc_description(18, 5)
            expected = "line 5 didn't jump to line 19, because the break on line 5 wasn't executed"
            assert expected == parser.missing_arc_description(5, 19)
            expected = (
                "line 18 didn't jump to line 10, " +
                "because the continue on line 10 wasn't executed"
            )
            assert expected == parser.missing_arc_description(18, 10)
            expected = (
                "line 10 didn't jump to line 2, " +
                "because the continue on line 10 wasn't executed"
            )
            assert expected == parser.missing_arc_description(10, 2)
            expected = (
                "line 18 didn't jump to line 14, " +
                "because the return on line 14 wasn't executed"
            )
            assert expected == parser.missing_arc_description(18, 14)
            expected = (
                "line 14 didn't return from function 'function', " +
                "because the return on line 14 wasn't executed"
            )
            assert expected == parser.missing_arc_description(14, -1)
            expected = (
                "line 18 didn't except from function 'function', " +
                "because the raise on line 16 wasn't executed"
            )
            assert expected == parser.missing_arc_description(18, -1)
        else:
            expected = (
                "line 18 didn't jump to line 19, " +
                "because the break on line 5 wasn't executed"
            )
            assert expected == parser.missing_arc_description(18, 19)
            expected = (
                "line 18 didn't jump to line 2, " +
                    "because the continue on line 10 wasn't executed" +
                " or " +
                    "the continue on line 12 wasn't executed"
            )
            assert expected == parser.missing_arc_description(18, 2)
            expected = (
                "line 18 didn't except from function 'function', " +
                    "because the raise on line 16 wasn't executed" +
                " or " +
                "line 18 didn't return from function 'function', " +
                    "because the return on line 14 wasn't executed"
            )
            assert expected == parser.missing_arc_description(18, -1)

    def test_missing_arc_descriptions_bug460(self):
        parser = self.parse_text(u"""\
            x = 1
            d = {
                3: lambda: [],
                4: lambda: [],
            }
            x = 6
            """)
        assert parser.missing_arc_description(2, -3) == "line 3 didn't finish the lambda on line 3"


class ParserFileTest(CoverageTest):
    """Tests for coverage.py's code parsing from files."""

    def parse_file(self, filename):
        """Parse `text` as source, and return the `PythonParser` used."""
        parser = PythonParser(filename=filename, exclude="nocover")
        parser.parse_source()
        return parser

    def test_line_endings(self):
        text = """\
            # check some basic branch counting
            class Foo:
                def foo(self, a):
                    if a:
                        return 5
                    else:
                        return 7

            class Bar:
                pass
            """
        counts = { 2:1, 3:1, 4:2, 5:1, 7:1, 9:1, 10:1 }
        name_endings = (("unix", "\n"), ("dos", "\r\n"), ("mac", "\r"))
        for fname, newline in name_endings:
            fname = fname + ".py"
            self.make_file(fname, text, newline=newline)
            parser = self.parse_file(fname)
            assert parser.exit_counts() == counts, "Wrong for %r" % fname

    def test_encoding(self):
        self.make_file("encoded.py", """\
            coverage = "\xe7\xf6v\xear\xe3g\xe9"
            """)
        parser = self.parse_file("encoded.py")
        assert parser.exit_counts() == {1: 1}

    def test_missing_line_ending(self):
        # Test that the set of statements is the same even if a final
        # multi-line statement has no final newline.
        # https://github.com/nedbat/coveragepy/issues/293

        self.make_file("normal.py", """\
            out, err = subprocess.Popen(
                [sys.executable, '-c', 'pass'],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE).communicate()
            """)

        parser = self.parse_file("normal.py")
        assert parser.statements == {1}

        self.make_file("abrupt.py", """\
            out, err = subprocess.Popen(
                [sys.executable, '-c', 'pass'],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE).communicate()""")   # no final newline.

        # Double-check that some test helper wasn't being helpful.
        with open("abrupt.py") as f:
            assert f.read()[-1] == ")"

        parser = self.parse_file("abrupt.py")
        assert parser.statements == {1}
