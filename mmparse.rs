/** @@docstring
 */
#[link(name="mmparse", vers="0.1", author="Dan Connolly")]

extern mod std; // else --test goes wonky

use core::vec::{any};

/**
$ cargo search rparse
info: central/rparse (0188129D-F459-4EA4-A928-A5BA5632EF2E) [parse, parsing]
   >> General purpose parser combinator library.
https://github.com/jesse99/rparse
*/
extern mod rparse;
use rparse::{Parser, ParseFailed, Combinators, match1, ret};

mod preprocessing {
    pub type Ranges = &[(char, char)];
    pure fn any_range(ch: char, ranges: Ranges) -> bool {
        any(ranges,
            |r| { match r { &(lo, hi) => ch >= lo && ch <= hi } })
    }

    pub const ascii_printable: Ranges = &[('\u0021', '\u007f')];
    pub const ascii_printable_but_dollar: Ranges =
        &[('\u0021', '\u0023'), ('\u0025', '\u007f')];
    pub const ascii_printable_but_cparen: Ranges =
        &[('\u0021', '\u0028'), ('\u002A', '\u007f')];
    pub const ws_char: &str = &" \t\r\n\x0A";
    pub const label_extra: &str = &"'-_.";

    pub fn label() -> Parser<@~str> {
        match1(|ch| {  // why isn't match1 pure?
            any_range(ch, [('A', 'Z'), ('a', 'z'), ('0', '9')])
                || label_extra.contains_char(ch)
        })
    }

    pub fn math_symbol() -> Parser<@~str> {
        match1(|ch| { any_range(ch, ascii_printable_but_dollar) }) }
}
               
#[cfg(test)]
// 4.1.1 Preliminaries
mod test_preliminaries {
    use mod preprocessing::*;

    #[test]
    fn simple_label() {
        let actual = label().parse(@~"some_file", @"label1");
        assert *actual.get() == ~"label1";
    }

    #[test]
    fn bad_label() {
        let actual = label().parse(@~"some_file", @"<X>");
        assert actual.is_err();
    }

    fn space() -> Parser<@~str> { ret(@~"") }

    #[test]
    fn simple_symbol() {
        let actual = math_symbol()
            .everything(space())
            .parse(@~"some_file", @"<*>");
        assert *actual.get() == ~"<*>";
    }

    #[test]
    fn bad_symbol() {
        let actual = math_symbol()
            .everything(space())
            .parse(@~"some_file", @"<$>");
        assert actual.is_err();
    }
}

/*
    let p = Preprocessing()

    /** The only characters that are allowed to appear in a Metamath
    source file are the 94 printable characters on standard ascii
    keyboards ...  plus the following non-printable (white space)
    characters: space, tab, car- riage return, line feed, and form
    feed.*@@/
    #[test]
    fn bad_char() {
      val r = ReportingParseRunner(p.comment).run("$( x \u0000 is not allowed $)")
      val s = r.result match {
	case Some(x) => x
	case None => r.parseErrors(0).getEndIndex() //  getEndIndex
      }
      s should equal (6)
    }

    it("""A Metamath database consists of a sequence of three kinds of tokens
       separated by white space.""") {
      val r = ReportingParseRunner(p.statements).run("axiom.1 $a |- x = x $.")
      val parts = r.result match {
	case Some(result) => result
	case None => r.parseErrors
      }
      parts should equal (
	p.StatementParts(None,Some("axiom.1"),"$a",
			 List("|-", "x", "=", "x"),None)
      )
    }
  }
*/
