/** @@docstring

http://en.wikipedia.org/wiki/Parser_combinator
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
use rparse::{Parser, ParseFailed, Combinators, StringParsers, EOT,
             ret, match1, match0, scan };

mod preprocessing {
    pub fn space() -> Parser<@~str> { match0(|ch| ws_char.contains_char(ch)) }

    pub fn label() -> Parser<@~str> {
        match1(|ch| {  // why isn't match1 pure?
            any_range(ch, [('A', 'Z'), ('a', 'z'), ('0', '9')])
                || label_extra.contains_char(ch)
        }).s0()
    }

    pure fn in_range_str(ch: char, r: Ranges, extra: &str) -> bool {
        any_range(ch, r) || extra.contains_char(ch)
    }

    pub fn math_symbol() -> Parser<@~str> {
        match1(|ch| { any_range(ch, ascii_printable_but_dollar) }).s0()
    }

    pub fn comment() -> Parser<@~str> {
        "$( ".lit().then(scan(to_close)).s0()
    }
    //TODO var last_comment: Option[String] = None
      
    pure fn to_close(chars: @[char], index: uint) -> uint {
        enum S { Start, Space, Dollar };
        let mut state = Start;
        let mut i = index;
        loop {
            let ch = chars[i];
            if (! in_range_str(ch, ascii_printable, ws_char)) {
                return 0;
            }
            state = match state {
              Start => match ch {
                ' ' => Space,
                _ => state
              },
              Space => match ch {
                '$' => Dollar,
                _ => Start
              },
              Dollar => match ch {
                ')' => {
                    debug!("to_close found ) at %u", i);
                    return i - index + 1
                },
                _ => Start
              }
            };
            debug!("to_close %c at %u", ch, i);
            i = i + 1;
        }
    }

    /**
    Parse a sequence of one or more comments, and return
    the value of the last one.

    TODO: test whether it consumes space proportional to the
    number of comments, and if so, fix.
     */
    pub fn comments() -> Parser<@~str> {
        do comment().thene |c0| {
            comments()
                .or(ret(c0))
        }
    }

    type Ranges = &[(char, char)];
    pure fn any_range(ch: char, ranges: Ranges) -> bool {
        any(ranges,
            |r| { match r { &(lo, hi) => ch >= lo && ch <= hi } })
    }

    const ascii_printable: Ranges = &[('\u0021', '\u007f')];
    const ascii_printable_but_dollar: Ranges =
        &[('\u0021', '\u0023'), ('\u0025', '\u007f')];
    const ascii_printable_but_cparen: Ranges =
        &[('\u0021', '\u0028'), ('\u002A', '\u007f')];
    const ws_char: &str = &" \t\r\n\x0A";
    const label_extra: &str = &"'-_.";
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

    #[test]
    fn simple_symbol_with_space() {
        let actual = math_symbol()
            .everything(space())
            .parse(@~"some_file", @" <*> ");
        assert *actual.get() == ~"<*>";
    }

    #[test]
    fn bad_symbol() {
        let actual = math_symbol()
            .everything(space())
            .parse(@~"some_file", @"<$>");
        assert actual.is_err();
    }

    #[test]
    fn simple_comment() {
        let actual = comment()
            .everything(space())
            .parse(@~"f", @"$( c1 $)");
        match actual {
          Ok(s) => {
            debug!("comment: [%s]", *s);
            assert s == @~"c1 $)"
          },
          Err(_) => fail ~"parsing comment"
        }
    }

    #[test]
    fn simple_comments() {
        let actual = comments()
            .everything(space())
            .parse(@~"f", @" $( c1 $)$( c2 $)");
        match actual {
          Ok(s) => {
            debug!("comment: [%s]", *s);
            assert s == @~"c2 $)"
          },
          Err(_) => fail ~"parsing comments"
        }
    }

    /** The only characters that are allowed to appear in a Metamath
    source file are the 94 printable characters on standard ascii
    keyboards ...  plus the following non-printable (white space)
    characters: space, tab, car- riage return, line feed, and form
    feed.*/
    #[test]
    fn bad_char() {
        let actual = comment()
            .everything(space())
            .parse(@~"f", @"$( x \u0000 is not allowed $)");
        match actual {
	  Ok(_) => fail,
	  Err(pf) => {
            debug!("parse failed at column: %u", pf.col);
            assert pf.col == 4
          }
        }
    }
}
/*
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
