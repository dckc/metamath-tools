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
use rparse::{Parser, ParseFailed, match1, Combinators};

mod preprocessing {
    pub fn label() -> Parser<@~str> {
        match1(|ch| {  // why isn't match1 pure?
            any([('A', 'Z'), ('a', 'z'), ('0', '9')],
                |r| { match r { &(lo, hi) => ch >= lo && ch <= hi }
                }) || any(['-', '_', '.'], |c| { ch == *c })
        })
    }
}
               
#[cfg(test)]
// 4.1.1 Preliminaries
mod test_preliminaries {
    #[test]
    fn simple_label() {
        match preprocessing::label().parse(@~"some_file", @"label1") {
          Ok(@x) => assert x == ~"label1",
          Err(_) => fail
        }
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
