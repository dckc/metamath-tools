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
use rparse::{Parser, StringParsers, GenericParsers, Combinators,
             ParseFailed,
             ret, match1, match0, scan, or_v,
             seq2, seq2_ret0, seq2_ret_str, seq4 };

mod preliminaries {
    pub fn white_space() -> Parser<@~str> {
        scan(|chars, start| {
            let mut i = start;
            while (ws_char.contains_char(chars[i])) { i += 1 }
            i - start
        }).err("whitspace")
    }
    // TODO: consider the impact of \x0A here but not in .s0() etc.
    const ws_char: &str = &" \t\r\n\x0A";

    // TODO: pub enum Label = @~str; // constrained syntax

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
        "$(".lit()
            .then(seq2_ret0(
                seq2_ret_str(white_space(), scan(to_close)),
                "$)".lit())).s0()
    }
      
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
              Start => if (ws_char.contains_char(ch)) {Space} else {state},
              Space => match ch { '$' => Dollar, _ => Start },
              Dollar => match ch {
                ')' => {
                    debug!("to_close found ) at %u", i);
                    return i - index - 1
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
    const label_extra: &str = &"'-_.";
}
               
#[cfg(test)]
// 4.1.1 Preliminaries
mod test_preliminaries {
    use mod preliminaries::*;

    /** The only characters that are allowed to appear in a Metamath
    source file are the 94 printable characters on standard ascii
    keyboards ...  plus the following non-printable (white space)
    characters: space, tab, car- riage return, line feed, and form
    feed.*/
    #[test]
    fn bad_char() {
        let actual = comment()
            .everything(white_space())
            .parse(@~"f", @"$( x \u0000 is not allowed $)");
        match actual {
	  Ok(_) => fail,
	  Err(pf) => {
            debug!("parse failed at column: %u", pf.col);
            assert pf.col == 4
          }
        }
    }


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
            .everything(white_space())
            .parse(@~"some_file", @" <*> ");
        assert *actual.get() == ~"<*>";
    }

    #[test]
    fn bad_symbol() {
        let actual = math_symbol()
            .everything(white_space())
            .parse(@~"some_file", @"<$>");
        assert actual.is_err();
    }

    #[test]
    fn simple_comment() {
        let actual = comment()
            .everything(white_space())
            .parse(@~"f", @"$( c1 $)");
        match actual {
          Ok(s) => {
            debug!("comment: [%s]", *s);
            assert s == @~" c1 "
          },
          Err(pf) => { fail fmt!("%?", pf) }
        }
    }

    #[test]
    fn simple_comments() {
        let actual = comments()
            .everything(white_space())
            .parse(@~"f", @" $( c1 $)$( c2 $)");
        match actual {
          Ok(s) => {
            debug!("comment: [%s]", *s);
            assert s == @~" c2 "
          },
          Err(pf) => { fail fmt!("%?", pf) }
        }
    }

    #[test]
    fn comments_over_lines() {
        let actual = comments()
            .everything(white_space())
            .parse(@~"comment_lines", @" $( c1 $)
$( c2 $)

$(
 c3
$)

");
        match actual {
          Ok(s) => {
            debug!("comment: [%s]", *s);
            debug!("comment length: [%u]", s.len());
            assert s.len() == 5;
            assert s.contains("c3")
          },
          Err(pf) => { fail fmt!("%?", pf) }
        }
    }

}


pub mod basic_syntax {
    use mod preliminaries::*;

    pub fn statements() -> Parser<@~[@~Statement]> {
        (comments().optional().thene(statement)).list(white_space())
    }

    pub enum Statement {
        BlockStart,
        BlockEnd,
        KeywordStatement(Option<@~str>, Parts)
    }

    fn statement(doc_o: Option<@~str>) -> Parser<@~Statement> {
        scoping_statement()
            .or({
                do keyword_statement().note("kw st.").thene() |parts| {
                    ret(@~KeywordStatement(doc_o, parts))
                }
            }).err("statement")
    }

    pub fn scoping_statement() -> Parser<@~Statement> {
        ("${".litv(@~BlockStart)
        .or("$}".litv(@~BlockEnd))).s0()
    }

    pub struct Parts {
        label: Option<@~str>,
        kw: Keyword,
        expr: @~[@~str],
        pf: Option<@~Proof>
    }

    pub fn keyword_statement() -> Parser<Parts> {
        do seq4(label().optional(),
                keyword(),
                expression(),
                proof().optional()) |l_o, kw, e, pf_o| {
            debug!("kws after seq4");
            Ok(Parts{label: l_o, kw: kw, expr: e, pf: pf_o})
        }
        .thene(|ks| "$.".litv(ks)).s0()
    }

    enum Keyword {
        c, v, f, e, d, a, p
    }

    pub fn keyword() -> Parser<Keyword> {
        let lookup = &[("$c", c), ("$v", v),
                       ("$f", f), ("$e", e),
                       ("$d", d),
                       ("$a", a), ("$p", p)];
        let parsers: @~[Parser<Keyword>] =
            @lookup.map(|p| match p { &(tok, kw) => tok.litv(kw) });
        or_v(parsers).s0()
    }

    pub fn expression() -> Parser<@~[@~str]> {
        math_symbol().list(white_space().or(comment())).s0()
    }


    /** A $p statement consists of ... followed by $=,
    followed by a sequence of labels, followed by $.

    A compressed proof, located between $= and $. keywords, consists
    of a left parenthesis, a sequence of statement labels, a right
    parenthesis, and a sequence of upper-case letters A through Z
    (with optional white space between them).  */
    pub fn proof() -> Parser<@~Proof> {
        debug!("in proof()");

        let plain = do (label().list(white_space().or(comments()))).thene()
            |labels| {
            debug!("plain matched");
            ret(@~Proof{labels: labels, digits: None})
        };

        let digits = seq2_ret_str(
            match1(|ch| any_range(ch, [('A', 'Z')])),
            match0(|ch| in_range_str(ch, [('A', 'Z')], ws_char)));

        let compressed = do seq4("(".s1(),
                                 // TODO: think about 0 labels some more
                                 label().list(white_space()).optional(),
                                 ")".s1(),
                                 digits) |open, labels, close, ddd| { // TODO: how to address unused variables?
            Ok(@~Proof{labels: labels.get_default(@~[]), digits: Some(ddd)})
        };

        "$=".s1() .then(comments().optional()).then(
            compressed.or(plain))
    }

    pub struct Proof {
        labels: @~[@~str],
        digits: Option<@~str>
    }

}


#[cfg(test)]
mod test_basic_syntax {
    use mod basic_syntax::*;

    #[test]
    fn simple_keyword() {
        let actual = keyword()
            .everything(white_space())
            .parse(@~"ex1", @" $a ");
        match actual {
          Ok(kw) => {
            debug!("keyword: [%?]", kw);
            match kw { a => (), _ => fail };
          },
          Err(pf) => { fail fmt!("%?", pf) }
        }
    }

    #[test]
    fn simple_expression() {
        let actual = expression()
            .everything(white_space())
            .parse(@~"ex1", @" |- x = x");
        match actual {
          Ok(expr) => {
            debug!("expression: [%?]", expr);
            assert expr.len() == 4;
            assert *expr[0] == ~"|-";
            ()
          },
          Err(pf) => { fail fmt!("%?", pf) }
        }
    }

    #[test]
    fn expression_before_dot() {
        let actual = expression()
            .parse(@~"ex2", @"|- x = x $.");
        match actual {
          Ok(s) => { debug!("expression: [%?]", s); () },
          Err(pf) => { fail fmt!("%?", pf) }
        }
    }

    #[test]
    fn simple_keyword_statement() {
        let actual = keyword_statement()
            .everything(white_space())
            .parse(@~"ax1", @"axiom.1 $a |- x = x $.");
        match actual {
          Ok(st) => {
            assert st.label == Some(@~"axiom.1");
            match st.kw { a => (), _ => fail };
            assert st.expr.len() == 4;
            debug!("keyword_statement: [%?]", st);
            ()
          },
          Err(pf) => { fail fmt!("%?", pf) }
        }
    }

    #[test]
    fn keyword_statement_with_doc() {
        let actual = statements()
            .everything(white_space())
            .parse(@~"ax1",
                   @" $( blah... $) $( doc... $) axiom.1 $a |- x = x $.");
        match actual {
          Ok(st) => {
            debug!("keyword_statement: [%?]", st);
            assert st.len() >= 1;
            match **st[0] {
              KeywordStatement(doc, _) => assert doc == Some(@~" doc... "),
              _ => fail ~"wrong kind of statement"
            }
          },
          Err(pf) => { fail fmt!("%?", pf) }
        }
    }

    #[test]
    fn test_proof() {
        let actual = statements()
            .everything(white_space())
            .parse(@~"pf1", @"th1 $p |- t = t $= tt tze $.");
        match actual {
          Ok(st) => {
            debug!("p statement: [%?]", st);
            assert st.len() >= 1;
            match **st[0] {
              KeywordStatement(_, Parts{pf: Some(proof), _}) => {
                debug!("how many labels in pf: %u", proof.labels.len());
                assert proof.labels.len() == 2;
              }
              _ => fail ~"wrong kind of statement"
            }
          },
          Err(pf) => { fail fmt!("%?", pf) }
        }

    }


    #[test]
    fn test_proof_zero_labels_compressed() {
        let actual = statements()
            .everything(white_space())
            .parse(@~"pf1", @"dummylink $p |- ph $= ( ) C $.");
        match actual {
          Ok(st) => debug!("pf: %?", st),
          Err(pf) => { fail fmt!("%?", pf) }
        }

    }

}
