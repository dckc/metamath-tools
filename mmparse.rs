/** @@docstring

http://en.wikipedia.org/wiki/Parser_combinator
 */
#[link(name="mmparse", vers="0.1", author="Dan Connolly")]

extern mod std; // else --test goes wonky

use core::vec::{any};
use std::map::{Map, HashMap};

/**
$ cargo search rparse
info: central/rparse (0188129D-F459-4EA4-A928-A5BA5632EF2E) [parse, parsing]
   >> General purpose parser combinator library.
https://github.com/jesse99/rparse
*/
extern mod rparse;
use rparse::{Parser, StringParsers, GenericParsers, Combinators,
             ParseFailed, State, Succeeded,
             ret, match1, match0, scan, or_v, eot,
             seq2, seq2_ret0, seq2_ret_str, seq3_ret1, seq4, seq5 };

mod preliminaries {
    pub fn white_space() -> Parser<@~str> {
        scan(|chars, start| {
            let mut i = start;
            while (ws_char.contains_char(chars[i])) { i += 1 }
            i - start
        })
    }
    // TODO: consider the impact of \x0A here but not in .s0() etc.
    const ws_char: &str = &" \t\r\n\x0A";

    enum Label = uint;
    impl Label: cmp::Eq {
        pure fn eq(other: &Label) -> bool { *self == **other }
        pure fn ne(other: &Label) -> bool { *self != **other }
    }
    enum Symbol = uint;
    impl Symbol: cmp::Eq {
        pure fn eq(other: &Symbol) -> bool { *self == **other }
        pure fn ne(other: &Symbol) -> bool { *self != **other }
    }

    // TODO: consider refactoring as SymbolParser with label() method etc.
    pub trait SymbolTable {
        fn intern_l(s: @~str) -> Label;
        fn intern_s(s: @~str) -> Symbol;
        fn lookup_l(l: Label) -> Option<@~str>;
        fn lookup_s(s: Symbol) -> Option<@~str>;
    }
    struct SymbolTableI {
        by_name: HashMap<@~str, uint>,
        mut by_index: ~[@~str]
    }
    fn SymbolTable() -> SymbolTable {
        // huh? why isn't HashMap() pure?
        @SymbolTableI{by_name: HashMap(), by_index: ~[]} as SymbolTable
    }

    impl @SymbolTableI: SymbolTable {
        fn intern_l(s: @~str) -> Label { Label(self.intern(s)) }
        fn intern_s(s: @~str) -> Symbol { Symbol(self.intern(s)) }
        fn intern(s: @~str) -> uint {
            match self.by_name.find(s) {
              Some(n) => n,
              None => {
                let n = self.by_index.len();
                self.by_name.insert(s, n);
                self.by_index.push(s);
                n
              }
            }
        }

        fn lookup_l(l: Label) -> Option<@~str> { self.lookup(*l) }
        fn lookup_s(s: Symbol) -> Option<@~str> { self.lookup(*s) }

        fn lookup(n: uint) -> Option<@~str> {
            if (n < self.by_index.len()) { Some(self.by_index[n]) }
            else { None }
        }
    }

    pub fn label(t: SymbolTable) -> Parser<Label> {
        match1(|ch| {
            any_range(ch, @[('A', 'Z'), ('a', 'z'), ('0', '9')])
                || label_extra.contains_char(ch)
        })
            .s0()
            .thene(|txt| ret(t.intern_l(txt)))
    }

    pure fn in_range_str(ch: char, r: &[Range], extra: &str) -> bool {
        any_range(ch, r) || extra.contains_char(ch)
    }

    pub fn math_symbol(t: SymbolTable) -> Parser<Symbol> {
        match1(|ch| { any_range(ch, ascii_printable_but_dollar) })
            .s0()
            .thene(|txt| ret(t.intern_s(txt)))
    }

    pub fn comment() -> Parser<@~str> {
        seq3_ret1("$(".lit(),
                  scan_to_close(ascii_printable, ')'),
                  "$)".s0())
    }

    fn scan_to_close(text_ranges: &static/[Range],
                     delim: char) -> Parser<@~str> {
        let to_close = fn@(input: @[char], offset: uint) -> uint {
            enum S { Start, Space, Text, Delim };
            let mut state = Start;
            let mut i = offset;
            loop {
                let ch = input[i];
                let is_space = ws_char.contains_char(ch);

                if (! (is_space
                       || ch == '$'
                       || ch == delim
                       || any_range(ch, text_ranges)) ) {
                    warn!("bad character: %c (ranges: %?)", ch, text_ranges);
                    return 0;
                }

                state = match (state, is_space, ch) {
                  (Start, true, _) => { Space }
                  (Start, false, _) => {
                    warn!("expected space");
                    return 0
                  }
                  (Space, true, _) => { Space }
                  (Space, _, '$') => { Delim }
                  (Space, _, _) => { Text }
                  (Text, true, _) => { Space }
                  (Text, _, _) => { Text }
                  (Delim, _, d) if d == delim => {
                    debug!("to_close found %c at %u", ch, i);
                    return i - offset - 1
                  }
                  (Delim, _, _) => { Text }
                };
                debug!("to_close: %u: %? %c", i, state, ch);
                i = i + 1;
            }
        };
        scan(to_close)
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

    type Range = (char, char);
    pure fn any_range(ch: char, ranges: &[Range]) -> bool {
        any(ranges,
            |r| { match r { &(lo, hi) => ch >= lo && ch <= hi } })
    }

    const ascii_printable: &[Range] = &[('\u0021', '\u007f')];
    const ascii_printable_but_dollar: &[Range] =
        &[('\u0021', '\u0023'), ('\u0025', '\u007f')];
    const ascii_printable_but_cparen: &[Range] =
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
	  Ok(_) => fail ~"charcter zero not allowed",
	  Err(pf) => {
            debug!("%?", pf);
          }
        }
    }


    #[test]
    fn simple_label() {
        let t = SymbolTable();
        let actual = label(t).parse(@~"some_file", @"label1");
        assert t.lookup_l(actual.get()) == Some(@~"label1");
    }

    #[test]
    fn bad_label() {
        let actual = label(SymbolTable()).parse(@~"some_file", @"<X>");
        assert actual.is_err();
    }

    #[test]
    fn simple_symbol_with_space() {
        let t = SymbolTable();
        let actual = math_symbol(t)
            .everything(white_space())
            .parse(@~"some_file", @" <*> ");
        assert t.lookup_s(actual.get()) == Some(@~"<*>");
    }

    #[test]
    fn bad_symbol() {
        let actual = math_symbol(SymbolTable())
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
    fn empty_comment() {
        let actual = comment()
            .parse(@~"empty_comment", @"$( $)");
        match actual {
          Ok(s) => {
            debug!("comment: [%s]", *s);
            assert s == @~" "
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

/*@@@@@@@@
    pub fn each_statement(thunk: fn&(st: ~Statement)) -> Parser<()> {
        (|input: State| {
            let mut output = input;
            loop {
                match statement()(output) {
                  Ok(pass) => {
                    assert pass.new_state.index > output.index;
                    output = pass.new_state;
                    thunk(copy *pass.value);
                  }
                  Err(_) => {
                    break;
                  }
                }
            }
            Ok(Succeeded{new_state: output, value: ()})
        }).then(comments().optional()).then(eot())
    }
*/

    // TODO: deprecate this. only for testing?
    pub fn statements(stdocs: @mut~[AboutSt], t: @SymbolTable)
        -> Parser<@~[Statement]> {
        seq2_ret0(
            statement(stdocs, t).list(white_space()),
            comments().optional())
    }

    pub enum Statement {
        BlockStart,
        BlockEnd,
        KeywordStatement(Parts)
    }

    fn statement(stdocs: @mut~[AboutSt], t: @SymbolTable)
        -> Parser<Statement> {
        do comments().optional().thene() |doc| {
            scoping_statement()
                .or(keyword_statement(stdocs, t, doc))
        }
    }

    pub fn scoping_statement() -> Parser<Statement> {
        ("${".litv(BlockStart)
        .or("$}".litv(BlockEnd))).s0()
    }

    enum StDoc = uint;
    pub struct AboutSt {
        doc: Option<@~str>,
        file: @~str,
        line: int
    }

    pub struct Parts {
        about: StDoc,
        label: Option<Label>,
        kw: Keyword,
        expr: ~[Symbol],
        pf: Option<Proof>
    }

    pub fn keyword_statement(stdocs: @mut~[AboutSt], t: @SymbolTable,
                             doc: Option<@~str>)
        -> Parser<Statement> {
        |input: State|
        {
            (do seq5(label(t).optional(),
                     keyword(),
                     expression(t),
                     proof(t).optional(),
                     "$.".s0()) |l, kw, e, pf, _dot| {
                let meta = AboutSt{doc: doc,
                                   file: input.file, line: input.line};
                let about = StDoc(stdocs.len());
                stdocs.push(meta);

                let ks = Parts{about: about, label: l, kw: kw,
                               expr: copy *e, pf: pf};

                debug!("%s:%d: [%s] %?",
                       *meta.file, meta.line,
                       *ks.label.map_default(@~"", |l| t.lookup_l(*l).get()),
                       ks.kw);
                Ok(KeywordStatement(ks))
            })(input)
        }
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

    pub fn expression(t: @SymbolTable) -> Parser<@~[Symbol]> {
        math_symbol(t).list(white_space().or(comment())).s0()
    }


    /** A $p statement consists of ... followed by $=,
    followed by a sequence of labels, followed by $.

    A compressed proof, located between $= and $. keywords, consists
    of a left parenthesis, a sequence of statement labels, a right
    parenthesis, and a sequence of upper-case letters A through Z
    (with optional white space between them).  */
    pub fn proof(t: @SymbolTable) -> Parser<Proof> {
        let plain = do (label(t).list(white_space().or(comments()))).thene()
            |labels| {
            ret(Proof{labels: copy *labels, digits: None})
        };

        let digits = scan_to_close(&[('A', 'Z')], '.');

        let compressed = do seq4("(".s1(),
                                 // TODO: think about 0 labels some more
                                 label(t).list(white_space()).optional(),
                                 ")".lit(),
                                 digits) |_open, labels, _close, ddd| {
            Ok(Proof{labels: labels.map_default(~[], |ls| copy **ls),
                     digits: Some(ddd.to_unique())})
        };

        "$=".s1() .then(comments().optional()).then(
            compressed.or(plain))
    }

    pub struct Proof {
        labels: ~[Label],
        digits: Option<~str>
    }

}


#[cfg(test)]
mod test_basic_syntax {
    use mod basic_syntax::*;

    fn check_parse<T: Copy Owned>(file: @~str, txt: @str, prod: Parser<T>,
                                  more_checks: fn&(T)) {
        let actual = prod.parse(file, txt);
        match actual {
          Ok(x) => {
            debug!("%s: [%?]", *file, x);
            more_checks(x)
          },
          Err(pf) => { fail fmt!("%?", pf) }
        }
    }

    #[test]
    fn simple_keyword() {
        let prod = keyword().everything(white_space());
        do check_parse(@~"ex1", @" $a ", prod) |kw| {
            match kw { a => (), _ => fail };
        }
    }

    #[test]
    fn simple_expression() {
        let t = SymbolTable();
        let prod = expression(t).everything(white_space());
        do check_parse(@~"expression1", @" |- x = x", prod) | expr | {
            assert expr.len() == 4;
            assert t.lookup_s(expr[0]) == Some(@~"|-");
        }
    }

    #[test]
    fn expression_before_dot() {
        let t = SymbolTable();
        let prod = expression(t);
        do check_parse(@~"ex_before_dot", @"|- x = x $.", prod) |_s| {
        }
    }

    fn check_statements(file: @~str, txt: @str,
                        more_checks: fn&(sts: @mut~[AboutSt],
                                         t: SymbolTable, ss: @~[Statement])) {
        let sts = @mut~[];
        let t = SymbolTable();
        let prod = statements(sts, t).everything(white_space());
        do check_parse(file, txt, prod) |ss| {
            more_checks(sts, t, ss)
        }
    }

    #[test]
    fn simple_a_statement() {
        do check_statements(@~"ax1", @"axiom.1 $a |- x = x $.")
            |sts, t, ss| {
            assert ss.len() == 1;
            match ss[0] {
              KeywordStatement(st) => {
                assert sts[*st.about].doc.is_none();
                assert st.label == Some(t.intern_l(@~"axiom.1"));
                match st.kw { a => (), _ => fail };
                assert st.expr.len() == 4;
                assert st.expr == [@~"|-", @~"x", @~"=", @~"x"].map(
                    |s| t.intern_s(*s));
              }
              _ => fail ~"wrong kind of statement"
            }
        }
    }

    #[test]
    fn keyword_statement_with_doc() {
        do check_statements(
            @~"kw_w_doc",
            @" $( blah... $) $( doc... $) axiom.1 $a |- x = x $.")
            |sts, _t, ss| {
            assert ss.len() >= 1;
            match ss[0] {
                KeywordStatement(st) => {
                    assert sts[*st.about].doc == Some(@~" doc... ")
                }
                _ => fail ~"wrong kind of statement"
            }
        }
    }

    #[test]
    fn test_proof() {
        do check_statements(@~"pf1",
                            @"th1 $p |- t = t $= tt tze $.")
            |_sts, _t, ss| {
            assert ss.len() >= 1;
            match ss[0] {
              KeywordStatement(Parts{pf: Some(proof), _}) => {
                debug!("how many labels in pf: %u", proof.labels.len());
                assert proof.labels.len() == 2;
              }
              _ => fail ~"wrong kind of statement"
            }
        }
    }


    #[test]
    fn test_proof_zero_labels_compressed() {
        do check_statements(@~"pf1", @"dummylink $p |- ph $= ( ) C $.")
            |_sts, _t, _ss| {   }
    }

    #[test]
    fn test_digits() {
        let digits = scan_to_close(&[('A', 'Z')], '.');
        do check_parse(@~"digits", @" ABC $.", digits) |ds| {
            assert ds == @~" ABC ";
        }
    }


    #[test]
    fn test_proof_line_num() {
        do check_statements(@~"pf_line_num", @"th1 $p |- ph $= ( a b c
d e f ) ABC
DEF $.
axiom.1 $a |- x = x $.
"
                           ) |sts, _t, ss| {
            assert ss.len() >= 2;
            match ss[1] {
              KeywordStatement(st) => assert sts[*st.about].line == 4,
              _ => fail ~"wrong kind of statement"
            }
        }
    }

    #[test]
    fn test_trailing_comments() {
        do check_statements(
            @~"trailing_comments",
            @"axiom.1 $a |- x = x $. $( blah... $) $( doc... $)")
            |_sts, _t, _ss| {
        }
    }

/*
    #[test]
    fn test_each_statment() {
        let count = @mut 0;
        let actual = each_statement( |_st| {
            *count += 1;
        }).parse(@~"each", @"$c 0 $. ax $a 1 = 1 $.");
        match actual {
          Ok(()) => assert *count == 2,
          Err(pf) => fail fmt!("%?", pf)
        }
    }
*/
}
