/**
start = space start
      | statement start
      | comment start
      | EOF

statement = '${' space+ | '$}' tspace | keyword_statement
keyword_statement = label? keyword expr proof? '$.' tspace
label = label_char+ space+
keyword = ($a | $p | ...) space+
expr = symbol*
symbol = symbol_char+ space+

comment = '$(' space (not_$) space)* '$)' tspace
ispace = (space+ comment?)*  # inter-token space

tspace = space+ | EOF

proof = '$=' space+ label+ '$'  # TODO: compressed proof
 */
use core::vec::{any};

extern mod std;
use std::rope;

enum Keyword {
    _c, _v, _f, _e, _d, _a, _p
}

enum Label = ~str;
enum Symbol = ~str;

pub enum Statement {
    BlockStart,
    BlockEnd,
    Axiom(/* StDoc ,*/ Option<Label>, ~[Symbol])
}

enum StDoc = uint;
pub struct AboutSt {
    doc: Option<@~str>,
    file: @~str,
    line: int
}


//TODO: let thunk return a bool to stop
pub fn each_statement(text: &str, thunk: fn&(&Statement))
    -> Result<(), ~str> {
    enum CharClass { Space, Printable, BadChar }

    enum State {
        Sep, // inter-token space
        StLabel,
        StKeyword,
        StExpr,

        CommentStart,
        CommentSpace,
        CommentDollar,
        Comment,
        CommentEnd,

        TSpace, // trailing space. EOF OK.
        BadSyntax, // line, col?
    }

    let mut state = Sep;
    let mut after_space = StLabel;
    let mut ch1 = '\u0000'; // LL(2) look-behind
    let mut keyword = _c;
    let mut label: Option<Label> = None;
    let mut expr: ~[Symbol] = ~[];
    let mut tok: Option<~str> = None;

    do text.all() |ch| {
        let cclass =
            if(" \t\r\n\x0A".contains_char(ch)) { Space }
            else if (ch > '\u007F' || ch < ' ') { BadChar }
            else { Printable };

        let is_label = any(&[('A', 'Z'), ('a', 'z'), ('0', '9')],
                           |r| match r { &(lo, hi) => ch >= lo && ch <= hi })
            || "'-_.".contains_char(ch);


        debug!("state: %? cclass: %? is_label: %? ch1: [%c] ch: [%c]",
               state, cclass, is_label, ch1, ch);

        state = match (state, cclass, is_label, ch1, ch) {
          (_, BadChar, _, _, _) => { debug!("bad char: %c", ch); BadSyntax },

          (Sep, Space, _, _, _) => Sep,
          (Sep, Printable, _, _, _) => { tok = Some(~""); after_space},

          (StLabel, Printable, true, _, _) => StLabel,
          (StLabel, Printable, false, '$', '(') => CommentStart,
          (StLabel, Printable, false, _, '$') => {
            if (tok.map_default(0, |what| what.len()) > 0) {
                debug!("expected space after label");
                BadSyntax
            } else {
                label = None;
                StKeyword
            }
          }
          (StLabel, Printable, false, _, _) => {
            debug!("bad label char: %c", ch);
            BadSyntax
          }
          (StLabel, Space, _, _, _) => {
            label = Some(Label(tok.get())); // non-implicitly-copyable... hmm..
            after_space = StKeyword;
            Sep
          }
          
          (StKeyword, Printable, _, '$', '(') => CommentStart,
          (StKeyword, Printable, _, '$', 'a') => {
            keyword = _a;
            expr = ~[];
            tok = None;
            StExpr
          }
          (StKeyword, _, _, _, _) => {
            debug!("$c etc. not implemented");
            BadSyntax
          }

          (StExpr, _, _, '$', '(') => { after_space = StExpr; CommentStart}
          (StExpr, _, _, '$', '.') => {
            let statement = match keyword {
              _a => Axiom(copy label, copy expr),
              _ => fail fmt!("not implemented: %?", keyword)
            };
            thunk(&statement);
            after_space = StLabel;
            TSpace
          }
          (StExpr, _, _, '$', _)  => {
              debug!("$ not allowed in expression.");
              BadSyntax
          }
          (StExpr, Printable, _, _, _) => {
            if (tok.is_none()) { tok = Some(~"") }
            StExpr
          }
          (StExpr, Space, _, _, _) => {
            match tok {
              Some(what) => {
                expr.push(Symbol(copy what));
              }
              _ => () // leading space
            }
            tok = None;
            after_space = StExpr;
            Sep
          }

          (CommentStart, Space, _, '(', _) => CommentSpace,
          (CommentStart, _, _, _, _) => {
              debug!("space expected after $(");
              BadSyntax
          }

          (CommentSpace, _, _, _, '$') => CommentDollar,
          (CommentSpace, Space, _, _, _) => CommentSpace,
          (CommentSpace, Printable, _, _, _) => Comment,

          (CommentDollar, _, _, '$', ')') => TSpace,
          (CommentDollar, Space, _, _, _) => CommentSpace,
          (CommentDollar, _, _, _, _) => Comment,

          (Comment, Space, _, _, _) => CommentSpace,
          (Comment, _, _, '$', ')') => {
              debug!("space expected before $(");
              BadSyntax
          }
          (Comment, _, _, _, _) => Comment,

          (TSpace, Space, _, _, _) => TSpace,
          (TSpace, _, _, _, _) => { tok = Some(~""); after_space }

          (BadSyntax, _, _, _, _) => fail fmt!("unexpected state: %?", state),
          (_, Space, _, _, _) => fail ~"compiler thinks this isn't covered",
          (_, Printable, _, _, _) => fail ~"compiler thinks this isn't covered"
        };

        ch1 = ch;
        tok = match tok {
          Some(s) => Some(s + str::from_char(ch)),
          None => tok
        };

        match state {
            BadSyntax => false,
            _ => true
        }
    };

    match state {
      TSpace => Ok(()),
      // TODO: count lines, show current line
      _ => Err(fmt!("parse failed after '%c' of %u in %?",
                    ch1, text.len(), state))
    }
}


fn to_str(text: rope::Rope) -> ~str {
    let mut out = ~"";
    do rope::loop_leaves(text) |l| {
        out = str::append(
            copy out, str::substr(*l.content, l.byte_offset, l.byte_len));
        true
    };
    out
}


#[cfg(test)]
mod test {
    fn test_axiom(txt: @~str, label: ~str, expr_len: uint) {
        let result = (do each_statement(*txt) |st| {
            debug!("each statement: %?", st);
            match *st {
              Axiom(Some(actual_label), expr) => {
                assert *actual_label == label;
                assert expr.len() == expr_len;
              }
              _ => fail
            }
        });
        match result {
          Ok(()) => (),
          Err(msg) => fail msg
        }
    }


    #[test]
    fn test_1_axiom() {
        test_axiom(@~"axiom.1 $a |- x = x $.", ~"axiom.1", 4)
    }

    #[test]
    fn test_2_axioms() {
        test_axiom(@~"axiom.1 $a |- x = x $. axiom.1 $a |- x = x $.",
                   ~"axiom.1", 4)
    }

    #[test]
    fn test_comment_0_axioms() {
        test_axiom(@~"$( hi $)", ~"", 0)
    }

    #[test]
    fn test_empty_comment() {
        test_axiom(@~"$( $)", ~"", 0)
    }

    #[test]
    fn test_comment_1_axioms() {
        test_axiom(@~"$( hi $) axiom.1 $a |- x = x $.", ~"axiom.1", 4)
    }
}
