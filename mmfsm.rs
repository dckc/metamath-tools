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

enum Label = rope::Rope;
enum Symbol = rope::Rope;

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
fn each_statement(text: rope::Rope, thunk: fn&(&Statement)) -> bool {
    enum CharClass { Space, Printable, BadChar }

    enum State {
        Start,
        StLabel,
        ISpace,
        StKeyword,
        StExpr,
        TSpace,

        BadSyntax, // line, col?
    }

    let mut state = Start;
    let mut after_space = Start;
    let mut ch1 = '\u0000'; // LL(2) look-behind
    let mut keyword = _c;
    let mut label: Option<Label> = None;
    let mut expr: ~[Symbol] = ~[];
    let mut pos = 0;
    let mut tok_start: Option<uint> = None;

    do rope::loop_chars(text) |ch| {
        let cclass =
            if(" \t\r\n\x0A".contains_char(ch)) { Space }
            else if (ch > '\u007F' || ch < ' ') { BadChar }
            else { Printable };

        let is_label = any(&[('A', 'Z'), ('a', 'z'), ('0', '9')],
                           |r| match r { &(lo, hi) => ch >= lo && ch <= hi })
            || "'-_.".contains_char(ch);


        debug!("state: %? cclass: %? is_label: %? ch1: '%c' ch: '%c'",
               state, cclass, is_label, ch1, ch);

        state = match (state, cclass, is_label, ch1, ch) {
          (_, BadChar, _, _, _) => BadSyntax,

          (Start, Space, _, _, _) => Start,
          (Start, Printable, true, _, _) => { tok_start = Some(pos); StLabel },
          (Start, _, _, _, _) => BadSyntax,  // not implemented
          
          (StLabel, Printable, true, _, _) => StLabel,
          (StLabel, Space, _, _, _) => {
            match tok_start {
              Some(where) => label = Some(Label(
                  rope::sub_chars(text, where, pos - where))),
              None => ()
            }
            after_space = StKeyword;
            ISpace
          }
          (StLabel, _, _, _, _) => BadSyntax,
          
          (ISpace, Space, _, _, _) => ISpace,
          (ISpace, Printable, _, _, _) => after_space,

          (StKeyword, Printable, _, '$', 'a') => {
            keyword = _a;
            expr = ~[];
            tok_start = None;
            StExpr
          },
          (StKeyword, _, _, _, _) => BadSyntax,

          (StExpr, _, _, '$', '.') => {
            let statement = match keyword {
              _a => Axiom(label, copy expr),
              _ => fail fmt!("not implemented: %?", keyword)
            };
            thunk(&statement);
            TSpace
          }
          (StExpr, _, _, '$', _)  => BadSyntax,
          (StExpr, Printable, _, _, _) => {
            if (tok_start.is_none()) { tok_start = Some(pos) }
            StExpr
          }
          (StExpr, Space, _, _, _) => {
            match tok_start {
              Some(where) => {
                expr.push(Symbol(rope::sub_chars(text, where, pos - where)));
              }
              _ => () // leading space
            }
            tok_start = None;
            StExpr
          }

          (TSpace, Space, _, _, _) => TSpace,
          (TSpace, Printable, _, _, _) => BadSyntax,  // not implemented

          (BadSyntax, _, _, _, _) => fail fmt!("unexpected state: %?", state)
        };

        ch1 = ch;
        pos += 1;
        match state {
            BadSyntax => false,
            _ => true
        }
    };

    match state {
      TSpace => true,
      _ => false
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn each_statement1() {
        let doc = rope::of_str(@~"axiom.1 $a |- x = x $.");
        let ok = (do each_statement(doc) |st| {
            debug!("each statement: %?", st);
            match *st {
              Axiom(Some(label), expr) => {
                assert rope::eq(*label, rope::of_str(@~"axiom.1"));
                assert expr.len() == 4;
              }
              _ => fail
            }
        });
        match ok {
          true => (),
          false => fail ~"oops."
        }
    }
}
