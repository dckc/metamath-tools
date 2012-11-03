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
    _open, _close, _c, _v, _f, _e, _d, _a, /*_p*/
}

enum Label = ~str;
enum Symbol = ~str;

struct Proof {
    labels: ~[Label],
    digits: Option<~str>
}

pub struct StatementParts {
    label: Option<Label>,
    kw: Keyword,
    expr: ~[Symbol],
/*    pf: Option<Proof>
*/
}

pub enum Statement {
    BlockStart,
    BlockEnd,
    Constants(~[Symbol]),
    Variables(~[Symbol]),
    Disjoint(~[Symbol]),
    FloatingHyp(Label, Symbol, Symbol),
    Antecedent(Label, ~[Symbol]),
    Axiom(/* StDoc ,*/ Label, ~[Symbol]),
/*
    Theorem(Label, ~[Symbol], ~[Label], Option<~str>)
*/
}

enum StDoc = uint;
pub struct AboutSt {
    doc: Option<@~str>,
    file: @~str,
    line: int
}



//@@@@@ suppress these warnings until I figure out what I'm after.
#[allow(non_implicitly_copyable_typarams)]

//TODO: let thunk return a bool to stop
pub fn each_statement(text: &str, thunk: fn&(&StatementParts))
    -> Result<(), ~str> {
    enum CharClass { Space, Printable, BadChar }

    enum CommentState {
        DollarOpen,
        CommentSpace,
        CommentText,
    }

    enum PfKind {
        Normal,
        Compressed
    }

    enum InitialSpace {
        NeedSpace, SeenSpace
    }

    enum DelimState {
        NoDelim, Dollar
    }

    enum State {
        Sep(InitialSpace, // seen initial space?
            @State // next state (one level push-down automata)
           ),
        Comment(CommentState, @State), // TODO: ~str contents
        StLabel(Option<~str>), // line, col?
        StKeyword(Option<Label>),
        StExpr(Option<Label>, Keyword,
               ~[Symbol], Option<~str>),
/*
        StPf(Option<Label>, Keyword, ~[Symbol],
             PfKind, ~[Label], Option<~str>),
        StPfDigits(Option<Label>, Keyword, ~[Symbol],
                   ~[Label], ~str),
*/

        BadSyntax(~str), // TODO: line, col
    }

    let mut state: @State = @Sep(SeenSpace, @StLabel(None));
    // TODO change back to look-behind for building labels, symbols
    let mut delim = NoDelim;

/*
    let check_block = |label: Option<Label>, block| {
        match label {
          Some(_) => BadSyntax(~"unexpected block after label"),
          None => {
            thunk(block);
            TSpace
          }
        }
    };
*/

    do text.all() |ch| {
        let cclass =
            if(" \t\r\n\x0A".contains_char(ch)) { Space }
            else if (ch > '\u007F' || ch < ' ') { BadChar }
            else { Printable };

        let is_label = any(&[('A', 'Z'), ('a', 'z'), ('0', '9')],
                           |r| match r { &(lo, hi) => ch >= lo && ch <= hi })
            || "'-_.".contains_char(ch);


        debug!("state: %? cclass: %? is_label: %? delim: [%?] ch: [%c]",
               state, cclass, is_label, delim, ch);
        
        state = match (*state, cclass, is_label, delim, ch) {
          (_, BadChar, _, _, _) => @BadSyntax(fmt!("bad char: %c", ch)),

          // _ll $a ...
          (Sep(_, next), Space, _, _, _) => @Sep(SeenSpace, next),
          (Sep(SeenSpace, next), Printable, _, _, '$') => next,
          (Sep(SeenSpace, next), Printable, _, _, _) => {
            match *next {
              StLabel(None) => @StLabel(Some(str::from_char(ch))),
              StExpr(l, kw, syms, None) => {
                @StExpr(l, kw, syms, Some(str::from_char(ch)))
              }
              _ => next
            }
          }
          (Sep(NeedSpace, _), Printable, _, _, _) => {
            @BadSyntax(~"expected space")
          }

          (_, _, _, Dollar, '(') => @Comment(DollarOpen, state),

          // ll _$a
          (StLabel(label_opt), Space, _, _, _) => {
            @Sep(SeenSpace, @StKeyword(label_opt.map(|s| Label(*s))))
          }
          // l_l $a
          (StLabel(label_opt), Printable, true, _, _) => {
            @StLabel(match label_opt {
                Some(chars) => Some(chars + str::from_char(ch)),
                None => Some(str::from_char(ch))
            })
          }
          // _$a
          (StLabel(_), _, _, _, _) => {
            @BadSyntax(fmt!("bad label char [%c]", ch))
          }

          // ${_
          (StKeyword(l), _, _, Dollar, '{') => {
            thunk(&StatementParts{label: l, kw: _open, expr: ~[]/*, pf: None*/});
            @Sep(NeedSpace, @StLabel(None))
          }
          // $}_
          (StKeyword(l), _, _, Dollar, '}') => {
            thunk(&StatementParts{label: l, kw: _close, expr: ~[]/*, pf: None*/});
            @Sep(NeedSpace, @StLabel(None))
          }
          // $a_
          (StKeyword(l), _, _, Dollar, kch) => {
            match decode_kw(kch) {
              Some(k) => {
                @Sep(NeedSpace,
                     @StExpr(l, k, ~[], None))
              }
              None => @BadSyntax(fmt!("Bad keyword: $%c", kch))
            }
          }
          (StKeyword(_), _, _, _, _) => {
            @BadSyntax(fmt!("expected keyword; got $%c", ch))
          }

          // ll $a ss _
          (StExpr(l, kw, syms, tok), Space, _, _, _) => {
            let syms2 = match tok {
              Some(chars) => syms + ~[Symbol(copy chars)],
              _ => syms
            };
            @Sep(SeenSpace, @StExpr(l, kw, syms2, None))
          }
          // ll $p ss $=_
          (StExpr(*), _, _, NoDelim, '$') => state,
          // $* ss_
          // $* s_
          (StExpr(l, kw, syms, tok), Printable, _, NoDelim, _) => {
            @StExpr(l, kw, syms, build_tok(tok, ch))
          }
/*
          // $=_
          (StExpr(l, kw, expr, tok), _, _, Dollar, '=') => {
            assert tok.is_none();
            @Sep(NeedSpace, @StPf(l, kw, expr, Normal, ~[], None))
          }
*/
          // ll $a ss $._
          (StExpr(l, kw, expr, tok), _, _, Dollar, '.') => {
            assert tok.is_none();
            let sp = StatementParts{label: l, kw: kw, expr: expr/*, pf: None*/};
            thunk(&sp);
            @Sep(NeedSpace, @StLabel(None))
          }
          (StExpr(*), _, _, _, _) => {
            @BadSyntax(fmt!("bad character in symbol: $%c", ch))
          }

/*
          (StPf(l, kw, expr, Normal, labels, tok),
           Space, _, _, _) => {
            // factor out build_tokens?
            let labels2 = match tok {
              Some(chars) => labels + ~[Label(copy chars)],
              _ => labels
            };
            @Sep(SeenSpace, @StPf(l, kw, expr, Normal, labels2, None))
          }
          // ll $p ss $= l_
          (StPf(l, kw, syms, Normal, labels, tok),
           Printable, true, NoDelim, _) => {
            @StPf(l, kw, syms, Normal, labels, build_tok(tok, ch))
          }
          // .. $$= ll $._
          (StPf(l, kw, expr, Normal, labels, tok), _, _, Dollar, '.') => {
            assert tok.is_none();
            let sp = StatementParts{
                label: l, kw: kw, expr: expr,
                pf: Some(Proof{labels: labels, digits: None})};
            thunk(&sp);
            @Sep(NeedSpace, @StLabel(None))
          }
*/

          (Comment(DollarOpen, next), Space, _, _, _) => {
            @Comment(CommentSpace, next)
          }
          (Comment(DollarOpen, _), _, _, _, _) => {
            @BadSyntax(~"$( must be followed by space")
          }
          (Comment(CommentSpace, _), _, _, _, '$') => state,
          (Comment(CommentSpace, next), _, _, Dollar, ')') => {
            @Sep(NeedSpace, next)
          }
          (Comment(CommentSpace, next), Printable, _, _, _) => {
            @Comment(CommentText, next)
          }
          (Comment(CommentText, next), Space, _, _, _) => {
            @Comment(CommentSpace, next)
          }
          (Comment(CommentText, _), Printable, _, _, _) => state,

          (BadSyntax(_), _, _, _, _) => fail ~"BadSyntax doesn't get here",

          _ => fail ~"@@not sure why compiler doesn't think this is covered."
        };

        delim = if (ch == '$') { Dollar } else { NoDelim };

        match *state {
          BadSyntax(_) => false,
          _ => true
        }
/*
        match (label, kw, expr.len(), pf_opt) {
          St(None, _c, _, None) => thunk(Constants(move expr)),
          (None, _v, _, None) => Ok(Variables(move expr)),
          (None, _d, _, None) => Ok(Disjoint(move expr)),
          (Some(l), _f, 2, None) => Ok(FloatingHyp(copy l,
                                                copy expr[0], copy expr[1])),
          (Some(l), _e, _, 0) => Ok(Antecedent(move l, move expr)),
          (Some(l), _a, _, 0) => Ok(Axiom(copy l, move expr)),
          (Some(l), _p, _, _) => Ok(Theorem(copy l, move expr,
                                            move pf_labels, move pf_digits)),

          (Some(_), _, _, _) => Err(
              fmt!("unexpected label on %? statement", keyword)),
          (None, _f, _, _) => Err(
              ~"too many tokesn in $f statement"),
          (_, _, 0, _) => Err(
              fmt!("empty expression in %? statement", keyword)),
          (_, _, _, _) => Err(
              fmt!("unexpected proof in %? statement", keyword)),
        };
        match sr {
          Ok(statement) => {
            thunk(&statement);
            TSpace
          }
          Err(msg) => BadSyntax(msg)
        }
    ;
*/
    };

    match *state {
      Sep(_, @StLabel(None)) => Ok(()),
      // TODO: count lines, show current line
      _ => Err(fmt!("parse failed in %?", state))
    }
}

fn decode_kw(kch: char) -> Option<Keyword> {
    do [('c', _c), ('v', _v),
        ('f', _f), ('e', _e),
        ('d', _d),
        ('a', _a)/*, ('p', _p)*/].find |ea| {
        match ea {
          (ltr, _) => ltr == kch
        }
    }.map(|nv| match nv { &(_, v) => v})
}


fn build_tok(tok: Option<~str>, ch: char) -> Option<~str> {
    let sch = str::from_char(ch);
    Some(match tok {
        Some(chars) => chars + sch,
        _ => sch
    })
}


#[cfg(test)]
mod test {
    fn test_axiom(txt: @~str, label: ~str, expr_len: uint, sym: ~str) {
        let result = (do each_statement(*txt) |st| {
            debug!("each statement: %?", st);
            assert *st.label.get() == label;
            assert st.expr.len() == expr_len;
            assert *st.expr[0] == sym;
        });
        match result {
          Ok(()) => (),
          Err(msg) => fail msg
        }
    }


    #[test]
    fn test_1_axiom() {
        test_axiom(@~"axiom.1 $a |- x = x $.", ~"axiom.1", 4, ~"|-")
    }

    #[test]
    fn test_2_axioms() {
        test_axiom(@~"axiom.1 $a |- x = x $. axiom.1 $a |- x = x $.",
                   ~"axiom.1", 4, ~"|-")
    }

    #[test]
    fn test_comment_0_axioms() {
        test_axiom(@~"$( hi $)", ~"", 0, ~"|-")
    }

    #[test]
    fn test_empty_comment() {
        test_axiom(@~"$( $)", ~"", 0, ~"|-")
    }

    #[test]
    fn test_comment_1_axioms() {
        test_axiom(@~"$( hi $) axiom.1 $a |- x = x $.", ~"axiom.1", 4, ~"|-")
    }

    #[test]
    fn require_space_after_kw() {
        do each_statement(~"axiom.1 $a|- x = x $.") |st| {
            debug!("bad statement: %?", st);
            match *st {
              _ => fail
            }
        };
    }
}
