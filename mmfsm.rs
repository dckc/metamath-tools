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

struct Proof {
    labels: @[Label],
    digits: Option<~str>
}

pub type Label = @~str;
pub type Symbol = @~str;

enum StDoc = uint;
pub struct AboutSt {
    doc: Option<@~str>,
    file: @~str,
    line: int
}



//@@@@@ suppress these warnings until I figure out what I'm after.
#[allow(non_implicitly_copyable_typarams)]

//TODO: let thunk return a bool to stop
pub fn each_statement(text: &str,
                      thunk: fn(label: &Option<Label>,
                                kw: Keyword,
                                expr: &[Symbol]/*,
                                    pf: Option<Proof>*/))
    -> Result<(), ~str> {
    enum CharClass { Space, Printable, BadChar }
    enum IsLabelChar { NotLabel, IsLabel }

    enum SepState {
        NeedSpace,
        SeenSpace,
        DollarOpen,
        CommentSpace,
        CommentText,
    }

    enum PfKind {
        Normal,
        Compressed
    }

    enum StPart {
        StLabelKw(Option<Label>), // line, col?
        StExpr(Option<Label>, Keyword,
               @[Symbol], Option<@~str>),
/*
        StPf(Option<Label>, Keyword, ~[Symbol],
             PfKind, ~[Label], Option<~str>),
        StPfDigits(Option<Label>, Keyword, ~[Symbol],
                   ~[Label], ~str),
*/

    }
    type State = Result<Option<SepState>, ~str>;

    let mksp = |sp: SepState| { @Ok(Some(sp)) };
    let start_part = || { @StLabelKw(None) };

    let mut state = mksp(SeenSpace);
    let mut part = start_part();
    let mut prev_ch = '\x00';

    let build_tok = |tok: &Option<@~str>, ch|  {
        // TODO: reduce copying
        let sch = @str::from_char(ch);
        Some(tok.map_default(sch, |chars| @(*chars + *sch)))
    };
    let build_toklist = |toks: @[@~str], tok: Option<@~str>| {
        tok.map_default(toks, |stropt| toks + @[*stropt])
    };


    do text.all() |ch| {
        let cclass =
            if(" \t\r\n\x0A".contains_char(ch)) { Space }
            else if (ch > '\u007F' || ch < ' ') { BadChar }
            else { Printable };

        debug!("state: %? cclass: %? ch0: [%?] ch1: [%c]",
               state, cclass, prev_ch, ch);

        state = match state {

          @Ok(Some(sp)) => match (sp, cclass, prev_ch, ch) {
            (_, BadChar, _, _) => @Err(fmt!("bad char: %c", ch)),
            (NeedSpace, Space, _, _) => mksp(SeenSpace),
            (NeedSpace, Printable, _, _) => @Err(~"expected space"),
            (SeenSpace, Space, _, _) => copy state,
            (SeenSpace, Printable, _, _) => @Ok((None)),
            
            (DollarOpen, Space, _, _) => mksp(CommentSpace),
            (DollarOpen, _, _, _) => @Err(~"expected space after $("),
            (CommentSpace, _, '$', ')') => mksp(NeedSpace),
            (CommentSpace, _, _, '$') => copy state,
            (CommentSpace, Space, _, _) => copy state,
            (CommentSpace, Printable, _, _) => mksp(CommentText),
            (CommentText, Space, _, _) => mksp(CommentSpace),
            (CommentText, Printable, _, _) => copy state,
          },

          @Ok(None) => {
              let is_label = (
                  if(any(&[('A', 'Z'), ('a', 'z'), ('0', '9')],
                         |r| match r { &(lo, hi) => ch >= lo && ch <= hi })
                     || "'-_.".contains_char(ch)) { IsLabel } else { NotLabel }
              );
              debug!("part: %? is_label: %?", part, is_label);

              match (part, cclass, is_label, prev_ch, ch) {
                (_, BadChar, _, _, _) => @Err(fmt!("bad char: %c", ch)),
                (_, _, _, '$', '(') => mksp(DollarOpen),

                // $_
                // ll $_
                (@StLabelKw(*), _, _, _, '$') => copy state,
                // ${_
                (@StLabelKw(l), _, _, '$', '{') => {
                  thunk(&l, _open, &[]/*, pf: None*/);
                  part = start_part();
                  mksp(NeedSpace)
                }
                // $}_
                (@StLabelKw(l), _, _, '$', '}') => {
                  thunk(&l, _close, &[]/*, pf: None*/);
                  part = start_part();
                  mksp(NeedSpace)
                }
                (@StLabelKw(l), _, _, '$', kch) => {
                  match decode_kw(kch) {
                    Some(k) => {
                      part = @StExpr(copy l, k, @[], None);
                      mksp(NeedSpace)
                    }
                    None => @Err(fmt!("Bad keyword: [$%c]", kch))
                  }
                }
                // ll _$a
                (@StLabelKw(l), Space, _, _, _) => {
                  part = @StLabelKw(build_tok(&l, prev_ch));
                  mksp(SeenSpace)
                }
                // l_l $a
                (@StLabelKw(l), Printable, IsLabel, _, _) => {
                  part = @StLabelKw(build_tok(&l, prev_ch));
                  @Ok(None)
                }
                // l_$a
                (@StLabelKw(_), _, _, _, _) => {
                  @Err(fmt!("bad label char [%c]", ch))
                }

                // ll $a ss _
                (@StExpr(*), Space, _, '$', _) => {
                  @Err(~"$ followed by space in Expr")
                }
                (@StExpr(l, kw, syms, tok), Space, _, _, _) => {
                  let syms2 = build_toklist(syms, build_tok(&tok, prev_ch));
                  part = @StExpr(copy l, kw, syms2, None);
                  mksp(SeenSpace)
                }
                // ll $p ss $_
                (@StExpr(*), _, _, _, '$') => state,
                /*
                // $=_
                (StExpr(l, kw, expr, tok), _, _, Dollar, '=') => {
                assert tok.is_none();
                @Sep(NeedSpace, @StPf(l, kw, expr, Normal, ~[], None))
                }
                */
                // ll $a ss $._
                (@StExpr(l, kw, expr, tok), _, _, '$', '.') => {
                  assert tok.is_none(); // we can only get here after space
                  thunk(&l, kw, expr/*, pf: None*/);
                  part = start_part();
                  mksp(NeedSpace)
                }
                // $* ss_
                // $* s_
                (@StExpr(l, kw, syms, tok), Printable, _, _, _) => {
                  part = @StExpr(l, kw, syms, build_tok(&tok, prev_ch));
                  @Ok(None)
                }
              }
          }

          @Err(_) => fail ~"Loop should not continue in Err state.",
        };

        prev_ch = ch;

        !state.is_err()
    };

    match (state, part) {
      (@Ok(Some(_)), @StLabelKw(_)) => Ok(()),
      // TODO: count lines, show current line
      _ => Err(fmt!("parse failed in %?", state))
    }
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


#[cfg(test)]
mod test {
    fn test_statement(txt: @~str,
                      label: Option<~str>,
                      expr_len: uint, sym: &str) {
        let result = (do each_statement(*txt) |l, kw, expr| {
            debug!("each statement: %? %? %?", l, kw, expr);
            match label {
              None => assert l.is_none(),
              Some(expected) => assert expected == *l.get()
            }
            assert expr.len() == expr_len;
            if (expr_len > 0) {
                assert sym == *expr[0];
            }
        });
        match result {
          Ok(()) => (),
          Err(msg) => fail msg
        }
    }


    #[test]
    fn test_1_axiom() {
        test_statement(@~"axiom.1 $a |- x = x $.",
                       Some(~"axiom.1"), 4, ~"|-")
    }

    #[test]
    fn test_2_axioms() {
        test_statement(@~"axiom.1 $a |- x = x $. axiom.1 $a |- x = x $.",
                       Some(~"axiom.1"), 4, ~"|-")
    }

    #[test]
    fn test_comment_0_axioms() {
        test_statement(@~"$( hi $)",
                       None, 0, ~"")
    }

    #[test]
    fn test_empty_comment() {
        test_statement(@~"$( $)",
                       None, 0, ~"")
    }

    #[test]
    fn test_comment_1_axioms() {
        test_statement(@~"$( hi $) axiom.1 $a |- x = x $.",
                       Some(~"axiom.1"), 4, ~"|-")
    }

    #[test]
    fn test_constants() {
        test_statement(@~"$c 0 1 2 3 $.",
                       None, 4, ~"0")
    }

    #[test]
    fn require_space_after_kw() {
        do each_statement(~"axiom.1 $a|- x = x $.") |l, kw, expr| {
            debug!("bad statement: %?", (l, kw, expr));
            fail
        };
    }
}
