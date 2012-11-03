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

pub enum Keyword {
    _open, _close, _c, _v, _f, _e, _d, _a, _p
}

pub enum Proof {
    FullProof(~[Label]),
    /*CompressedProof(@[Label], ~str)*/
}

pub type Label = ~str;
pub type Symbol = ~str;

enum StDoc = uint;
pub struct AboutSt {
    doc: Option<@~str>,
    file: @~str,
    line: int
}



//TODO: let thunk return a bool to stop
pub fn each_statement(text: &str,
                      thunk: fn(label: &Option<Label>,
                                kw: Keyword,
                                expr: &[Symbol],
                                pf: &Option<Proof>))
    -> Result<(), ~str> {
    enum CharClass { Space, Printable, BadChar }

    enum SepState {
        NeedSpace,
        SeenSpace,
        DollarOpen,
        CommentSpace,
        CommentText,
    }

    enum PfKind {
        Normal,
        /*Compressed*/
    }

    enum StPart {
        StLabelKw(~mut ~str), // line, col?
        StExpr(Option<Label>, Keyword,
               ~mut~[Symbol], ~mut ~str),
        StPf(Option<Label>, Keyword, ~[Symbol],
             PfKind, ~mut~[Label], ~mut ~str),
/*
        StPfDigits(Option<Label>, Keyword, ~[Symbol],
                   ~[Label], ~str),
*/

    }

    let is_label = |ch: char| {
        any(&[('A', 'Z'), ('a', 'z'), ('0', '9')],
            |r| match r { &(lo, hi) => ch >= lo && ch <= hi })
            || "'-_.".contains_char(ch)
    };

    type State = Result<Option<SepState>, ~str>;

    let mksp = |sp: SepState| { @Ok(Some(sp)) };
    let stay = || { @Ok(None) };
    let start_part = || { @StLabelKw(~mut~"") };

    fn build_tok(tok: &mut ~str, ch: char) {
        str::push_char(tok, ch)
    }
    fn build_toklist(toks: &mut ~[~str], tok: &~mut ~str, ch:char) {
        str::push_char(*tok, ch);
        toks.push(copy **tok);
        // there's gotta be a better way
        while(tok.len() > 0) { str::pop_char(*tok); }
    }
    let finish = |tok: &~mut~str| -> Option<~str> {
        if (tok.len() > 0) { Some(tok.to_unique()) } else { None }
    };

    let mut state = mksp(SeenSpace);
    let mut part = start_part();
    let mut prev_ch = '\x00';

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
              debug!("part: %?", part);

              match (part, cclass, prev_ch, ch) {
                (_, BadChar, _, _) => @Err(fmt!("bad char: %c", ch)),
                (_, _, '$', '(') => mksp(DollarOpen),

                // $_
                // ll $_
                (@StLabelKw(*), _, _, '$') => copy state,
                // ${_
                (@StLabelKw(l), _, '$', '{') => {
                  thunk(&finish(&l), _open, &[], &None);
                  part = start_part();
                  mksp(NeedSpace)
                }
                // $}_
                (@StLabelKw(l), _, '$', '}') => {
                  thunk(&finish(&l), _close, &[], &None);
                  part = start_part();
                  mksp(NeedSpace)
                }
                (@StLabelKw(l), _, '$', kch) => {
                  match decode_kw(kch) {
                    Some(k) => {
                      part = @StExpr(finish(&l), k, ~mut~[], ~mut~"");
                      mksp(NeedSpace)
                    }
                    None => @Err(fmt!("Bad keyword: [$%c]", kch))
                  }
                }
                // ll _$a
                (@StLabelKw(l), Space, _, _) => {
                  build_tok(l, prev_ch);
                  mksp(SeenSpace)
                }
                // l_l $a
                (@StLabelKw(l), Printable, _, _) => {
                  match is_label(prev_ch) {
                    true => {
                      build_tok(l, prev_ch);
                      stay()
                    }
                    _ => @Err(fmt!("bad label char [%c]", ch))
                  }

                }

                // ll $a ss _
                (@StExpr(*), Space, '$', _) => {
                  @Err(~"$ followed by space in Expr")
                }
                (@StExpr(_, _, syms, tok), Space, _, _) => {
                  build_toklist(syms, &tok, prev_ch);
                  mksp(SeenSpace)
                }
                // ll $p ss $_
                (@StExpr(*), _, _, '$') => state,
                // ll $p ss $=_
                (@StExpr(l, kw, expr, tok), _, '$', '=') => {
                  assert tok.len() == 0;
                  part = @StPf(copy l, kw, copy *expr,
                               Normal, ~mut~[], ~mut~"");
                  mksp(NeedSpace)
                }
                // ll $a ss $._
                (@StExpr(l, kw, syms, tok), _, '$', '.') => {
                  assert tok.len() == 0;
                  let expr = copy *syms;
                  thunk(&l, kw, expr, &None);
                  part = start_part();
                  mksp(NeedSpace)
                }
                // $* ss_
                // $* s_
                (@StExpr(_, _, _, tok), Printable, _, _) => {
                    build_tok(tok, prev_ch);
                    stay()
                }

                // ll $p ss $= _
                // ll $p ss $= l _
                (@StPf(_, _, _, Normal, labels, tok),
                 Space, _, _) => {
                  build_toklist(labels, &tok, prev_ch);
                  mksp(SeenSpace)
                }
                // ll $p ss $= l $_
                (@StPf(_, _, _, Normal, _, _), _, _, '$') => stay(),
                // .. $$= ll $._
                (@StPf(l, kw, expr, Normal, labels, tok),
                 _, '$', '.') => {
                  assert tok.len() == 0;
                  let steps = copy *labels;
                  thunk(&l, kw, expr,
                        &Some(FullProof(steps)));
                  part = start_part();
                  mksp(NeedSpace)
                }
                // ll $p ss $= l_
                (@StPf(_, _, _, Normal, _, tok), _, _, _) => {
                  match is_label(prev_ch) {
                    true => {
                      build_tok(tok, prev_ch);
                      stay()
                    }
                    _ => @Err(fmt!("bad label char in proof: %c", prev_ch))
                  }
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
        ('a', _a), ('p', _p)].find |ea| {
        match ea {
          (ltr, _) => ltr == kch
        }
    }.map(|nv| match nv { &(_, v) => v})
}


#[cfg(test)]
mod test {
    fn test_statement(txt: @~str,
                      label: Option<Label>,
                      expr_len: uint, sym: &str) {
        let result = (do each_statement(*txt) |l, kw, expr, pf| {
            debug!("each statement: %?", (l, kw, expr, pf));
            assert *l == label;
            /*@@
            match label {
              None => assert l.is_none(),
              Some(expected) => assert expected == l.get()
            }*/
            assert expr.len() == expr_len;
            if (expr_len > 0) {
                assert sym == expr[0];
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
    fn test_pf() {
        test_statement(@~"th1 $p s1 s2 $= QED $.",
                       Some(~"th1"), 2, ~"s1")
    }

    #[test]
    fn require_space_after_kw() {
        do each_statement(~"axiom.1 $a|- x = x $.") |l, kw, expr, pf| {
            debug!("bad statement: %?", (l, kw, expr, pf));
            fail
        };
    }
}
