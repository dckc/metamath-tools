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
    CompressedProof(~[Label], ~str)
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

    let ws_char = " \t\r\n\x0A";

    enum SepState {
        NeedSpace,
        SeenSpace,
        DollarOpen,
        CommentSpace,
        CommentText,
    }

    enum PfPart {
        Steps,
        CompressedSteps,
        CompressedDigits
    }

    enum StPart {
        StLabelKw(~mut ~str), // line, col?
        StExpr(Option<Label>, Keyword,
               ~mut~[Symbol], ~mut ~str),
        StPf(Option<Label>, Keyword, ~[Symbol],
             PfPart, ~mut~[Label], ~mut ~str),
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

    let build_tok = |tok: &mut ~str, ch: char, cclass: CharClass,
                     check: fn(c: char) -> bool, err_msg: ~str| -> @State
    {
        if check(ch) {
            str::push_char(tok, ch);
            match cclass {
              Space => mksp(SeenSpace),
              _ => stay()
            }
        } else {
            @Err(fmt!("%s: [%c]", err_msg, ch))
        }
    };

    let build_toklist = |toks: &mut ~[~str], tok: &~mut ~str, prev_ch: char,
                         cclass: CharClass,
                         check: fn(c: char) -> bool, err_msg: ~str| -> @State {
        if check(prev_ch) {
            match cclass {
              Space => {
                str::push_char(*tok, prev_ch);
                toks.push(copy **tok);
                // @@@there's gotta be a better way...
                while(tok.len() > 0) { str::pop_char(*tok); }
                mksp(SeenSpace)
              }
              _ => {
                str::push_char(*tok, prev_ch);
                stay()
              }
            }
        } else {
            @Err(fmt!("%s: [%c]", err_msg, prev_ch))
        }
    };

    let finish = |tok: &~mut~str| -> Option<~str> {
        if (tok.len() > 0) { Some(tok.to_unique()) } else { None }
    };

    let mut state = mksp(SeenSpace);
    let mut part = start_part();
    let mut prev_ch = '\x00';

    do text.all() |ch| {
        let cclass =
            if(ws_char.contains_char(ch)) { Space }
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
                // l_l $a
                // ll_ $a
                (@StLabelKw(l), _, _, _) => {
                  build_tok(l, prev_ch, cclass, is_label, ~"bad label char")
                }

                // ll $p ss $=_
                (@StExpr(l, kw, expr, tok), _, '$', '=') => {
                  assert tok.len() == 0;
                  part = @StPf(copy l, kw, copy *expr,
                               Steps, ~mut~[], ~mut~"");
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
                // $* _s
                // $* s_s
                // $* ss _
                (@StExpr(_, _, syms, tok), _, _, _) => {
                  build_toklist(syms, &tok, prev_ch, cclass,
                                |ch| ch != '$', ~"bad symbol char")
                }

                // ll $p ss $= ( _      for emacs: )
                (@StPf(l, kw, expr, Steps, labels, tok), Space, '(', _) => {
                  assert tok.len() == 0;
                  if (labels.len() == 0) {
                      part = @StPf(copy l, kw, copy expr,
                                   CompressedSteps, copy labels, copy tok);
                      mksp(SeenSpace)
                  } else {
                      @Err(~"misplaced (")
                  }
                }

                // ll $p ss $= ( xx )_
                (@StPf(l, kw, expr, CompressedSteps, labels, tok),
                 Space, ')', _) => {
                  assert tok.len() == 0;
                  part = @StPf(copy l, kw, copy expr,
                               CompressedDigits, copy labels, copy tok);
                  mksp(SeenSpace)
                }

                // .. $$= ll $._
                (@StPf(l, kw, expr, pp, labels, tok),
                 _, '$', '.') => {
                  let steps = copy *labels;
                  match pp {
                    Steps => {
                      assert tok.len() == 0;
                      thunk(&l, kw, expr,
                            &Some(FullProof(steps)));
                    }
                    _ => {
                      let digits = (copy finish(&tok)).get_default(~"");
                      thunk(&l, kw, expr,
                            &Some(CompressedProof(steps, digits)));
                    }
                  }
                  part = start_part();
                  mksp(NeedSpace)
                }

                // ll $p ss $= _l
                // ll $p ss $= l_l
                // ll $p ss $= ll_
                (@StPf(_, _, _, pp, labels, tok), _, _, _) => {
                  match pp {
                    CompressedDigits => {
                      build_tok(tok, prev_ch, cclass,
                                (|ch| ((ch >= 'A' && ch <= 'Z')
                                       || ws_char.contains_char(ch))),
                                ~"bad compressed proof digit")
                    }
                    _ => build_toklist(labels, &tok, prev_ch, cclass,
                                       is_label, ~"bad label char")
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
                      check: fn(l: &Option<Label>,
                                kw: Keyword,
                                expr: &[Symbol],
                                pf: &Option<Proof>)){
        let result = (do each_statement(*txt) |l, kw, expr, pf| {
            debug!("each statement: %?", (l, kw, expr, pf));
            check(l, kw, expr, pf);
        });
        match result {
          Ok(()) => (),
          Err(msg) => fail msg
        }
    }

    fn test_axiom(txt: @~str,
                  label: Label,
                  expr_len: uint, sym: &str) {
        do test_statement(txt) |l, kw, expr, pf| {
            assert *l == Some(copy label);
            match kw { _a => (), _ => fail }
            assert expr.len() == expr_len;
            if (expr_len > 0) {
                assert sym == expr[0];
            }
            assert pf.is_none()
        }
    }


    #[test]
    fn test_1_axiom() {
        test_axiom(@~"axiom.1 $a |- x = x $.",
                   ~"axiom.1", 4, ~"|-")
    }

    #[test]
    fn test_2_axioms() {
        test_axiom(@~"axiom.1 $a |- x = x $. axiom.1 $a |- x = x $.",
                   ~"axiom.1", 4, ~"|-")
    }

    #[test]
    fn test_comment_0_axioms() {
        test_statement(@~"$( hi $)", |_l, _k, _e, _p| ())
    }

    #[test]
    fn test_empty_comment() {
        test_statement(@~"$( $)", |_l, _k, _e, _p| ())
    }

    #[test]
    fn test_comment_1_axioms() {
        test_axiom(@~"$( hi $) axiom.1 $a |- x = x $.",
                   ~"axiom.1", 4, ~"|-")
    }

    #[test]
    fn test_constants() {
        do test_statement(@~"$c 0 1 2 3 $.") |l, kw, expr, pf| {
            match kw { _c => (), _ => fail }
            assert l.is_none();
            assert expr.len() == 4;
            assert pf.is_none()
        }
    }

    #[test]
    fn test_pf() {
        do test_statement(@~"th1 $p s1 s2 $= QED $.") |l, kw, expr, pf| {
            match kw { _p => (), _ => fail }
            assert *l == Some(~"th1");
            assert expr.len() == 2;
            assert expr[0] == ~"s1";
            match *pf {
              Some(FullProof(steps)) => assert steps[0] == ~"QED",
              _ => fail
            }
        }
    }

    #[test]
    fn compressed_pf() {
        do test_statement(@~"th1 $p s1 s2 $= ( a1 a2 ) ABC $.")
            |_l, kw, _expr, pf| {
            match kw { _p => (), _ => fail }
            match *pf {
              Some(CompressedProof(steps, digits)) => {
                assert steps[0] == ~"a1";
                assert digits == ~"ABC";
              }
              _ => fail
            }
        }
    }

    #[test]
    fn require_space_after_kw() {
        do each_statement(~"axiom.1 $a|- x = x $.") |l, kw, expr, pf| {
            debug!("bad statement: %?", (l, kw, expr, pf));
            fail
        };
    }

    #[test]
    fn single_char_bad_label() {
        let result = do each_statement(~"( $a|- x = x $.") |l, kw, expr, pf| {
            debug!("bad statement: %?", (l, kw, expr, pf));
            fail
        };
        match result {
          Ok(()) => fail ~"oops; should not have parsed",
          Err(msg) => debug!("good: %s", msg)
        }
    }
}
