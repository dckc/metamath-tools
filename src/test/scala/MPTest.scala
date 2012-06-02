package com.madmode.math

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class TestPreliminaries extends Spec with ShouldMatchers {
  describe("4.1.1 Preliminaries") {
    val p = new Preprocessing()

    it("""The only characters that are allowed to appear in a Metamath
source file are the 94 printable characters on standard ascii keyboards ...
plus the following non-printable (white space) characters: space, tab, car-
riage return, line feed, and form feed.""") {

      val s = p.parseAll(p.label, "$( \u0000 is not allowed $) label") match {
	case p.Success(txt, _) => txt
	/* TODO: better diagnostic for bad character. */
	case p.NoSuccess(failure, _) => "BAD CHAR"
      }
      s should equal ("BAD CHAR")
    }

    it("""A Metamath database consists of a sequence of three kinds of tokens
       separated by white space.""") {
      val parts = p.parseAll(p.tokens, "axiom.1 $a |- x = x $.") match {
	case p.Success(result, _) => result
	case p.NoSuccess(failure, _) => failure
      }
      parts should equal (List("axiom.1", "$a", "|-", "x", "=", "x", "$."))
    }

    it("""The set of keyword tokens is ${, $}, $c, $v, $f, $e, $d, $a,
       $p, $., $=, $(, $), $[, and $]. The last four are called
       auxiliary or preprocessing keywords. """) {
      val parts = p.parseAll(p.tokens,
			      "${ $} $c $v $f $e $d $a $p $. $=") match {
	case p.Success(result, _) => result
	case p.NoSuccess(failure, _) => failure
      }
      parts should equal (List("${", "$}", "$c", "$v", "$f", "$e",
			       "$d", "$a", "$p", "$.", "$="))
    }



    it("""A label token consists of any combination of
letters, digits, and the characters hyphen, underscore, and period.""") {
      val l = p.parseAll(p.label, "letters09-_.") match {
	case p.Success(txt, _) => txt
	case p.NoSuccess(failure, _) => failure
      }
      l should equal ("letters09-_.")

      val notl = p.parseAll(p.label, "letter$") match {
	case p.Success(txt, _) => txt
	case p.NoSuccess(failure, _) => "NOT LABEL"
      }
      notl should equal ("NOT LABEL")
    }

    it("""A math symbol token may consist of any combination of the 93
          printable standard ascii characters other than $ .""") {
      val sym = p.parseAll(p.math_symbol, "|-") match {
	case p.Success(txt, _) => txt
	case p.NoSuccess(failure, _) => failure
      }
      sym should equal ("|-")
    }

    it("""All tokens are case-sensitive.""") {
      val sym = p.parseAll(p.math_symbol, "case-SENSITIVE") match {
	case p.Success(txt, _) => txt
	case p.NoSuccess(failure, _) => failure
      }
      sym should equal ("case-SENSITIVE")
    }

  }
}

class TestParser extends BasicSyntax {
  def test_file(doc: String): Either[String, (Context, Database)] = {
    parseAll(database, doc) match {
      case Success(result, _) => Right(result)
      case failure : NoSuccess => Left(failure.msg)
    }
  }
}


class ParserTest extends Spec with ShouldMatchers {
  describe("MetaMath lexer") {
/*@@@@
    it("should lex abc as a sym") {
      new TestLexer().test_sym("abc") should equal ("abc");
    }

    it("should not allow spaces in symbols") {
      (new TestLexer().test_sym("ab c") contains "FAILURE") should equal (true);
    }
*/

    it("should parse a simple example") {
      val bs = new BasicSyntax()
      val ctx1 = Context(List("|-", "="),
			 List("x"), Map(), Map())
      bs.ctx = ctx1
      val s = bs.parseAll(bs.statement, "axiom.1 $a |- x = x $.") match {
	case bs.Success(Database(List(s)), _) => s
	case other => other
      }
      s should equal (
	Statement("axiom.1",
			     Axiom(Con("|-"),
				   List(Var("x"), Con("="), Var("x")))) )
    }

    it("should parse a larger example") {
      val bs = new BasicSyntax()
      val stmts = (bs.parseAll(bs.database, """
$c wff set |- ( ) -> $.
$v P Q x $.
stmt1 $f wff P $.
stmt2 $f set x $.
stmt3 $e |- ( P -> Q ) $.
"""
			     )) match {
	case bs.Success((ctx, Database(stmts)), _) => stmts
	case other => other
      }
      stmts should equal (
	List(Statement("stmt1", VariableType(Con("wff"), Var("P"))),
	     Statement("stmt2", VariableType(Con("set"), Var("x"))),
	     Statement("stmt3", Logical(Con("|-"),
					List(Con("("), Var("P"),
					     Con("->"), Var("Q"),
					     Con(")"))))
	   ) )
    }

  }
}
