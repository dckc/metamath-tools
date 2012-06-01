package com.madmode.math

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class TestLexer extends Preliminaries {
  def test_sym(doc: String): String = {
    parseAll(sym, doc) match {
      case Success(s, _) => s
      case failure : NoSuccess => "FAILURE:" + failure.msg
    }
  }
}


class TestParser extends BasicSyntax {
  def test_file(doc: String): Either[String, List[Statement]] = {
    parseAll(database, doc) match {
      case Success(result, _) => Right(result)
      case failure : NoSuccess => Left(failure.msg)
    }
  }
}


class ParserTest extends Spec with ShouldMatchers {
  describe("MetaMath lexer") {
    it("should lex abc as a sym") {
      new TestLexer().test_sym("abc") should equal ("abc");
    }

    it("should not allow spaces in symbols") {
      (new TestLexer().test_sym("ab c") contains "FAILURE") should equal (true);
    }

    it("should parse a simple example") {
      (new TestParser().test_file("axiom.1 $a |- x = x $.")) should equal (
	Right(List(Indexed("axiom.1", Axiom("|-", List("x", "=", "x"))))) )
    }

    it("should parse a larger example") {
      (new TestParser().test_file("""
stmt1 $f wff P $.
stmt2 $f set x $.
stmt3 $e |- ( P -> Q ) $.
""")) should equal (
	Right(List(Indexed("stmt1",VariableType("wff","P")),
		   Indexed("stmt2",VariableType("set","x")),
		   Indexed("stmt3",Logical("|-",List("(", "P", "->", "Q", ")")))
		   )) )
    }

  }
}
