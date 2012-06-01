package com.madmode.math

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class TestLexer extends MetamathLexer {
  def test_sym(doc: String): String = {
    parseAll(sym, doc) match {
      case Success(s, _) => s
      case failure : NoSuccess => "FAILURE:" + failure.msg
    }
  }
}


class TestParser extends MetamathParser {
  def test_file(doc: String): List[String] = {
    parseAll(file, doc) match {
      case Success(result, _) => result
      case failure : NoSuccess => List("FAILURE:" + failure.msg)
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

    it("should tokenize a label") {
      (new TestParser().test_file("axiom.1")) should equal (List("axiom.1"))
    }

    it("should tokenize a simple example") {
      (new TestParser().test_file("axiom.1 $a |- x = x $.")) should equal (
List("axiom.1", "$a", "|-", "x", "=", "x", "$.") )
    }

  }
}
