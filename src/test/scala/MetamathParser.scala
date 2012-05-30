package com.madmode.math

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class TestParser extends MetamathLexer {
  def test_sym(doc: String): Boolean = {
    parseAll(sym, doc) match {
      case Success(_, _) => true
      case _ => false
    }
  }
}


class ParserTest extends Spec with ShouldMatchers {
  describe("MetaMath lexer") {
    it("should lex abc as a sym") {
      new TestParser().test_sym("abc");
    }

    it("should not allow spaces in symbols") {
      ! (new TestParser().test_sym("ab c"));
    }
  }
}
