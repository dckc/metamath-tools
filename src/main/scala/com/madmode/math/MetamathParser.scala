package com.madmode.math

import scala.util.parsing.combinator.{Parsers, RegexParsers}

class MetamathLexer extends RegexParsers {
  override val whiteSpace = " \t\r\n\f".r
  def comment = "\\$\\((?:[^\\$]|\\$[^)])*\\$\\)".r
  def label = "[A-Za-z0-9-_\\.]*".r
  def sym = "[0-9A-Za-z`~!@#%^&*()\\-_=+\\[\\]{};:'\",\\.<>/?\\\\|]*".r
}


class MetamathParser extends MetamathLexer {

}

class TestParser extends MetamathLexer {
  def test_sym(doc: String): Boolean = {
    parseAll(sym, doc) match {
      case Success(_, _) => true
      case _ => false
    }
  }
}

object ExampleApp extends App {
  println("Hello, Metamath Tools.")

  val t1 = new TestParser().test_sym("abc");
  println("sym test: " + t1)
}
    
