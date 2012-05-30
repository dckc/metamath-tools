/**
 * ref:
 * Metamath
 * A Computer Language for Pure Mathematics
 * Norman Megill
 * http://us.metamath.org/index.html#book
 * ISBN: 978-1-4116-3724-5
 * esp. section 4.1 Specification of the Metamath Language
 */
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

object ExampleApp extends App {
  println("Hello, Metamath Tools.")
}
    
