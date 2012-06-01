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

import scala.util.parsing.combinator.{ Parsers, RegexParsers }

class Preliminaries extends RegexParsers {
  /* 4.1.1 Preliminaries */
  val ws_char_e = "[ \t\r\n\f]"
  val comment_e = "\\$\\((?:[^\\$]|\\$[^)])*\\$\\)"

  def label = "[A-Za-z0-9\\-_\\.]+".r
  /** math symbol */
  def sym = "[0-9A-Za-z`~!@#%^&*()\\-_=+\\[\\]{};:'\",\\.<>/?\\\\|]+".r
}

class Preprocessing extends Preliminaries {
  /**
   * "Comments are ignored (treated like white space) for the
   * purpose of parsing."
   */
  override val whiteSpace = ("(?:" + ws_char_e + "|" + comment_e + ")+").r

  /**
   * TODO: implement file inclusion.
   */
  def file_inclusion_command = "$[" ~ filename ~ "$]"
  def filename = sym
}

class BasicSyntax extends Preprocessing {
  def database = block
  def block: Parser[List[Statement]] = (
    statement ~ block ^^ { case s ~ b =>
      s :: b }
    | ("${" ~> block <~ "$}") ~ block ^^ { case b1 ~ b2 =>
      b1 ++ b2 }
    | success(List())
  )

  def statement: Parser[Statement] = (
    declare_variables | declare_constants | hypothesis
    | disjoint_variable_restriction | assertion )

  /* TODO: The same math symbol may not occur twice
   * in a given $v or $c statement. */
  /* TODO: A constant must be declared in the
   * outermost block and may not be declared a second time. */

  def declare_variables = "$v" ~> sym .* <~ "$." ^^ { case syms =>
    VariableDecl(syms) }
  def declare_constants = "$c" ~> sym .* <~ "$." ^^ { case syms =>
    ConstantDecl(syms) }

  def hypothesis = (
    label ~ (("$f" ~> active_constant ~ active_variable) ^^
	     { case k ~ v => VariableType(k, v) }
	     | ("$e" ~> active_constant ~ rep(active_symbol)) ^^
	     { case k ~ expr => Logical(k, expr) }) <~ "$.") ^^ {
    case l ~ expr => Indexed(l, expr)
  }

  def active_symbol = active_constant | active_variable
  def active_constant = sym /* TODO: active */
  def active_variable = sym /* TODO: active */

  def disjoint_variable_restriction = "$d" ~> rep(active_variable) <~ "$." ^^ {
    DisjoinVariables(_) }

  def assertion = (
    label ~ (("$a" ~> active_constant ~ rep(active_symbol)) ^^
	     { case k ~ expr => Axiom(k, expr) }
	     | ("$p" ~> active_constant ~ rep(active_symbol)
		~ "$=" ~ rep(label)) ^^
	     { case k ~ expr ~ "$=" ~ pf =>
	       Theorem(k, expr, pf) }) <~ "$.") ^^ {
    case l ~ expr => Indexed(l, expr)
  }

  /* TODO: A $f, $e, or $d statement is active from ... */
}


sealed abstract class Statement

sealed abstract class Declaration extends Statement
case class VariableDecl(symbols: List[String]) extends Declaration
case class ConstantDecl(symbols: List[String]) extends Declaration
case class DisjoinVariables(symbols: List[String]) extends Declaration

case class Indexed(label: String, expr: Expression) extends Statement

sealed abstract class Expression
sealed abstract class Hypothesis extends Expression
case class VariableType(c: String, t: String) extends Hypothesis
case class Logical(mark: String, symbols: List[String]) extends Hypothesis
case class Axiom(mark: String, symbols: List[String]) extends Expression
/* TODO: inference, i.e. with hypotheses. */
case class Theorem(mark: String, symbols: List[String], proof: List[String]
		 ) extends Expression


object ExampleApp extends App {
  println("Hello, Metamath Tools.")
}

