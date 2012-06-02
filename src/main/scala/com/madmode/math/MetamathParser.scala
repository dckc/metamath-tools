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
  val allowed_range = ascii_printable + ws_char
  val ascii_printable = "\u0021-\u007f"
  val ws_char = " \t\r\n\f"

  def tokens = rep(keyword | label | math_symbol)
  def keyword = ( "${" | "$}" | "$c" | "$v" | "$f" | "$e"
		 | "$d" | "$a" | "$p" | "$." | "$="
		 | "$(" | "$)" | "$[" | "$]" )

  def label: Parser[String] = """[A-Za-z0-9\-_\.]+""".r
  def math_symbol: Parser[String] = ("[" + ascii_printable + """&&[^\$]]+""").r

}

class Preprocessing extends Preliminaries {
  /**
   * "Comments are ignored (treated like white space) for the
   * purpose of parsing."
   */
  override val whiteSpace = (
    "(?:[" + ws_char + "]|" +
    comment_start + "(?:[" + allowed_range + "])*?" + comment_end
    + ")+"
  ).r
  val comment_start = """\$\("""
  val comment_end = """\$\)"""


  /**
   * TODO: implement file inclusion.
   */
  def file_inclusion_command = "$[" ~ filename ~ "$]"
  def filename = ("[" + ascii_printable + """&&[^\$]]+""").r
}

class BasicSyntax extends Preprocessing with CheckedParser {
  /* not functional, but HmmImpl.hs seems to use the state monad... */
  var ctx = Context(List(), List(), Map(), Map())
  def database = statements ^^ { case db => (ctx, db) }

  def statements: Parser[Database] = (
    statement ~ statements ^^ {
      case Database(ss1) ~ Database(ss2) => Database(ss1 ++ ss2) }
    | success(Database(List()))
  )

  /* TODO: use commit */
  def statement: Parser[Database] = (
    declare_constants
    | declare_variables 
    | disjoint_variable_restriction
    | labelled_statement )

  /* TODO: The same math symbol may not occur twice
   * in a given $v or $c statement. */
  /* TODO: A constant must be declared in the
   * outermost block and may not be declared a second time. */

  def d0 = Database(List())
  def d1(s: Statement) = Database(List(s))

  def declare_constants = "$c" ~> math_symbol .* <~ "$." ^^ { case syms =>
    /* TODO: if syms intersects ctx.constants or ctx.variables, FAIL. */
    ctx = Context(ctx.constants ++ syms,
		  ctx.variables, ctx.hypotheses, ctx.statements)
    d0
  }
  def declare_variables = "$v" ~> math_symbol .* <~ "$." ^^ { case syms =>
    ctx = Context(ctx.constants, ctx.variables ++ syms,
		  ctx.hypotheses, ctx.statements)
    d0
  }

  def disjoint_variable_restriction: Parser[Database] = (
    "$d" ~> rep(active_variable) <~ "$." ^^ {
      /* TODO: DVRs */
      case _ => d0
    }
  )

  def labelled_statement: Parser[Database] = label ~ expr <~ "$." ^^ {
    case l ~ Right(e) => d1(Statement(l, e))
    case oops => {
      println("" + oops)
      /* TODO: report error nicely. */
      d0
    }
  }

  /* TODO: consider using Either for errors. */
  def expr: Parser[Either[BadName, Expression]] = (
    variable_type_hypothesis
    | logical_hypothesis
    | axiom
    | theorem
  )

  def variable_type_hypothesis: Parser[Either[BadName, Expression]] = (
    "$f" ~> active_constant ~ active_variable ^^ {
      case Right(k) ~ Right(v) => Right(VariableType(k, v))
      case Left(badsym) ~ _ => Left(badsym)
      case _ ~ Left(badsym) => Left(badsym)
    }
  )

  def logical_hypothesis: Parser[Either[BadName, Expression]] = (
    "$e" ~> active_constant ~ active_symbols ^^ {
      case Right(k) ~ Right(expr) => Right(Logical(k, expr))
      case Left(badsym) ~ _ => Left(badsym)
      case _ ~ Left(badsym) => Left(badsym)
    }
  )

  def fold_either[L, R](ss: List[Either[L, R]]
		 ): Either[L, List[R]] = ss match {
    case List() => Right(List())
    case Left(oops) :: _ => Left(oops)
    case Right(x) :: xss => fold_either(xss) match {
      case Right(xs) => Right(x :: xs)
      case Left(oops) => Left(oops)
    }
  }

  def active_symbols = rep(active_symbol) ^^ fold_either
  def active_symbol: Parser[Either[BadSymbol, Symbol]] =  math_symbol ^^ {
    case s if ctx.constants.contains(s) => Right(Con(s))
    case s if ctx.variables.contains(s) => Right(Var(s))
    case oops => Left(BadSymbol(oops))
  }
  def active_constant = math_symbol ^^ {
    case s if ctx.constants.contains(s) => Right(Con(s))
    case oops => Left(BadSymbol(oops))
  }
  def active_variable = math_symbol ^^ {
    case s if ctx.variables.contains(s) => Right(Var(s))
    case oops => Left(BadSymbol(oops))
  }


  def axiom: Parser[Either[BadName, Expression]] = (
    "$a" ~> active_constant ~ active_symbols ^^ {
      case Right(k) ~ Right(expr) => Right(Axiom(k, expr))
      case Left(badsym) ~ _ => Left(badsym)
      case _ ~ Left(badsym) => Left(badsym)
    }
    /* TODO: hypotheses from ctx */
  )

  def theorem: Parser[Either[BadName, Expression]] = (
    "$p" ~> active_constant ~ active_symbols ~ ("$=" ~> rep(label)) ^^ {
      case Right(k) ~ Right(expr) ~ labels => {
	fold_either(
	  for (l <- labels) yield (ctx.statements get l) match {
	    case Some(s) => Right(s)
	    case _ => Left(BadLabel(l))
	  }
	) match {
	  case Right(pf) => Right(Theorem(k, expr, pf))
	  case Left(oops) => Left(oops)
	}
      }
      case Left(badsym) ~ _ ~ _ => Left(badsym)
      case _ ~ Left(badsym) ~ _ => Left(badsym)
    }
  )

/*
  def block: Parser[List[Statement]] = (
    statement ~ block ^^ { case s ~ b =>
      s :: b }
    | ("${" ~> block <~ "$}") ~ block ^^ { case b1 ~ b2 =>
      b1 ++ b2 }
    | success(List())
  )
*/

  /* TODO: A $f, $e, or $d statement is active from ... */
}

trait CheckedParser extends Parsers {
  /**
   * checked wraps a Parser[T] with a check on its results
   */
  def checked[T](p: => Parser[T])(
    check: (T, Input) => ParseResult[T]): Parser[T] = Parser {
      in => p(in) match {
	case s @ Success(x, in) => check(x, in)
	case ns => ns
      }
    }
}


/* ack: HmmImpl.hs */
case class Database(statements: List[Statement])

case class Context(constants: List[String],
		   variables: List[String],
		   /* dvrs: List[DisjointVariables], TODO: refine */
		   hypotheses: Map[String, Hypothesis],
		   statements: Map[String, Statement])

sealed abstract class Symbol
case class Var(sym: String) extends Symbol
case class Con(sym: String) extends Symbol

sealed abstract class BadName
case class BadSymbol(sym: String) extends BadName
case class BadLabel(label: String) extends BadName

case class Statement(label: String, expr: Expression)

sealed abstract class Expression
sealed abstract class Hypothesis extends Expression
case class VariableType(t: Con, v: Var) extends Hypothesis
case class Logical(mark: Con, symbols: List[Symbol]) extends Hypothesis
case class Axiom(mark: Con, symbols: List[Symbol]) extends Expression
/* TODO: inference, i.e. with hypotheses. */
case class Theorem(mark: Con, symbols: List[Symbol],
		   proof: List[Statement]) extends Expression


object ExampleApp extends App {
  println("Hello, Metamath Tools.")
}

