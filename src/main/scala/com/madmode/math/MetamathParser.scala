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
  /* WIERD! putting allowed_range before ascii_printable causes it to
   * get the value "nullnull". */
  val ascii_printable = "\u0021-\u007f"
  val ws_char = " \t\r\n\f"
  val allowed_range = ascii_printable + ws_char

  def tokens = rep(keyword | label | math_symbol)
  def keyword = ( "${" | "$}" | "$c" | "$v" | "$f" | "$e"
		 | "$d" | "$a" | "$p" | "$." | "$="
		 | "$(" | "$)" | "$[" | "$]" )

  def label: Parser[String] = """[A-Za-z0-9\-_\.]+""".r
  def math_symbol: Parser[String] = ("[" + ascii_printable + """&&[^\$]]+""").r
}

class Preprocessing extends Preliminaries {
  /**
   * Note use of reluctant *?
   * http://docs.oracle.com/javase/1.4.2/docs/api/java/util/regex/Pattern.html
   */
  override val whiteSpace = (
    "(?:[" + ws_char + "]|(?:" +
    """\$\(""" + "[" + allowed_range + "]*?" + """\$\)"""
    + "))+"
  ).r


  def file_inclusion_command = "$[" ~ filename ~ "$]"
  def filename = ("[" + ascii_printable + """&&[^\$]]+""").r
}

class BasicSyntax extends Preprocessing with CheckedParser {
  val ctx = Context(List(), List(), Map(), Map())
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

  def d0 = Database(List())
  def d1(s: Statement) = Database(List(s))

  def declare_constants = "$c" ~> math_symbol .* <~ "$." ^^ { case syms =>
    ctx.constants = ctx.constants ++ syms
    d0
  }
  def declare_variables = "$v" ~> math_symbol .* <~ "$." ^^ { case syms =>
    ctx.variables = ctx.variables ++ syms
    d0
  }

  def disjoint_variable_restriction: Parser[Database] = (
    "$d" ~> rep(active_variable) <~ "$." ^^ {
      case _ => d0
    }
  )

  def labelled_statement: Parser[Database] = label ~ expr <~ "$." ^^ {
    case l ~ Right(e) => {
      ctx.statements = ctx.statements + (l -> Statement(l, e))
      d1(Statement(l, e))
    }
    case oops => {
      println("" + oops)
      /* TODO: report error nicely. */
      d0
    }
  }

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

/**
 * not functional, but HmmImpl.hs seems to use the state monad...
 */
case class Context(var constants: List[String],
		   var variables: List[String],
		   /* dvrs: List[DisjointVariables], TODO: refine */
		   var hypotheses: Map[String, Hypothesis],
		   var statements: Map[String, Statement])

sealed abstract class Symbol
case class Var(n: String) extends Symbol
case class Con(s: String) extends Symbol

sealed abstract class BadName
case class BadSymbol(sym: String) extends BadName
case class BadLabel(label: String) extends BadName

case class Statement(label: String, expr: Expression) {
  override def toString = label + " " + expr.toString() + " $.\n"
}

sealed abstract class Expression {
  def format(kw: String, expr: List[Symbol]) = {
    (List(kw) ++ (expr map {
      case Var(n) => n
      case Con(s) => s
    })) mkString " "
  }
}

sealed abstract class Hypothesis extends Expression
case class VariableType(t: Con, v: Var) extends Hypothesis {
  override def toString() = format("$f", List(t, v))
}

case class Logical(mark: Con, symbols: List[Symbol]) extends Hypothesis {
  override def toString = format("$e", mark :: symbols)
}
case class Axiom(mark: Con, symbols: List[Symbol]) extends Expression {
  override def toString = format("$a", mark :: symbols)
}
/* TODO: inference, i.e. with hypotheses. */
case class Theorem(mark: Con, symbols: List[Symbol],
		   proof: List[Statement]) extends Expression {
  override def toString = format("$p", mark :: symbols) + "\n$= " + (
    proof map { _.label } mkString " " )
}


object ExampleApp extends App {
  override def main(args: Array[String]) {
    if (args.length != 2) {
      println("Usage: parser input_file")
      return
    }

    val infn = args(1)
    val fis = new java.io.FileInputStream(infn)
    val isr = new java.io.InputStreamReader(fis)
    val bs = new BasicSyntax()
    val ctx, db = bs.parseAll(bs.database, isr)
    println("Context: " + ctx)
    println("Database: " + db)
  }
}

