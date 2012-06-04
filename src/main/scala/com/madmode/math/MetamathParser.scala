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

import scala.util.parsing.combinator.{ Parsers, RegexParsers, PackratParsers }
import scala.util.parsing.combinator.{ lexical, syntactical, token }
import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.{PagedSeqReader}
import java.io.{InputStreamReader,FileInputStream}

abstract class Preliminaries extends RegexParsers {

  /* WIERD! putting allowed_range before ascii_printable causes it to
   * get the value "nullnull". */
  val ascii_printable = "\u0021-\u007f"
  def ws_char = " \t\r\n\f"
  val allowed_range = ascii_printable + ws_char

  /*@@ def tokens = rep(keyword | label | math_symbol) */
  def keyword = ( "${" | "$}" | "$c" | "$v" | "$f" | "$e"
		 | "$d" | "$a" | "$p" | "$." | "$="
		 | "$(" | "$)" | "$[" | "$]" )

  def label: Parser[String] = """[A-Za-z0-9\-_\.]+""".r
  def math_symbol: Parser[String] = ("[" + ascii_printable + """&&[^\$]]+""").r
}

class Preprocessing extends Preliminaries
with token.Tokens
with lexical.Scanners {
  override type Elem = Char
  override def token = (
    file_inclusion_command
    | block_start | block_end
    | unlabelled_statement
    | labelled_statement
  )

  def whitespace = ("[" + ws_char + "]").r

  def comment = (
    "$(" ~!
    ("(?:[" + allowed_range + """&&[^\$]]|\$["""
     + allowed_range + """&&[^\)]])*""").r
    ~! "$)"
  ) ^^ { case start ~ content ~ end => start + content + end }

  /** ignore comments */
  def igc[T](p: => Parser[T]): Parser[T] = (comment.*) ~> p

  def file_inclusion_command = igc("$[" ~> filename <~ "$]") ^^ {
    case filename => FileInclusion(filename) }
  def filename = ("[" + ascii_printable + """&&[^\$]]+""").r

  def block_start = igc( "${") ^^^ BlockStart
  def block_end = igc( "$}") ^^^ BlockEnd

  def unlabelled_statement = (
    igc( ("$c" | "$v" | "$d") )
    ~! igc(math_symbol).*
    <~ "$."
    ) ^^ {
    case "$c" ~ symbols => ConstantDeclaration(symbols)
    case "$v" ~ symbols => VariableDeclaration(symbols)
    case "$d" ~ symbols => DisjointValueRestriction(symbols)
  }

  def documentation: Parser[String] =
    (comment ~> documentation) | comment
  def labelled_statement = (
    documentation.? ~ label ~! igc ("$f" | "$e" | "$a" | "$p")
    ~! (igc(math_symbol) *)
    ~! (igc("$=") ~> proof).?
    <~ "$."
  ) ^? ({
    case doc ~ label ~ "$f" ~ expr ~ None =>
      FloatingHypothesis(doc, label, expr)
    case doc ~ label ~ "$e" ~ expr ~ None =>
      EssentialHypothesis(doc, label, expr)
    case doc ~ label ~ "$a" ~ expr ~ None =>
      AxiomaticAssertion(doc, label, expr)
    case doc ~ label ~ "$p" ~ expr ~ Some(pf) =>
      ProvableAssertion(doc, label, expr, pf)
  }, {
    case doc ~ label ~ "$p" ~ expr ~ None =>
      "missing $= in $p statement"
    case doc ~ label ~ kw ~ expr ~ Some(pf) =>
      "unexpected $= in " + kw + " statement"
  })

  def proof: Parser[Proof] = (
    (igc("(") ~> igc(label) .+  <~ igc(")") ) ~ ("[A-Z" + ws_char + "]").r ^^ {
      case labels ~ digits => CompressedProof(labels, digits)
    }
    | igc(label) .+ ^^ { case labels => ExplicitProof(labels) }
  )

/*@@indent*/
sealed abstract class StatementToken(kw: String) extends Token{
  def chars = kw
}
case class FileInclusion(filename: String)
     extends StatementToken("${")
sealed abstract class ScopingStatement(kw: String)
		extends StatementToken(kw)
case object BlockStart
		extends ScopingStatement("${")
case object BlockEnd
		extends ScopingStatement("$}")
case class ConstantDeclaration(cs: List[String])
     extends StatementToken("$c")
case class VariableDeclaration(vs: List[String])
     extends StatementToken("$v")
case class DisjointValueRestriction(vs: List[String])
     extends StatementToken("$d")
case class FloatingHypothesis(doc: Option[String], label: String,
			      expr: List[String])
     extends StatementToken("$f")
case class EssentialHypothesis(doc: Option[String], label: String,
			       expr: List[String])
     extends StatementToken("$e")
case class AxiomaticAssertion(doc: Option[String], label: String,
			      expr: List[String])
     extends StatementToken("$a")
case class ProvableAssertion(doc: Option[String], label: String,
			     expr: List[String],
			     pf: Proof)
     extends StatementToken("$p")

sealed abstract class Proof
case class CompressedProof(labels: List[String], digits: String) extends Proof
case class ExplicitProof(labels: List[String]) extends Proof
}

class BasicSyntax extends syntactical.TokenParsers {
  override type Tokens = Preprocessing
  override val lexical = new Preprocessing

/*
  val ctx = Context.initial()
  def database = statements ^^ { case db => (ctx, db) }

  /@@**
   * for testing...
   @@/
  def statements_lex: Parser[List[List[String]]] = rep(statement_lex)
  def statement_lex: Parser[List[String]] = (
    "${" ^^ { case kw => List(kw) }
    | "$}" ^^ { case kw => List(kw) }
    | ( opt(label) ~! ("$c" | "$v" | "$f" | "$e" | "$d" | "$a" | "$p")
       ~! rep(math_symbol) ~! proof_lex ~! "$.") ^^ {
	 case lopt ~ kw ~ syms ~ pf_opt ~ end => {
	   val tokens = (List() ++ lopt ++ List(kw) ++ syms
	    ++ pf_opt ++ List(end))
	   tokens
	 }
       }
  )

  def statements: Parser[Database] = (
    statement ~! statements ^^ {
      case Database(ss1) ~ Database(ss2) =>
	Database(ss1 ++ ss2) }
    | success(d0)
  )

  def statement: Parser[Database] = (
    block
    | ( "$c" ~! commit(declare_constants)
       | "$v" ~! commit(declare_variables)
       | "$d" ~! commit(disjoint_variable_restriction)
    ) ^^ { case kw ~ decl => decl }
    | labelled_statement
  )

  def d0 = Database(List())
  def d1(s: Statement) = Database(List(s))

  def block = block_start ~! statements <~ "$}" ^^ {
    case save_ctx ~ Database(statements) => {
      val assertions = statements filter {
	case Statement(_, a: Assertion) => true
	case _ => false
      }
      ctx.statements = save_ctx.statements ++ (
	assertions map {_.label} zip assertions)
      Database(assertions)
    }
  }
  def block_start = "${" ^^ { case kw => ctx.push() }

  def declare_constants =
    math_symbol.* <~ "$." ^^ { case syms => { ctx.add_constants(syms); d0 } }
  def declare_variables =
    math_symbol.* <~ "$." ^^ { case syms => { ctx.add_variables(syms); d0 } }
  def active_symbol: Parser[Either[BadSymbol, Symbol]] =
    math_symbol ^^ { case s => ctx.active_symbol(s) }
  def active_constant =
    math_symbol ^^ { case s => ctx.active_constant(s) }
  def active_variable =
    math_symbol ^^ { case s => ctx.active_variable(s) }
  def active_symbols = rep(active_symbol) ^^ fold_either

  def fold_either[L, R](ss: List[Either[L, R]]
		 ): Either[L, List[R]] = ss match {
    case List() => Right(List())
    case Left(oops) :: _ => Left(oops)
    case Right(x) :: xss => fold_either(xss) match {
      case Right(xs) => Right(x :: xs)
      case Left(oops) => Left(oops)
    }
  }


  def disjoint_variable_restriction: Parser[Database] = (
    rep(active_variable) <~ "$." ^^ {
      case _ => d0
    }
  )

  def labelled_statement: Parser[Database] =
    label ~! expr ^^ {
      case l ~ e => {
	val stmt = Statement(l, e)
	ctx.statements = ctx.statements + (l -> stmt)
	d1(stmt)
      }
    }

  def expr: Parser[Expression] = (
    "$f" ~! variable_type_hypothesis
    | "$e" ~! logical_hypothesis
    | "$a" ~! axiom
    | "$p" ~! theorem
  ) ^^ { case kw ~ e => e }

  def variable_type_hypothesis: Parser[Expression] = (
    (active_constant ~ active_variable) <~ "$." ^? ({
      case Right(k) ~ Right(v) => VariableType(k, v)
    }, {
      case Left(oops) ~ _ => oops.toString()
      case _ ~ Left(oops) => oops.toString()
    })
  )

  def logical_hypothesis: Parser[Expression] = (
    (active_constant ~ active_symbols) <~ "$." ^? ({
      case Right(k) ~ Right(expr) => Logical(k, expr)
    }, {
      case Left(oops) ~ _ => oops.toString()
      case _ ~ Left(oops) => oops.toString()
    })
  )

  def axiom: Parser[Expression] = (
    (active_constant ~! active_symbols) <~ "$." ^? ({
      case Right(k) ~ Right(expr) => Axiom(k, expr)
    }, {
      case Left(oops) ~ _ => oops.toString()
      case _ ~ Left(oops) => oops.toString()
    })
    /@* TODO: hypotheses from ctx *@/
  )

  def theorem: Parser[Expression] = (
    (active_constant ~ active_symbols ~ ("$=" ~> proof_steps)) <~ "$." ^? ({
      case Right(k) ~ Right(expr) ~ Right(proof_steps) => {
	Theorem(k, expr, proof_steps)
      }
    }, {
      case Left(oops) ~ _ ~ _ => oops.toString()
      case _ ~ Left(oops) ~ _ => oops.toString()
      case _ ~ _ ~ Left(oops) => oops.toString()
    })
  )
  def proof_steps: Parser[Either[BadName, List[Statement]]] = (
    rep1(label) ^^ {
      case labels =>
	fold_either(
	  for (l <- labels) yield (ctx.statements get l) match {
	    case Some(s) => Right(s)
	    case None => Left(BadLabel(l))
	  }
	)
    }
    /@* TODO: compressed proofs *@/
    | ("(" ~> rep(label) <~ ")") ~ proof_step_indexes ^^ {
      case labels ~ digits => Right(List()) }
  )
  def proof_step_indexes = """[A-Z \t\r\f\n]+""".r
@@@@@@ */
}


/* ack: HmmImpl.hs */
case class Database(statements: List[Statement])

/**
 * not functional, but HmmImpl.hs seems to use the state monad...
 */
case class Context(var symbols: Map[String, Symbol],
		   /* dvrs: List[DisjointVariables], TODO: refine */
		   var hypotheses: Map[String, Hypothesis],
		   var statements: Map[String, Statement],
		   val top: Boolean) {
  def push() = Context(
    symbols, hypotheses, statements, false)
  def add_constants(syms: List[String]) =
    symbols = symbols ++ (syms zip (syms map { Con(_) }))
  def add_variables(syms: List[String]) =
    symbols = symbols ++ (syms zip (syms map { Var(_) }))

  def active_symbol(name: String): Either[BadSymbol, Symbol] =
    symbols get name match {
      case Some(sym) => Right(sym)
      case None => Left(BadSymbol(name))
    }
  def active_constant(name: String) = 
    symbols get name match {
      case Some(k @ Con(_)) => Right(k)
      case _ => Left(BadSymbol(name))
    }
  def active_variable(name: String) =
    symbols get name match {
      case Some(v @ Var(_)) => Right(v)
      case _ => Left(BadSymbol(name))
    }
}
object Context{
  def initial() = Context(Map(), Map(), Map(), true)
}


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

sealed abstract class Assertion extends Expression

case class Axiom(mark: Con, symbols: List[Symbol]) extends Assertion {
  override def toString = format("$a", mark :: symbols)
}
/* TODO: inference, i.e. with hypotheses. */
case class Theorem(mark: Con, symbols: List[Symbol],
		   proof: List[Statement]) extends Assertion {
  override def toString = format("$p", mark :: symbols) + "\n$= " + (
    proof map { _.label } mkString " " )
}


/*@@@@@
object Utility extends App {
  def reader(filename: String) = {
    val bytes_in = new FileInputStream(filename)
    val chars_in = new InputStreamReader(bytes_in)
    chars_in
  }

  override def main(args: Array[String]) {
    if (args.length != 2) {
      println("Usage: parser input_file")
      return
    }

    val infn = args(1)
    val bs = new BasicSyntax()

    println("tokenize:")
    bs.parseAll(bs.statements_lex, reader(infn)) match {
      case bs.Success(stmts, _) => {
	println("statements: " + stmts.length)
      }
      case bs.NoSuccess(failure, rest) => {
	println(failure)
	println(rest.pos)
      }
    }

    println("parse:")
    bs.parseAll(bs.database, reader(infn)) match {
      case bs.Success((ctx, db), _) => {
	println("Context symbols: " + ctx.symbols.size)
	println("Context hypotheses: " + ctx.hypotheses.size)
	println("Context statements: " + ctx.statements.size)
	println("Database statements: " + db.statements.length)
      }
      case bs.NoSuccess(failure, rest) => {
	println(failure)
	println(rest.pos)
      }
    }
  }
}
*/
