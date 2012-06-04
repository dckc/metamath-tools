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

  def label: Parser[Symbol] =
    """[A-Za-z0-9\-_\.]+""".r ^^ { Symbol(_) }
  def math_symbol: Parser[Symbol] =
    ("[" + ascii_printable + """&&[^\$]]+""").r ^^ { Symbol(_) }
}

class Preprocessing extends Preliminaries
with token.Tokens
with lexical.Scanners {
  override type Elem = Char
  override def token = scoping_statement | statement

  def whitespace = ("[" + ws_char + "]").r

  def comment = (
    "$(" ~!
    ("(?:[" + allowed_range + """&&[^\$]]|\$["""
     + allowed_range + """&&[^\)]])*""").r
    ~! "$)"
  ) ^^ { case start ~ content ~ end => start + content + end }

  /** ignore comments */
  def igc[T](p: => Parser[T]): Parser[T] = (comment.*) ~> p

/*@@@@@@@
  def file_inclusion_command = igc("$[" ~> filename <~ "$]") ^^ {
    case filename => FileInclusion(filename) }
  def filename = ("[" + ascii_printable + """&&[^\$]]+""").r
*/

  def scoping_statement = block_start | block_end
  def block_start = igc( "${") ^^^ BlockStart()
  def block_end = igc( "$}") ^^^ BlockEnd()

  def statement =
    documentation.? ~! label.? ~! keyword7 ~! expression ~! proof.? <~ "$." ^^ {
      case doc ~ label ~ kw ~ expr ~ pf =>
	StatementParts(doc, label, kw, expr, pf)
    }

  def documentation: Parser[String] =
    (comment ~> documentation) | comment

      /* TODO: consider interning keywords to save memory */
  def keyword7 = ("$c" | "$v" | "$f" | "$e" | "$d" | "$a" | "$p")
  def expression = igc(math_symbol) *

  def proof: Parser[Proof] = "$=" ~> (
    (igc("(") ~> igc(label) .+  <~ igc(")") ) ~ ("[A-Z" + ws_char + "]").r ^^ {
      case labels ~ digits => CompressedProof(labels, digits)
    }
    | igc(label) .+ ^^ { case labels => ExplicitProof(labels) }
  )

  sealed abstract class StatementToken(kw: String) extends Token{
    def chars = kw
  }
  sealed abstract class ScopingStatement(kw: String)
		  extends StatementToken(kw)
  case class BlockStart
		  extends ScopingStatement("${")
  case class BlockEnd
		  extends ScopingStatement("$}")
  case class StatementParts(doc: Option[String],
			    label: Option[Symbol],
			    kw: String,
			    expr: List[Symbol],
			    pf: Option[Proof])
      extends StatementToken(kw)

/*@@@@@@
  case class ConstantDeclaration(cs: List[Symbol])
       extends StatementToken("$c")
  case class VariableDeclaration(vs: List[Symbol])
       extends StatementToken("$v")
  case class DisjointValueRestriction(vs: List[Symbol])
       extends StatementToken("$d")
  case class Labelled(doc: Option[String], label: Symbol,
		      expr: Expression) extends StatementToken(expr.kw) {
    override def toString = "\n " + label + " " + expr.toString() + " $."
  }

  sealed abstract class Expression(kw0: String) {
    val kw = kw0
    def expr: List[Symbol]
    def in_context(ctx: Context): Either[BadName, Int]
    def toString() = {
      (List(kw) ++ (expr map { _.name })) mkString " "
    }
  }

  sealed abstract class Hypothesis(kw: String) extends Expression(kw)
  case class VariableType(t: Symbol, v: Symbol) extends Hypothesis("$f") {
    override def expr = List(t, v)

    override def in_context(ctx: Context): Either[BadName, Int] = {
      (ctx.active_constant(t), ctx.active_variable(v)) match {
	case (Right(k), Right(v)) => Right(2)
	case (Left(oops), _) => Left(oops)
	case (_, Left(oops)) => Left(oops)
      }
      }
  }

  case class Logical(k: Symbol, syms: List[Symbol]) extends Hypothesis("$e") {
    override def expr = syms
    override def in_context(ctx: Context): Either[BadName, Int] = {
      (ctx.active_constant(k), ctx.active_symbols(syms)) match {
	case (Right(k), Right(v)) => Right(2)
	case (Left(oops), _) => Left(oops)
	case (_, Left(oops)) => Left(oops)
      }
    }
  }

  sealed abstract class Assertion(kw: String) extends Expression(kw)
  case class Axiom(k: Symbol, syms: List[Symbol])
       extends Assertion("$a") {
	 override def expr = k :: syms
       }
  case class Theorem(k: Symbol, syms: List[Symbol], pf: Proof)
       extends Assertion("$p") {
	 override def expr = k :: syms
	 override def toString = super.toString() + "\n$= " + proof
       }
*/

  /** TODO: Proof.toString() */
  sealed abstract class Proof
  case class CompressedProof(labels: List[Symbol], digits: String) extends Proof
  case class ExplicitProof(labels: List[Symbol]) extends Proof
}


class BasicSyntax extends syntactical.TokenParsers {
  override type Tokens = Preprocessing
  override val lexical = new Preprocessing

  val ctx = Context.initial()
  def d0 = Database(List())
  def d1(s: Statement) = Database(List(s))

  def database = statements ^^ { case db => (ctx, db) }

  def statements: Parser[Database] = (
    statement ~! statements ^^ {
      case Database(ss1) ~ Database(ss2) =>
	Database(ss1 ++ ss2) }
    | success(d0)
  )

  def statement: Parser[Database] = (
    block
    | declaration
    /*@@| labelled_statement*/
  )

  def block = (
    acceptMatch("${",
		{ case bs: lexical.BlockStart => ctx.push() })
    ~! statements
    <~ acceptMatch("$}",
		   { case be: lexical.BlockEnd => be }) ) ^^ {
      case save_ctx ~ db => ctx.pop(save_ctx, db)
  }

/*
		    ~! (igc("$=") ~> proof).?
		    <~ "$.") ^? ({
    case "$f" ~ (t :: v :: Nil) ~ None =>
      VariableType(t, v)
    case "$e" ~ (k :: expr) ~ None =>
      Logical(k, expr)
    case "$a" ~ (k :: expr) ~ None =>
      Axiom(k, expr)
    case "$p" ~ (k :: expr) ~ Some(pf) =>
      Theorem(k, expr, pf)
  }, {
    case "$f" ~ expr ~ None =>
      "$f must be followed by 2 symbols; got " + expr.length
    case "$e" ~ expr ~ None =>
      "$e must be followed by constant, symbols"
    case "$a" ~ expr ~ None =>
      "$a must be followed by constant, symbols"
    case "$p" ~ expr ~ Some(pf) =>
      "$p must be followed by constant, symbols"
    case "$p" ~ expr ~ None =>
      "missing $= in $p statement"
    case kw ~ expr ~ Some(pf) =>
      "unexpected $= in " + kw + " statement"
  })
*/

  def declaration = acceptMatch("declare", {
      case lexical.StatementParts(_, None, "$c", syms, None) =>
	ctx.add_constants(syms); d0
      case lexical.StatementParts(_, None, "$v", syms, None) =>
	ctx.add_variables(syms); d0
      case lexical.StatementParts(_, None, "$d", syms, None) =>
	/*@@*/ d0
  })

/*@@
  def labelled_statement: Parser[Database] = acceptMatch("labelled", {
    case st @ lexical.Labelled(_, _, e) => (st, e.in_context(ctx)) }) ^? ({
      case (st, Right(aux)) => d1(Statement(0, aux))
    },{
      case (st, Left(oops)) => oops.toString()
    })
*/

/*@@@@@
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

case class Statement(st: Int, aux: Int) {
  override def toString = st.toString()
}

/**
 * not functional, but HmmImpl.hs seems to use the state monad...
 */
case class Context(var symbols: Map[Symbol, MathSymbol],
		   /* dvrs: List[DisjointVariables], TODO: refine */
		   /* TODO: var hypotheses: Map[Symbol, Hypothesis], */
		   var statements: Map[Symbol, Statement],
		   val top: Boolean) {
  def push() = Context(
    symbols, statements, false)

  def pop(save_ctx: Context, db:Database): Database = {
/*
    val assertions = db.statements filter {
      case Statement(Labelled(_, _, a: Assertion)) => true
      case _ => false
    }
    statements = save_ctx.statements ++ (
      assertions map {_.st.label} zip assertions)
    Database(assertions)
*/
    Database(List())
  }

  def add_constants(syms: List[Symbol]) =
    symbols = symbols ++ (syms zip (syms map { Con(_) }))
  def add_variables(syms: List[Symbol]) =
    symbols = symbols ++ (syms zip (syms map { Var(_) }))

  def active_symbol(x: Symbol): Either[BadSymbol, MathSymbol] =
    symbols get x match {
      case Some(sym) => Right(sym)
      case None => Left(BadSymbol(x.name))
    }
  def active_constant(x: Symbol) = 
    symbols get x match {
      case Some(k @ Con(_)) => Right(k)
      case _ => Left(BadSymbol(x.name))
    }
  def active_variable(x: Symbol) =
    symbols get x match {
      case Some(v @ Var(_)) => Right(v)
      case _ => Left(BadSymbol(x.name))
    }

  def active_symbols(xs: List[Symbol]) =
    fold_either(xs map active_symbol)

  def fold_either[L, R](ss: List[Either[L, R]]
		      ): Either[L, List[R]] = ss match {
    case List() => Right(List())
    case Left(oops) :: _ => Left(oops)
    case Right(x) :: xss => fold_either(xss) match {
      case Right(xs) => Right(x :: xs)
      case Left(oops) => Left(oops)
    }
  }
}
object Context{
  def initial() = Context(Map(), Map(), true)
}


sealed abstract class MathSymbol
case class Var(n: Symbol) extends MathSymbol
case class Con(s: Symbol) extends MathSymbol

sealed abstract class BadName
case class BadSymbol(sym: String) extends BadName
case class BadLabel(label: String) extends BadName

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
