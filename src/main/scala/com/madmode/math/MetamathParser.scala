/**
 * ref:
 * Metamath
 * A Computer Language for Pure Mathematics
 * Norman Megill
 * http://us.metamath.org/index.html#book
 * ISBN: 978-1-4116-3724-5
 * esp. section 4.1 Specification of the Metamath Language
 *
 * ack: HmmImpl.hs (@@cite fully)
 *
 */
package com.madmode.math

import scala.util.parsing.combinator.{ Parsers, RegexParsers, PackratParsers }
import scala.util.parsing.combinator.{ lexical, syntactical, token }
import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.{PagedSeqReader}
import java.io.{InputStreamReader,FileInputStream}

import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}

abstract class PreliminariesPEG extends Parser {

  def ascii_printable =
    rule { "\u0021" - "\u007f" }
  def ascii_printable_but_dollar =
    rule { ("\u0021"-"\u0023") | ("\u0025"-"\u007f") }
  def ascii_printable_but_cparen =
    rule { ("\u0021"-"\u0028") | ("\u002A"-"\u007f") }
  def ws_char =
    rule { " " | "\t" | "\r" | "\n" | "\f" }
  def allowed_range =
    rule { ascii_printable | ws_char }

  def label: Rule1[String] =
    rule { oneOrMore(letter | digit | "-" | "_" | ".") } ~> (s => s)
  def letter =
    rule { ("A" - "Z") | ("a" - "z") }
  def digit =
    rule { "0" - "9" }

  def math_symbol: Rule1[String] =
    rule { oneOrMore(ascii_printable_but_dollar) } ~> ( s => s )
}



abstract class Preliminaries extends RegexParsers {

  /* WIERD! putting allowed_range before ascii_printable causes it to
   * get the value "nullnull". */
  val ascii_printable = "\u0021-\u007f"
  val ascii_printable_but_dollar = "\u0021-\u0023\u0025-\u007f"
  val ascii_printable_but_cparen = "\u0021-\u0028\u002A-\u007f"
  def ws_char = " \t\r\n\f"
  val allowed_range = ascii_printable + ws_char

  def label: Parser[String] =
    """[A-Za-z0-9\-_\.]+""".r
  def math_symbol: Parser[String] =
    ("[" + ascii_printable_but_dollar + """]+""").r
}

class PreprocessingPEG extends PreliminariesPEG {
  // TODO: EOF
  def thingy = scoping_statement | statement
  def statements = zeroOrMore(thingy) /* for testing; not used in token */

  def whitespace: Rule0 =
    rule { oneOrMore(ws_char) }

  var last_comment: Option[String] = None
  def comment: Rule0 = (
    "$(" ~
    zeroOrMore(( ws_char | ascii_printable_but_dollar )
	       | ("$" ~ ( ws_char | ascii_printable_but_cparen ) ))
    ~% { case content => last_comment = Some(content) }
    ~ "$)"
  )

  /** ignore whitespace and comments */
  /** TODO: lexically include files */
  def igc[T](p: => Rule1[T]): Rule1[T] =
    zeroOrMore(whitespace | comment | file_inclusion_command) ~ p
  def igc(p: => Rule0): Rule0 =
    zeroOrMore(whitespace | comment | file_inclusion_command) ~ p

  def file_inclusion_command = rule { "$[" ~ filename ~ "$]" }
  def filename = rule { oneOrMore(ascii_printable_but_dollar) }

  def scoping_statement = block_start | block_end
  def block_start = igc( "${" ) ~> ( input => BlockStart() )
  def block_end = igc( "$}" ) ~> ( input => BlockEnd() )

  def statement =
    (label_kw_expr ~ optional(proof) ~ "$." ) ~~> {
      (lke: (Option[String], String, List[String]),
       pf: Option[Proof]) => {
	lke match {
	  case (label, kw, expr) =>
	    val s = StatementParts(last_comment, label, kw, expr, pf)
	  last_comment = None
	  s
	}
      }
    }

  /* seems we can only pop 3 off the stack at a time. */
  def label_kw_expr =
    rule { (optional(label /*TODO igc()*/) ~ keyword7 ~ expression) ~~>
	  { (label_opt: Option[String], kw: String, expr: List[String]) =>
	    (label_opt, kw, expr) }
	}

  // TODO: igc()
  def keyword7: Rule1[String] = rule {
    ("$c" | "$v" | "$f" | "$e" | "$d" | "$a" | "$p") ~> (s => s)
  }
  def expression: Rule1[List[String]] = rule {
    zeroOrMore(math_symbol, whitespace | comment)
  }

  def proof: Rule1[Proof] =
    rule { "$=" ~ (
      (igc("(") ~ zeroOrMore(igc(label)) ~ igc(")") ~
       oneOrMore( ("A" - "Z") | ws_char ) ~> (s => s)) ~~> {
	(labels: List[String], digits: String) =>
	  Proof(labels, Some(digits))
      }
      | oneOrMore(igc(label)) ~~> {
	labels: List[String] => Proof(labels, None) }
    ) }

  sealed abstract class StatementToken(kw: String) {
    def chars = kw
  }
  sealed abstract class ScopingStatement(kw: String)
		  extends StatementToken(kw)
  case class BlockStart()
		  extends ScopingStatement("${")
  case class BlockEnd()
		  extends ScopingStatement("$}")
  case class StatementParts(doc: Option[String],
			    label: Option[String],
			    kw: String,
			    expr: List[String],
			    pf: Option[Proof]) extends StatementToken(kw)

}


class Preprocessing extends Preliminaries
with token.Tokens
with lexical.Scanners {
  override type Elem = Char
  override def token = scoping_statement | statement | eof

  override def whitespace = ("[" + ws_char + "]*").r

  var last_comment: Option[String] = None
  def comment = (
    "$(" ~!
    ("(?:[" + ws_char + ascii_printable_but_dollar + "]|" +
     """(?:\$[""" + ws_char + ascii_printable_but_cparen + """]))*+""").r
    ~! "$)"
  ) ^^ {
    case start ~ content ~ end => {
      last_comment = Some(content)
      ""
    }
  }

  /** ignore comments */
  /** TODO: lexically include files */
  def igc[T](p: => Parser[T]): Parser[T] =
    ((comment | file_inclusion_command).*) ~> p

  def file_inclusion_command = "$[" ~! filename ~! "$]" ^^ {
    case open ~ filename ~ close => filename }
  def filename = ("[" + ascii_printable + """&&[^\$]]+""").r

  def scoping_statement = block_start | block_end
  def block_start = igc( "${" ) ^^^ BlockStart()
  def block_end = igc( "$}" ) ^^^ BlockEnd()

  def statement =
    (igc(label).? ~! keyword7 ~! expression ~! proof.?) <~ "$." ^^ {
      case label ~ kw ~ expr ~ pf => {
	val s = StatementParts(last_comment, label, kw, expr, pf)
	last_comment = None
	s
      }
    }
  def statements = commit(token).* /* for testing; not used in token */

  def keyword7 = igc("$c" | "$v" | "$f" | "$e" | "$d" | "$a" | "$p")
  def expression = igc(math_symbol) *

  def proof: Parser[Proof] = "$=" ~> (
    (igc("(") ~> igc(label) .*  <~ igc(")") ) ~ ("[A-Z" + ws_char + "]+").r ^^ {
      case labels ~ digits => Proof(labels, Some(digits))
    }
    | igc(label) .+ ^^ { case labels => Proof(labels, None) }
  )

  def eof =
    comment.* ^^^ EOF

  sealed abstract class StatementToken(kw: String) extends Token{
    def chars = kw
  }
  sealed abstract class ScopingStatement(kw: String)
		  extends StatementToken(kw)
  case class BlockStart()
		  extends ScopingStatement("${")
  case class BlockEnd()
		  extends ScopingStatement("$}")
  case class StatementParts(doc: Option[String],
			    label: Option[String],
			    kw: String,
			    expr: List[String],
			    pf: Option[Proof]) extends StatementToken(kw)

}


case class Proof(labels: List[String], digits: Option[String])

class BasicSyntax extends syntactical.TokenParsers {
  def parse[T](p: Parser[T], in: String) =
    p(new lexical.Scanner(in))

  def parse[T](p: Parser[T], in: java.io.Reader) =
    p(new lexical.Scanner(new PagedSeqReader(PagedSeq.fromReader(in))))

  def parseAll[T](p: Parser[T], in: String) =
    parse(phrase(p), in)

  def parseAll[T](p: Parser[T], in: java.io.Reader) =
    parse(phrase(p), in)

  override type Tokens = Preprocessing
  /** TODO: consider moving to constructor arg */
  override val lexical = new Preprocessing

  val ctx = Context.initial()
  def d0 = Database(List())
  def d1(s: Labelled) = Database(List(s))

  def database = phrase(statements) ^^ { case db => (ctx, db) }

  def statements: Parser[Database] = (
    statement ~! statements ^^ {
      case Database(ss1) ~ Database(ss2) => Database(ss1 ++ ss2) }
    | success(d0)
  )

  def statement: Parser[Database] = (
    block
    | declaration
    | labelled_statement
  )

  def block = (
    acceptMatch("${",
		{ case bs: lexical.BlockStart => ctx.push() })
    ~! statements
    <~ acceptMatch("$}",
		   { case be: lexical.BlockEnd => be }) ) ^^ {
      case save_ctx ~ db => ctx.pop(save_ctx, db)
  }

  def declaration = acceptMatch("declare", {
    case d @ lexical.StatementParts(_, None, _, _, _) => d}) ^? ({
      case lexical.StatementParts(_, None, "$c", syms, None) =>
	{ ctx.add_constants(syms); d0 }
      case lexical.StatementParts(_, None, "$v", syms, None) =>
	{ ctx.add_variables(syms); d0 }
      case lexical.StatementParts(_, None, "$d", syms, None) =>
	/*@@ TODO: distinct variable restriction handling */ d0
    }, {
      case lexical.StatementParts(_, None, kw @ LabelledKeyword(), syms, _) =>
	kw + " statement requires label"
      case lexical.StatementParts(_, None, kw, syms, Some(pf)) =>
	"unexpected proof in " + kw
    })

  val LabelledKeyword = "\\$[efap]".r
  def labelled_statement: Parser[Database] = acceptMatch("labelled", {
    case ls @ lexical.StatementParts(doc, Some(label), kw, expr, pf) =>
      (doc, label, in_context(kw, expr, pf)) }) ^? ({
	case (doc, label, Right(e)) => {
	  val ls = Labelled(doc, Symbol(label), e)
	  ctx.add_statement(ls)
	  d1(ls)
	}
      }, {
	case (_, _, Left(oops)) => oops.toString()
      })

  def in_context(kw: String, expr: List[String], pfopt: Option[Proof]
	       ): Either[BadExpr, Expression] =
    (kw, pfopt) match {
      case ("$f", None) => VariableType.in_context(ctx, expr)
      case ("$e", None) => Logical.in_context(ctx, expr)
      case ("$a", None) => Axiom.in_context(ctx, expr)
      case ("$p", Some(pf)) => Theorem.in_context(ctx, expr, pf)

      case ("$p", None) => Left(MissingProof())
      case (kw, Some(pf)) => Left(UnexpectedProof(kw))
    }

}


case class Database(statements: List[Labelled])

case class Labelled(doc: Option[String], label: Symbol,
		    expr: Expression) {
  override def toString = "\n " + label.name + " " + expr.toString() + " $."
}

sealed abstract class Expression(kw: String) {
  def expr: List[MathSymbol]
  override def toString() = {
    (List(kw) ++ (expr map { _.name })) mkString " "
  }
}


sealed abstract class Hypothesis(kw: String) extends Expression(kw)
case class VariableType(t: Con, v: Var) extends Hypothesis("$f") {
  override def expr = List(t, v)
}
object VariableType {
  def in_context(ctx: Context,
		 expr: List[String]): Either[BadExpr, VariableType] = {
    expr match {
      case t :: v :: Nil => {
	(ctx.active_constant(t), ctx.active_variable(v)) match {
	  case (Right(k), Right(v)) => Right(VariableType(k, v))
	  case (Left(oops), _) => Left(oops)
	  case (_, Left(oops)) => Left(oops)
	}
      }
      case _ => Left(BadLength(expr))
    }
  }
}

case class Logical(k: Con, syms: List[MathSymbol]) extends Hypothesis("$e") {
  override def expr = syms
}
object Logical {
  def in_context(ctx: Context,
		 expr: List[String]): Either[BadExpr, Logical] = {
    ctx.active_parts(expr) match {
      case Right((k, ss)) => Right(Logical(k, ss))
      case Left(oops) => Left(oops)
    }
  }
}

sealed abstract class Assertion(kw: String) extends Expression(kw)
case class Axiom(k: Con, syms: List[MathSymbol]) extends Assertion("$a") {
  override def expr = k :: syms
}
object Axiom {
  def in_context(ctx: Context,
		 expr: List[String]): Either[BadExpr, Axiom] = {
    ctx.active_parts(expr) match {
      case Right((k, ss)) => Right(Axiom(k, ss))
      case Left(oops) => Left(oops)
    }
  }
}

case class Theorem(k: Con, syms: List[MathSymbol],
		   pf_steps: List[Labelled],
		   compressed: Option[String]) extends Assertion("$p") {
  override def expr = k :: syms
  override def toString = {
    val steps = (pf_steps map { _.label }) mkString(" ")
    super.toString() + "\n$= " + (compressed match {
      case None => steps
      case Some(digits) => "( " + steps + " ) " + digits
    })
  }
}
object Theorem {
  def in_context(ctx: Context,
		 expr: List[String],
		 pf: Proof): Either[BadExpr, Theorem] = {
    ctx.active_parts(expr) match {
      case Right((k, ss)) => {
	ctx.active_labels(pf.labels) match {
	  case Right(statements) => Right(Theorem(k, ss, statements, pf.digits))
	  case Left(oops) => Left(oops)
	}
      }
      case Left(oops) => Left(oops)
    }
  }
}


/**
 * not functional, but HmmImpl.hs seems to use the state monad...
 */
case class Context(var symbols: Map[String, MathSymbol],
		   /* dvrs: List[DisjointVariables], TODO: refine */
		   /* TODO: var hypotheses: Map[Symbol, Hypothesis], */
		   var statements: Map[String, Labelled],
		   top: Boolean) {
  def push() = Context(
    symbols, statements, false)

  def pop(save_ctx: Context, db:Database): Database = {
    val assertions = db.statements filter {
      case Labelled(_, _, a: Assertion) => true
      case _ => false
    }
    statements = save_ctx.statements ++ (
      (assertions map {_.label.name}) zip assertions)
    Database(assertions)
  }

  def add_statement(s: Labelled) =
    statements = statements + (s.label.name -> s)

  def add_constants(syms: List[String]) =
    symbols = symbols ++ (syms zip (syms map { Con(_) }))
  def add_variables(syms: List[String]) =
    symbols = symbols ++ (syms zip (syms map { Var(_) }))

  def active_symbol(x: String): Either[BadSymbol, MathSymbol] =
    symbols get x match {
      case Some(sym) => Right(sym)
      case None => Left(BadSymbol(x))
    }
  def active_constant(x: String) = 
    symbols get x match {
      case Some(k @ Con(_)) => Right(k)
      case _ => Left(BadSymbol(x))
    }
  def active_variable(x: String) =
    symbols get x match {
      case Some(v @ Var(_)) => Right(v)
      case _ => Left(BadSymbol(x))
    }

  def active_symbols(xs: List[String]) =
    fold_either(xs map active_symbol)

  def active_parts(expr: List[String]): Either[BadExpr,
					       (Con, List[MathSymbol])] =
    expr match {
      case k :: sym :: syms => {
	(active_constant(k), active_symbols(sym :: syms)) match {
	  case (Right(k), Right(ss)) => Right((k, ss))
	  case (Left(oops), _) => Left(oops)
	  case (_, Left(oops)) => Left(oops)
	}
      }
      case _ => Left(BadLength(expr))
    }
					       
  def active_labels(labels: List[String]) =
    fold_either(
      for (l <- labels) yield (statements get l) match {
	case Some(s) => Right(s)
	case None => Left(BadLabel(l))
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
}
object Context{
  def initial() = Context(Map(), Map(), true)
}


sealed abstract class MathSymbol {
  def name: String
}
case class Var(n: String) extends MathSymbol {
  def name = n
}
case class Con(s: String) extends MathSymbol {
  def name = s
}

sealed abstract class BadExpr
case class BadLabel(label: String) extends BadExpr
case class BadSymbol(sym: String) extends BadExpr
case class BadLength(syms: List[String]) extends BadExpr
case class MissingProof() extends BadExpr
case class UnexpectedProof(kw: String) extends BadExpr


object Utility extends App {
  def reader(filename: String) = {
    val bytes_in = new FileInputStream(filename)
    val chars_in = new InputStreamReader(bytes_in)
    chars_in
  }

  def minline(r: InputStreamReader): String = {
    val br = new java.io.BufferedReader(r)
    val lines: Stream[String] = {
      def loop(): Stream[String] = {
	val l = br.readLine()
	if (l == null) {
	  Stream[String]()
	} else {
	  l #:: loop()
	}
      }
      loop()
    }
    lines.max
  }

  override def main(args: Array[String]) {
    if (args.length != 2) {
      println("Usage: parser input_file")
      return
    }

    val infn = args(1)

    println("decode:")
    println(minline(reader(infn)))

    val p = new Preprocessing()

    println("tokenize:")
    p.parseAll(p.statements, reader(infn)) match {
      case p.Success(stmts, _) => {
	println("statements: " + stmts.length)
      }
      case ns @ p.NoSuccess(_, _) => {
	println(ns)
      }
    }

    val bs = new BasicSyntax()
    println("parse:")
    bs.parseAll(bs.database, reader(infn)) match {
      case bs.Success((ctx, db), _) => {
	println("Context symbols: " + ctx.symbols.size)
	/*println("Context hypotheses: " + ctx.hypotheses.size)*/
	println("Context statements: " + ctx.statements.size)
	println("Database statements: " + db.statements.length)

	for (s <- db.statements.take(20))
	  println(s)
      }
      case ns @ bs.NoSuccess(_, _) => {
	println(ns)
      }
    }
  }
}
