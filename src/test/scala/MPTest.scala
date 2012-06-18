package com.madmode.math

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import org.parboiled.scala._

class TestPreliminariesPEG extends FunSpec with ShouldMatchers {
  describe("4.1.1 Preliminaries") {
    val p = new PreprocessingPEG()

    it("""The only characters that are allowed to appear in a Metamath
source file are the 94 printable characters on standard ascii keyboards ...
plus the following non-printable (white space) characters: space, tab, car-
riage return, line feed, and form feed.""") {

      val r = ReportingParseRunner(p.comment).run("$( x \u0000 is not allowed $)")
      val s = r.result match {
	case Some(x) => x
	case None => r.parseErrors(0).getEndIndex() //  getEndIndex
      }
      s should equal (6)
    }

    it("""A Metamath database consists of a sequence of three kinds of tokens
       separated by white space.""") {
      val r = ReportingParseRunner(p.statements).run("axiom.1 $a |- x = x $.")
      val parts = r.result match {
	case Some(result) => result
	case None => r.parseErrors
      }
      parts should equal (
	p.StatementParts(None,Some("axiom.1"),"$a",
			 List("|-", "x", "=", "x"),None)
      )
    }
  }
}

class TestPreliminaries extends FunSpec with ShouldMatchers {
  describe("4.1.1 Preliminaries") {
    val p = new Preprocessing()

    it("""The only characters that are allowed to appear in a Metamath
source file are the 94 printable characters on standard ascii keyboards ...
plus the following non-printable (white space) characters: space, tab, car-
riage return, line feed, and form feed.""") {

      val s = p.parseAll(p.comment, "$( x \u0000 is not allowed $)") match {
	case p.Success(txt, _) => txt
	case p.NoSuccess(failure, where) => where.pos
      }
      s.toString() should equal ("1.6")
    }

    it("""A Metamath database consists of a sequence of three kinds of tokens
       separated by white space.""") {
      val parts = p.parseAll(p.token, "axiom.1 $a |- x = x $.") match {
	case p.Success(result, _) => result
	case p.NoSuccess(failure, _) => failure
      }
      parts should equal (
	p.StatementParts(None,Some("axiom.1"),"$a",
			 List("|-", "x", "=", "x"),None)
      )
    }

    it("""The set of keyword tokens is ${, $}, $c, $v, $f, $e, $d, $a,
       $p, $., $=, $(, $), $[, and $]. The last four are called
       auxiliary or preprocessing keywords. """) {
      def check(txt: String) = {
	p.parseAll(p.token, txt) match {
	  case p.Success(p.BlockStart(), _) => "${"
	  case p.Success(p.BlockEnd(), _) => "$}"
	  case p.Success(p.StatementParts(_, _, kw, _, _), _) => kw
	  case other => other
	}
      }
      check("${") should equal ("${")
      check("$}") should equal ("$}")
      check("$c x $.") should equal ("$c")
      check("$v x $.") should equal ("$v")
      check("label $f k x $.") should equal ("$f")
      check("label $e k x $.") should equal ("$e")
      check("$d v0 v1 $.") should equal ("$d")
      check("label $a k x $.") should equal ("$a")
      check("label $p k x $= step $.") should equal ("$p")

      check("$( doc $) label $a k $( ig $) x $.") should equal ("$a")
      check("$[ file $] $c x $.") should equal ("$c")
    }

    it("""A label token consists of any combination of
       letters, digits, and the characters hyphen, underscore, and period."""
     )
    {
      val l = p.parseAll(p.token, "letters09-_. $a k expr $.") match {
	case p.Success(p.StatementParts(_, Some(txt), _, _, _), _) =>
	  txt
	case fail @ p.NoSuccess(_, _) => fail
      }
      l should equal ("letters09-_.")

      val notl = p.parseAll(p.token, "letter$ $a k expr $.") match {
	case p.Success(x, _) => x
	case p.NoSuccess(failure, rest) => rest.pos
      }
      notl.toString() should equal ("1.7")
    }

    it("""A math symbol token may consist of any combination of the 93
          printable standard ascii characters other than $ .""") {
      val sym = p.parseAll(p.math_symbol, "|-") match {
	case p.Success(txt, _) => txt
	case p.NoSuccess(failure, _) => failure
      }
      sym should equal ("|-")
    }

    it("""All tokens are case-sensitive.""") {
      val sym = p.parseAll(p.math_symbol, "case-SENSITIVE") match {
	case p.Success(txt, _) => txt
	case p.NoSuccess(failure, _) => failure
      }
      sym should equal ("case-SENSITIVE")
    }

  }

  describe("extension: statement comments") {
    val p = new Preprocessing()

    it("""A comment before a statement is captured as a docstring""") {
      (p.parseAll(p.token, "$( doc $) label $a k x $.") match {
	case p.Success(p.StatementParts(Some(doc), _, _, _, _), _) => doc
	case other => other
      }) should equal ( "doc " )
    }
  }
  
}

class TestPreprocessing extends FunSpec with ShouldMatchers {
  describe("4.1.2 Preprocessing") {
    val p = new Preprocessing()

    it("""Comments are ignored (treated like white space) for the
       purpose of parsing."""  ) {
      val parts = p.parseAll(p.token,
			     """axiom.1 $a $( turnstile $) |- x = x $.
			     $( end $)""") match {
	case p.Success(result, _) => result
	case ns @ p.NoSuccess(_, _) => ns
      }
      parts should equal (
	p.StatementParts(None,Some("axiom.1"),"$a",
			 List("|-", "x", "=", "x"),None) )
    }

    it("The contents of the file replace the inclusion command.") (pending)

  }
}

class TestBasicSyntax extends FunSpec with ShouldMatchers {
  describe("4.1.3 Basic Syntax") {
    it("""After preprocessing, a database will consist of a sequence
       of statements.  """) {
      val bs = new BasicSyntax()
      bs.ctx.add_constants(List("|-", "="))
      bs.ctx.add_variables(List("x"))
      val db = bs.parseAll(bs.statements, "axiom.1 $a |- x = x $.") match {
	case bs.Success(db, _) => db
	case other => other
      }
      db should equal (
	Database(List(Labelled(None, Symbol("axiom.1"),
			       Axiom(Con("|-"),
				     List(Var("x"), Con("="), Var("x")))))) )
    }

    it("${ begins a block and a matching $} ends the block.") {
      val bs = new BasicSyntax()
      val db = bs.parseAll(bs.statements, "${ $} ${ ${ $} $}") match {
	case bs.Success(db, _) => db
	case other => other
      }
      db should equal (
	Database(List()) )
    }

    it("""These statements declare the math symbols to be variables
       or constants respectively.""") {
      val bs = new BasicSyntax()
      val ctx = bs.parseAll(bs.database, """
			    $c wff set |- ( ) -> $.
			    $v P Q x $.""") match {
	case bs.Success((ctx, Database(List())), _) => 
	  (Set() ++ ctx.symbols.values)
	case other => other
      }
      ctx should equal ( (Set(Con("wff"), Con("set"),
			      Con("|-"), Con("("), Con(")"), Con("->"),
			      Var("P"), Var("Q"), Var("x")) ))
    }

    it("""A math symbol becomes active when declared and stays active
       until the end of the block in which it is declared.""") {
      def check(txt: String) = {
	val bs = new BasicSyntax()
	bs.parseAll(bs.database, txt) match {
	  case bs.Success(x, _) => x
	  case ns @ bs.NoSuccess(why, _) => why
	}
      }
      check("""
	    $c wff set |- ( ) -> $.
	    w2 $a wff ( p -> q ) $.
	    """) should equal("BadSymbol(p)")
      check("""
	    stmt1 $f wff P $.
	    """) should equal("BadSymbol(wff)")
      check("""
	    $c wff set |- ( ) -> $.
	    $v p $.
	    w2 $e wff ( p -> q ) $.
	    """) should equal("BadSymbol(q)")
      check("""
	    $c wff set |- ( ) -> $.
	    $v p q $.
	    w2 $p wff ( p -> q ) $= x $.
	    """) should equal("BadLabel(x)")
    }

    it("""A variable may not be declared a second time while it is
       active, but it may be declared again (as a variable, but not as
       a constant) after it becomes inactive.""") (pending)

    it("""A constant must be declared in the outermost block and may
       not be declared a second time""") (pending)

    it("A hypothesis is a $f or $e statement.") {
      val bs = new BasicSyntax()
      val stmts = bs.parseAll(bs.database, """
			      $c wff set |- ( ) -> $.
			      $v P Q x $.
			      stmt1 $f wff P $.
			      stmt2 $f set x $.
			      stmt3 $e |- ( P -> Q ) $.
			      """
			     ) match {
	case bs.Success((ctx, Database(stmts)), _) => stmts
	case other => other
      }
      stmts should equal (
	List(Labelled(None, 'stmt1, VariableType(Con("wff"), Var("P"))),
	     Labelled(None, 'stmt2, VariableType(Con("set"), Var("x"))),
	     Labelled(None, 'stmt3, Logical(Con("|-"),
				       List(Con("("), Var("P"),
					    Con("->"), Var("Q"),
					    Con(")"))))
	   ) )
    }

    it("""A $d statement is also called a disjoint (or distinct)
       variable restriction.""") (pending)

    it("""An assertion is a $a or $p statement.""") {
      val bs = new BasicSyntax()
      val stmts = bs.parseAll(bs.database, """
			      $c ( ) -> wff $.
			      $v p q r s $.
			      wp $f wff p $.
			      wq $f wff q $.
			      wr $f wff r $.
			      ws $f wff s $.
			      w2 $a wff ( p -> q ) $.
			      wnew $p wff ( s -> ( r -> p ) )
			           $= ws wr wp w2 w2 $.
			      """
			     ) match {
	case bs.Success((ctx, Database(stmts)), _) => stmts
	case other => other
      }
      val wp = Labelled(None, 'wp,VariableType(Con("wff"),Var("p")))
      val wq = Labelled(None, 'wq,VariableType(Con("wff"),Var("q")))
      val wr = Labelled(None, 'wr,VariableType(Con("wff"),Var("r")))
      val ws = Labelled(None, 'ws,VariableType(Con("wff"),Var("s")))
      val w2 = Labelled(None, 'w2,
			Axiom(Con("wff"),
			      List(Con("("),
				   Var("p"), Con("->"), Var("q"),
				   Con(")"))))
      val wnew = Labelled(None, 'wnew,
			   Theorem(Con("wff"),
				   List(Con("("),
					Var("s"), Con("->"), Con("("),
					Var("r"), Con("->"), Var("p"),
					Con(")"), Con(")")),
				   List(ws, wr, wp, w2, w2), None))
      stmts should equal (List(wp, wq, wr, ws, w2, wnew))
    }

    it("""A $f, $e, or $d statement is active from the place it occurs
       until the end of the block it occurs in.  """) (pending)

    it(""" A $a or $p statement is active from the place it occurs
       through the end of the database.  """) (pending)

    it("""There may not be two active $f statements containing the
       same variable.  """) (pending)

    it("""Each variable in a $e, $a, or $p statement must exist in an
       active $f statement.  """) (pending)

    it("""Each label token must be unique.""") (pending)

    it("""No label token may match any math symbol token.""") (pending)

    it("""The set of mandatory variables associated with an assertion
       is the set of (zero or more) variables in the assertion and in
       any active $e statements.  """) (pending)

    it("""The (possibly empty) set of mandatory hypotheses is the set
       of all active $f statements containing mandatory variables,
       together with all active $e statements. """) (pending)

    it("""The set of mandatory $d statements associated with an
       assertion are those active $d statements whose variables are
       both among the assertion’s mandatory variables.  """) (pending)

    it("""A compressed proof, located between $= and $. keywords,
       consists of a left parenthesis, a sequence of statement labels,
       a right parenthesis, and a sequence of upper-case letters A
       through Z (with optional white space between them).  """
     ) {
      val bs = new BasicSyntax()
      val stmts = bs.parseAll(bs.database, """
			      $c |- $.
			      $v ph ps $.
			      ${
				dummylink.1 $e |- ph $.
			        dummylink.2 $e |- ps $.
			      
			        dummylink $p |- ph $=
				  (  ) CC $.
			      $( [7-Feb-2006] $)
			      $}
			      """
			    ) match {
	case bs.Success((ctx, Database(
	  List(Labelled(None, l, Theorem(Con(mark), _, _, _))) )), _) =>
	    (l, mark)
	case other => other
      }
      stmts should equal (
	('dummylink, "|-") )
    }
  }
}
