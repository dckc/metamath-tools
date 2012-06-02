package com.madmode.math

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class TestPreliminaries extends Spec with ShouldMatchers {
  describe("4.1.1 Preliminaries") {
    val p = new Preprocessing()

    it("""The only characters that are allowed to appear in a Metamath
source file are the 94 printable characters on standard ascii keyboards ...
plus the following non-printable (white space) characters: space, tab, car-
riage return, line feed, and form feed.""") {

      val s = p.parseAll(p.label, "$( \u0000 is not allowed $) label") match {
	case p.Success(txt, _) => txt
	/* TODO: better diagnostic for bad character. */
	case p.NoSuccess(failure, _) => "BAD CHAR"
      }
      s should equal ("BAD CHAR")
    }

    it("""A Metamath database consists of a sequence of three kinds of tokens
       separated by white space.""") {
      val parts = p.parseAll(p.tokens, "axiom.1 $a |- x = x $.") match {
	case p.Success(result, _) => result
	case p.NoSuccess(failure, _) => failure
      }
      parts should equal (List("axiom.1", "$a", "|-", "x", "=", "x", "$."))
    }

    it("""The set of keyword tokens is ${, $}, $c, $v, $f, $e, $d, $a,
       $p, $., $=, $(, $), $[, and $]. The last four are called
       auxiliary or preprocessing keywords. """) {
      val parts = p.parseAll(p.tokens,
			      "${ $} $c $v $f $e $d $a $p $. $=") match {
	case p.Success(result, _) => result
	case p.NoSuccess(failure, _) => failure
      }
      parts should equal (List("${", "$}", "$c", "$v", "$f", "$e",
			       "$d", "$a", "$p", "$.", "$="))
    }

    it("""A label token consists of any combination of
letters, digits, and the characters hyphen, underscore, and period.""") {
      val l = p.parseAll(p.label, "letters09-_.") match {
	case p.Success(txt, _) => txt
	case p.NoSuccess(failure, _) => failure
      }
      l should equal ("letters09-_.")

      val notl = p.parseAll(p.label, "letter$") match {
	case p.Success(txt, _) => txt
	case p.NoSuccess(failure, _) => "NOT LABEL"
      }
      notl should equal ("NOT LABEL")
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
}

class TestPreprocessing extends Spec with ShouldMatchers {
  describe("4.1.2 Preprocessing") {
    val p = new Preprocessing()

    it("""Comments are ignored (treated like white space) for the
       purpose of parsing."""  ) {
      val parts1 = p.parseAll(p.tokens,
			     "$( turnstile $)") match {
	case p.Success(result, _) => result
	case failure => failure
      }
      parts1 should equal (List())

      val parts = p.parseAll(p.tokens,
			     "axiom.1 $a $( turnstile $) |- x = x $.") match {
	case p.Success(result, _) => result
	case p.NoSuccess(failure, _) => failure
      }
      parts should equal (List("axiom.1", "$a", "|-", "x", "=", "x", "$."))
    }

    it("The contents of the file replace the inclusion command.") (pending)

  }
}


class TestBasicSyntax extends Spec with ShouldMatchers {
  describe("4.1.3 Basic Syntax") {
    it("""After preprocessing, a database will consist of a sequence
       of statements.  """) {
      val bs = new BasicSyntax()
      bs.ctx.constants = List("|-", "=")
      bs.ctx.variables = List("x")
      val db = bs.parseAll(bs.statements, "axiom.1 $a |- x = x $.") match {
	case bs.Success(db, _) => db
	case other => other
      }
      db should equal (
	Database(List(Statement("axiom.1",
				Axiom(Con("|-"),
				      List(Var("x"), Con("="), Var("x")))))) )
    }

    it("${ begins a block and a matching $} ends the block.") (pending)

    it("""These statements declare the math symbols to be variables
       or constants respectively.""") {
      val bs = new BasicSyntax()
      val ctx = bs.parseAll(bs.database, """
			    $c wff set |- ( ) -> $.
			    $v P Q x $.""") match {
	case bs.Success((ctx, Database(List())), _) => 
	  (ctx.constants, ctx.variables)
	case other => other
      }
      ctx should equal ( (List("wff", "set", "|-", "(", ")", "->"),
			  List("P", "Q", "x") ))
    }

    it("""A math symbol becomes active when declared and stays active
       until the end of the block in which it is declared.""") (pending)

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
	List(Statement("stmt1", VariableType(Con("wff"), Var("P"))),
	     Statement("stmt2", VariableType(Con("set"), Var("x"))),
	     Statement("stmt3", Logical(Con("|-"),
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
      val wp = Statement("wp",VariableType(Con("wff"),Var("p")))
      val wq = Statement("wq",VariableType(Con("wff"),Var("q")))
      val wr = Statement("wr",VariableType(Con("wff"),Var("r")))
      val ws = Statement("ws",VariableType(Con("wff"),Var("s")))
      val w2 = Statement("w2",
			 Axiom(Con("wff"),
			       List(Con("("),
				    Var("p"), Con("->"), Var("q"),
				    Con(")"))))
      val wnew = Statement("wnew",
			   Theorem(Con("wff"),
				   List(Con("("),
					Var("s"), Con("->"), Con("("),
					Var("r"), Con("->"), Var("p"),
					Con(")"), Con(")")),
				   List(ws, wr, wp, w2, w2)))
      stmts should equal (List(wp, wq, wr, ws, w2, wnew))

    }

  }
}
