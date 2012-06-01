package com.madmode.math

import org.scalacheck.Properties
//import org.scalacheck.Prop.{forAll, &&, ==>}
import org.scalacheck.Prop._


object LexSpec extends Properties("MetaMath symbol") {
  val tp = new TestLexer();
/*
  property("concat") = forAll {
    (c1: Char, c2: Char) =>
      (tp.test_sym(c1.toString()) && tp.test_sym(c2.toString())) ==>
	tp.test_sym(c1.toString() + c2)
  }

*/
}
