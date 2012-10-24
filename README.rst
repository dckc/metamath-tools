metamath-tools -- toward a metamath parser, proof checker in scala
==================================================================

Metamath__, by Norman Megill, is "a tiny language that can express
theorems in abstract mathematics, accompanied by proofs that can be
verified by a computer program."

__ http://us.metamath.org/index.html

The language is accompanied by a proof checker and a growing database
of thousands of proved theorems covering conventional results in
logic, set theory, number theory, group theory, algebra, analysis, and
topology.

I don't recall exactly what motivated me to start a scala implementation.
One idea:

  - proof assistant using angular.js

Performance of the scala implementation was pretty bad; as I recall,
it took about 20 minutes to parse what the original C version parses
in a few seconds.

I'm picking up the project again since discovering rust__.

__ http://www.rust-lang.org/

background/context:

 - `Saying Goodbye to Moore Method math notes and Robert Miner`__
   by Dan C at Saturday, February 18, 2012
 - `Map and Territory in RDF APIs`__
   by connolly 27 Apr 2010
 - `Fun and Frustration with Scala`__
   by connolly 18 Jan 2010
 

__ http://www.madmode.com/2012/02/moore-method-wikipedia-free.html
__ http://dig.csail.mit.edu/breadcrumbs/node/253
__ http://www.advogato.org/person/connolly/diary/71.html

Dev Tools, Reference
--------------------

  - sbt
  - `Scala Standard Library`__
  - ScalaTest__

__ http://www.scala-lang.org/api/current/index.html
__ http://www.scalatest.org/

Origins: Typesafe Stack scaffold
--------------------------------

.. note:: TODO: find out where this came from

A sample for the Typesafe Stack (http://typesafe.com/stack).

Scala sample project using Scala and SBT.

To run and test it use SBT invoke: 'sbt run'
