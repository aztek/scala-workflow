package com.github.aztek.idioms

import Idiom._

object Main extends App {
  val plus = ((_: String) + (_: String)).curried

  val foo = Some("foo")
  val bar = Some("bar")

  val qux = ${plus(foo)(bar)}

  println(qux)
}
