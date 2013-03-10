package com.github.aztek.idioms

import Idiom._

object Main extends App {
  val foo: Option[Int] = None
  val bar: Option[Int] = Some(100)

  val qux = ${foo + bar}
  println(qux)

  val baz = ${foo + 5 * bar}
  println(baz)
}
