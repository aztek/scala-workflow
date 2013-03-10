package scala.idioms

object Main extends App {
  val foo: Option[Int] = Some(40)
  val bar: Option[Int] = Some(3)

  val qux = ${foo + bar}
  println(qux)

  val baz = ${foo + 5 * bar}
  println(baz)

  val xyz = ${4 * foo - bar}
  println(xyz)
}
