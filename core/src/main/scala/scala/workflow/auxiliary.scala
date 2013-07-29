package scala.workflow

object Auxiliary extends Auxiliary

trait Auxiliary {
  case class State[A, S](run: S ⇒ (A, S)) {
    def result(s: S) = { val (result, _) = run(s); result }
    def state(s: S)  = { val (_, state)  = run(s); state  }
  }

  case class Writer[R, O](result: R, output: O)

  trait SemiGroup[A] {
    def append: (A, A) ⇒ A
  }

  trait Monoid[A] extends SemiGroup[A] {
    def empty: A
  }
}