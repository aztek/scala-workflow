package scala.workflow

object Auxiliary extends Auxiliary

trait Auxiliary {
  case class State[A, S](run: S ⇒ (A, S)) {
    def result(s: S) = { val (result, _) = run(s); result }
    def state(s: S)  = { val (_, state)  = run(s); state  }
  }

  case class Writer[R, O : Monoid](result: R, output: O)

  trait Semigroup[A] {
    def append: (A, A) ⇒ A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }
}