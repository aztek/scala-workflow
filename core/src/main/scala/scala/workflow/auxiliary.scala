package scala.workflow

object Auxiliary extends Auxiliary

trait Auxiliary {
  case class State[A, S](run: S ⇒ (A, S)) {
    def result(s: S) = { val (result, _) = run(s); result }
    def state(s: S)  = { val (_, state)  = run(s); state  }
  }

  case class Cont[R, A](run: (A ⇒ R) ⇒ R)

  trait Semigroup[A] {
    def append: (A, A) ⇒ A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  case class Writer[R, O : Monoid](result: R, output: O)
}