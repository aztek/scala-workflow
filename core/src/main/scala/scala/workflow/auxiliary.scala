package scala.workflow

object Auxiliary extends Auxiliary

trait Auxiliary {
  case class State[A, S](run: S ⇒ (A, S)) {
    def result(s: S) = { val (result, _) = run(s); result }
    def state(s: S)  = { val (_, state)  = run(s); state  }
  }

  trait Semigroup[A] {
    def append: (A, A) ⇒ A
  }

  object Semigroup extends SemigroupInstances

  trait Monoid[A] extends Semigroup[A] {
    val unit: A
  }

  object Monoid extends MonoidInstances
}