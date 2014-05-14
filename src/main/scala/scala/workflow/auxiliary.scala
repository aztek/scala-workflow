package scala.workflow

trait Semigroup[A] {
  def append: (A, A) ⇒ A
}

object Semigroup extends SemigroupInstances

trait Monoid[A] extends Semigroup[A] {
  val unit: A
}

object Monoid extends MonoidInstances {
  def apply[A](u: A, ap: (A, A) => A) = new Monoid[A] {
    val unit = u
    def append = ap
  }
}
