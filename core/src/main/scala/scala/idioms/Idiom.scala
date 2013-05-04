package scala.idioms

import language.higherKinds

trait Functor[F[_]] {
  def map[A, B](f: A ⇒ B): F[A] ⇒ F[B]
  def $ [G[_]](g: Functor[G]) = new FunctorT(this, g)
}

object Functor extends FunctorInstances

trait Pointed[F[_]] extends Functor[F] {
  def pure[A](a: ⇒ A): F[A]
  def $ [G[_]](g: Pointed[G]) = new PointedT(this, g)
}

trait SemiIdiom[F[_]] extends Functor[F] {
  def app[A, B](f: F[A ⇒ B]): F[A] ⇒ F[B]
  def $ [G[_]](g: SemiIdiom[G]) = new SemiIdiomT(this, g)
}

object SemiIdiom extends SemiIdiomInstances

trait Idiom[F[_]] extends SemiIdiom[F] with Pointed[F] {
  def $ [G[_]](g: Idiom[G]) = new IdiomT(this, g)
}

object Idiom extends IdiomInstances
