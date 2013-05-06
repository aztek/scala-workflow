package scala.workflow

import language.higherKinds

trait Workflow[F[_]]

trait Mapping[F[_]] { self: Workflow[F] ⇒
  def map[A, B](f: A ⇒ B): F[A] ⇒ F[B]
}

trait Applying[F[_]] { self: Workflow[F] ⇒
  def app[A, B](f: F[A ⇒ B]): F[A] ⇒ F[B]
}

trait Pointing[F[_]] { self: Workflow[F] ⇒
  def point[A](a: ⇒ A): F[A]
}

trait Binding[F[_]] { self: Workflow[F] ⇒
  def bind[A, B](f: A ⇒ F[B]): F[A] ⇒ F[B]
}

trait Functor[F[_]] extends Workflow[F] with Mapping[F] {
  def $ [G[_]](g: Functor[G]) = new FunctorT(this, g)
}

object Functor extends FunctorInstances

trait SemiIdiom[F[_]] extends Functor[F] with Applying[F] {
  def $ [G[_]](g: SemiIdiom[G]) = new SemiIdiomT(this, g)
}

object SemiIdiom extends SemiIdiomInstances

trait Idiom[F[_]] extends SemiIdiom[F] with Pointing[F] {
  def $ [G[_]](g: Idiom[G]) = new IdiomT(this, g)
}

object Idiom extends IdiomInstances

trait SemiMonad[F[_]] extends SemiIdiom[F] with Binding[F]

trait Monad[F[_]] extends Idiom[F] with Binding[F]