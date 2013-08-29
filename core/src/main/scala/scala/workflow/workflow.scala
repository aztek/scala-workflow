package scala.workflow

import language.higherKinds

trait Workflow[F[_]]

trait Mapping[F[_]] extends Workflow[F] {
  def map[A, B](f: A ⇒ B): F[A] ⇒ F[B]
}

trait Applying[F[_]] extends Workflow[F] with Mapping[F] {
  def app[A, B](f: F[A ⇒ B]): F[A] ⇒ F[B]
}

trait Pointing[F[_]] extends Workflow[F] {
  def point[A](a: ⇒ A): F[A]
}

trait Binding[F[_]] extends Workflow[F] {
  def bind[A, B](f: A ⇒ F[B]): F[A] ⇒ F[B]
}

trait Functor[F[_]] extends Mapping[F] with FunctorComposition[F] {
  def $ [G[_]](g: FunctorComposition[G]) = g & this
}

object Functor extends FunctorInstances

trait SemiIdiom[F[_]] extends Functor[F] with Applying[F] with SemiIdiomComposition[F] {
  def $ [G[_]](g: SemiIdiomComposition[G]) = g & this
}

object SemiIdiom extends SemiIdiomInstances

trait Idiom[F[_]] extends SemiIdiom[F] with Pointing[F] with IdiomComposition[F] {
  def map[A, B](f: A ⇒ B) = app(point(f))
  def $ [G[_]](g: IdiomComposition[G]) = g & this
}

object Idiom extends IdiomInstances

trait SemiMonad[F[_]] extends SemiIdiom[F] with Binding[F]

trait Monad[F[_]] extends Idiom[F] with Binding[F] {
  def app[A, B](f: F[A ⇒ B]) = bind(a ⇒ bind((g: A ⇒ B) ⇒ point(g(a)))(f))
  def $ [G[_]](g: MonadComposition[G]) = g & this
}

object Monad extends MonadInstances