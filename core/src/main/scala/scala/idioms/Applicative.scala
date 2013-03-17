package scala.idioms

import language.higherKinds
import util.Try

trait Applicative[F[_]] {
  def pure[T](t: ⇒ T): F[T]
  def app[A, B](f: F[A ⇒ B]): F[A] ⇒ F[B]
}

object Applicative {
  implicit val option = new Applicative[Option] {
    def pure[A](a: ⇒ A) = Option(a)
    def app[A, B](of: Option[A ⇒ B]) = oa ⇒ for (f ← of; a ← oa) yield f(a)
  }

  implicit val list = new Applicative[List] {
    def pure[A](a: ⇒ A) = List(a)
    def app[A, B](fs: List[A ⇒ B]) = as ⇒ for (f ← fs; a ← as) yield f(a)
  }

  implicit val try_ = new Applicative[Try] {
    def pure[A](a: ⇒ A) = Try(a)
    def app[A, B](ff: Try[A ⇒ B]) = aa ⇒ for (f ← ff; a ← aa) yield f(a)
  }

  implicit def left[T] = {
    type `Either[_, T]`[A] = Either[A, T]
    new Applicative[`Either[_, T]`] {
      def pure[A](a: ⇒ A) = Left(a)
      def app[A, B](ef: Either[A ⇒ B, T]) = ea ⇒ for (f ← ef.left; a ← ea.left) yield f(a)
    }
  }

  implicit def right[T] = {
    type `Either[T, _]`[A] = Either[T, A]
    new Applicative[`Either[T, _]`] {
      def pure[A](a: ⇒ A) = Right(a)
      def app[A, B](ef: Either[T, A ⇒ B]) = ea ⇒ for (f ← ef.right; a ← ea.right) yield f(a)
    }
  }

  implicit def function[T] = {
    type `T ⇒ _`[A] = T ⇒ A
    new Applicative[`T ⇒ _`] {
      def pure[A](a: ⇒ A) = (_: T) ⇒ a
      def app[A, B](f: T ⇒ A ⇒ B) = g ⇒ t ⇒ f(t)(g(t))
    }
  }

  def pure[F[_] : Applicative, A](a: ⇒ A) = implicitly[Applicative[F]].pure[A](a)

  def app[F[_] : Applicative, A, B](f: F[A ⇒ B])(a: F[A]) = implicitly[Applicative[F]].app(f)(a)
}
