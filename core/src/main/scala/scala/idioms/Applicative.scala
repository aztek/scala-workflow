package scala.idioms

import language.higherKinds
import util.Try

trait Applicative[F[_]] {
  def pure[T](t: T): F[T]
  def app[A, B](f: F[A ⇒ B]): F[A] ⇒ F[B]
}

object Applicative {
  implicit val option = new Applicative[Option] {
    def pure[A](a: A) = Some(a)
    def app[A, B](optF: Option[A ⇒ B]) = optA ⇒ for (f ← optF; a ← optA) yield f(a)
  }

  implicit val list = new Applicative[List] {
    def pure[A](a: A) = List(a)
    def app[A, B](fs: List[A ⇒ B]) = as ⇒ for (f ← fs; a ← as) yield f(a)
  }

  implicit val try_ = new Applicative[Try] {
    def pure[A](a: A) = Try(a)
    def app[A, B](ff: Try[A ⇒ B]) = aa ⇒ for (f ← ff; a ← aa) yield f(a)
  }

  implicit def function[T] = {
    type `T ⇒ _`[A] = T ⇒ A
    new Applicative[`T ⇒ _`] {
      def pure[A](a: A) = (_: T) ⇒ a
      def app[A, B](f: T ⇒ A ⇒ B) = (g: T ⇒ A) ⇒ (t: T) ⇒ f(t)(g(t))
    }
  }

  def pure[F[_] : Applicative, A](a: A) = implicitly[Applicative[F]].pure[A](a)

  def app[F[_] : Applicative, A, B](f: F[A ⇒ B])(a: F[A]) = implicitly[Applicative[F]].app(f)(a)
}
