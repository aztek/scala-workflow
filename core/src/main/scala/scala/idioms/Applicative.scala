package scala.idioms

import language.higherKinds

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

  def pure[F[_] : Applicative, A](a: A) = implicitly[Applicative[F]].pure[A](a)

  def app[F[_] : Applicative, A, B](f: F[A ⇒ B])(a: F[A]) = implicitly[Applicative[F]].app(f)(a)
}
