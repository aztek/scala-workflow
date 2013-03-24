package scala.idioms

import language.higherKinds
import util.Try
import concurrent.Future
import concurrent.ExecutionContext.Implicits.global

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

  implicit val set = new Applicative[Set] {
    def pure[A](a: ⇒ A) = Set(a)
    def app[A, B](fs: Set[A ⇒ B]) = as ⇒ for (f ← fs; a ← as) yield f(a)
  }

  implicit val try_ = new Applicative[Try] {
    def pure[A](a: ⇒ A) = Try(a)
    def app[A, B](ff: Try[A ⇒ B]) = aa ⇒ for (f ← ff; a ← aa) yield f(a)
  }

  implicit val future = new Applicative[Future] {
    def pure[A](a: ⇒ A) = Future(a)
    def app[A, B](ff: Future[A ⇒ B]) = fa ⇒ for (f ← ff; a ← fa) yield f(a)
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

  implicit def function1[R] = {
    type `R ⇒ _`[A] = R ⇒ A
    new Applicative[`R ⇒ _`] {
      def pure[A](a: ⇒ A) = (_: R) ⇒ a
      def app[A, B](f: R ⇒ A ⇒ B) = g ⇒ t ⇒ f(t)(g(t))
    }
  }

  implicit def function2[R, S] = {
    type `R ⇒ S ⇒ _`[A] = R ⇒ S ⇒ A
    new Applicative[`R ⇒ S ⇒ _`] {
      def pure[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ a
      def app[A, B](f: R ⇒ S ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ f(r)(s)(g(r)(s))
    }
  }

  implicit def function3[R, S, T] = {
    type `R ⇒ S ⇒ T ⇒ _`[A] = R ⇒ S ⇒ T ⇒ A
    new Applicative[`R ⇒ S ⇒ T ⇒ _`] {
      def pure[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ a
      def app[A, B](f: R ⇒ S ⇒ T ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ f(r)(s)(t)(g(r)(s)(t))
    }
  }

  implicit def function4[R, S, T, U] = {
    type `R ⇒ S ⇒ T ⇒ U ⇒ _`[A] = R ⇒ S ⇒ T ⇒ U ⇒ A
    new Applicative[`R ⇒ S ⇒ T ⇒ U ⇒ _`] {
      def pure[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ (_: U) ⇒ a
      def app[A, B](f: R ⇒ S ⇒ T ⇒ U ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ f(r)(s)(t)(u)(g(r)(s)(t)(u))
    }
  }

  implicit def function5[R, S, T, U, V] = {
    type `R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ _`[A] = R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ A
    new Applicative[`R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ _`] {
      def pure[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ (_: U) ⇒ (_: V) ⇒ a
      def app[A, B](f: R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ v ⇒ f(r)(s)(t)(u)(v)(g(r)(s)(t)(u)(v))
    }
  }

  implicit def function6[R, S, T, U, V, W] = {
    type `R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ W ⇒ _`[A] = R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ W ⇒ A
    new Applicative[`R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ W ⇒ _`] {
      def pure[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ (_: U) ⇒ (_: V) ⇒ (_: W) ⇒ a
      def app[A, B](f: R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ W ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ v ⇒ w ⇒ f(r)(s)(t)(u)(v)(w)(g(r)(s)(t)(u)(v)(w))
    }
  }
}
