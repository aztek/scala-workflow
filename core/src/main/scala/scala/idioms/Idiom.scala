package scala.idioms

import concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import language.higherKinds
import util.Try

trait Idiom[F[_]] {
  def pure[T](t: ⇒ T): F[T]
  def app[A, B](f: F[A ⇒ B]): F[A] ⇒ F[B]

  def $ [G[_]](g: Idiom[G]): Idiom[({type λ[α] = F[G[α]]})#λ] = new IdiomT(this, g)
}

/** Idioms transformer
  * Idioms are know to be composable. IdiomT provides an implementation
  * of Idiom[ F [ G ] ] by given Idiom[F] and Idiom[G].
  */
class IdiomT[F[_], G[_]](f: Idiom[F], g: Idiom[G]) extends Idiom[({type λ[α] = F[G[α]]})#λ] {
  def pure[T](t: ⇒ T) = f.pure(g.pure(t))
  def app[A, B](h: F[G[A ⇒ B]]) = f.app((f.app[G[A ⇒ B], G[A] ⇒ G[B]](f.pure(g.app _)))(h))
}

object Idiom {
  implicit val option = new Idiom[Option] {
    def pure[A](a: ⇒ A) = Option(a)
    def app[A, B](ff: Option[A ⇒ B]) = for (f ← ff; a ← _) yield f(a)
  }

  implicit val list = new Idiom[List] {
    def pure[A](a: ⇒ A) = List(a)
    def app[A, B](ff: List[A ⇒ B]) = for (f ← ff; a ← _) yield f(a)
  }

  implicit val set = new Idiom[Set] {
    def pure[A](a: ⇒ A) = Set(a)
    def app[A, B](ff: Set[A ⇒ B]) = for (f ← ff; a ← _) yield f(a)
  }

  implicit val try_ = new Idiom[Try] {
    def pure[A](a: ⇒ A) = Try(a)
    def app[A, B](ff: Try[A ⇒ B]) = for (f ← ff; a ← _) yield f(a)
  }

  implicit val future = new Idiom[Future] {
    def pure[A](a: ⇒ A) = Future(a)
    def app[A, B](ff: Future[A ⇒ B]) = for (f ← ff; a ← _) yield f(a)
  }

  implicit def left[T] = new Idiom[({type λ[α] = Either[α, T]})#λ] {
    def pure[A](a: ⇒ A) = Left(a)
    def app[A, B](ef: Either[A ⇒ B, T]) = ea ⇒ for (f ← ef.left; a ← ea.left) yield f(a)
  }

  implicit def right[T] = new Idiom[({type λ[α] = Either[T, α]})#λ] {
    def pure[A](a: ⇒ A) = Right(a)
    def app[A, B](ef: Either[T, A ⇒ B]) = ea ⇒ for (f ← ef.right; a ← ea.right) yield f(a)
  }

  implicit def partialFunction[R] = new Idiom[({type λ[α] = PartialFunction[R, α]})#λ] {
    def pure[A](a: ⇒ A) = { case _ ⇒ a }
    def app[A, B](f: PartialFunction[R, A ⇒ B]) = g ⇒ { case t ⇒ f(t)(g(t)) }
  }

  implicit def function[R] = new Idiom[({type λ[α] = R ⇒ α})#λ] {
    def pure[A](a: ⇒ A) = (_: R) ⇒ a
    def app[A, B](f: R ⇒ A ⇒ B) = g ⇒ t ⇒ f(t)(g(t))
  }

  implicit def function2[R, S] = new Idiom[({type λ[α] = R ⇒ S ⇒ α})#λ] {
    def pure[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ a
    def app[A, B](f: R ⇒ S ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ f(r)(s)(g(r)(s))
  }

  implicit def function3[R, S, T] = new Idiom[({type λ[α] = R ⇒ S ⇒ T ⇒ α})#λ] {
    def pure[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ a
    def app[A, B](f: R ⇒ S ⇒ T ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ f(r)(s)(t)(g(r)(s)(t))
  }

  implicit def function4[R, S, T, U] = new Idiom[({type λ[α] = R ⇒ S ⇒ T ⇒ U ⇒ α})#λ] {
    def pure[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ (_: U) ⇒ a
    def app[A, B](f: R ⇒ S ⇒ T ⇒ U ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ f(r)(s)(t)(u)(g(r)(s)(t)(u))
  }

  implicit def function5[R, S, T, U, V] = new Idiom[({type λ[α] = R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ α})#λ] {
    def pure[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ (_: U) ⇒ (_: V) ⇒ a
    def app[A, B](f: R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ v ⇒ f(r)(s)(t)(u)(v)(g(r)(s)(t)(u)(v))
  }

  implicit def function6[R, S, T, U, V, W] = new Idiom[({type λ[α] = R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ W ⇒ α})#λ] {
    def pure[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ (_: U) ⇒ (_: V) ⇒ (_: W) ⇒ a
    def app[A, B](f: R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ W ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ v ⇒ w ⇒ f(r)(s)(t)(u)(v)(w)(g(r)(s)(t)(u)(v)(w))
  }
}
