package scala.idioms

import language.higherKinds

import concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import util.Try

trait FunctorInstances {
  implicit def tupleL[T] = new Functor[({type λ[α] = (α, T)})#λ] {
    def map[A, B](f: A ⇒ B) = { case (lhs, rhs) ⇒ (f(lhs), rhs) }
  }

  implicit def tupleR[T] = new Functor[({type λ[α] = (T, α)})#λ] {
    def map[A, B](f: A ⇒ B) = { case (lhs, rhs) ⇒ (lhs, f(rhs)) }
  }

  implicit def tuple3L[M, R] = new Functor[({type λ[α] = (α, M, R)})#λ] {
    def map[A, B](f: A ⇒ B) = { case (lhs, mhs, rhs) ⇒ (f(lhs), mhs, rhs) }
  }

  implicit def tuple3M[L, R] = new Functor[({type λ[α] = (L, α, R)})#λ] {
    def map[A, B](f: A ⇒ B) = { case (lhs, mhs, rhs) ⇒ (lhs, f(mhs), rhs) }
  }

  implicit def tuple3R[L, M] = new Functor[({type λ[α] = (L, M, α)})#λ] {
    def map[A, B](f: A ⇒ B) = { case (lhs, mhs, rhs) ⇒ (lhs, mhs, f(rhs)) }
  }

  implicit def map[T] = new Functor[({type λ[α] = Map[T, α]})#λ] {
    def map[A, B](f: A ⇒ B) = _ mapValues f
  }
}

trait SemiIdiomInstances {
  val zipList = new SemiIdiom[List] {
    def map[A, B](f: A ⇒ B) = _ map f
    def app[A, B](fs: List[A ⇒ B]) = _ zip fs map { case (a, f) ⇒ f(a) }
  }
}

trait IdiomInstances {
  implicit val option = new Idiom[Option] {
    def pure[A](a: ⇒ A) = Option(a)
    def map[A, B](f: A ⇒ B) = _ map f
    def app[A, B](ff: Option[A ⇒ B]) = for (f ← ff; a ← _) yield f(a)
  }

  implicit val list = new Idiom[List] {
    def pure[A](a: ⇒ A) = List(a)
    def map[A, B](f: A ⇒ B) = _ map f
    def app[A, B](ff: List[A ⇒ B]) = for (f ← ff; a ← _) yield f(a)
  }

  implicit val set = new Idiom[Set] {
    def pure[A](a: ⇒ A) = Set(a)
    def map[A, B](f: A ⇒ B) = _ map f
    def app[A, B](ff: Set[A ⇒ B]) = for (f ← ff; a ← _) yield f(a)
  }

  implicit val try_ = new Idiom[Try] {
    def pure[A](a: ⇒ A) = Try(a)
    def map[A, B](f: A ⇒ B) = _ map f
    def app[A, B](ff: Try[A ⇒ B]) = for (f ← ff; a ← _) yield f(a)
  }

  implicit val future = new Idiom[Future] {
    def pure[A](a: ⇒ A) = Future(a)
    def map[A, B](f: A ⇒ B) = _ map f
    def app[A, B](ff: Future[A ⇒ B]) = for (f ← ff; a ← _) yield f(a)
  }

  implicit val stream = new Idiom[Stream] {
    def pure[A](a: ⇒ A) = Stream(a)
    def map[A, B](f: A ⇒ B) = _ map f
    def app[A, B](fs: Stream[A ⇒ B]) = for (f ← fs; a ← _) yield f(a)
  }

  val zipStream = new Idiom[Stream] {
    def pure[A](a: ⇒ A) = Stream.continually(a)
    def map[A, B](f: A ⇒ B) = _ map f
    def app[A, B](fs: Stream[A ⇒ B]) = _ zip fs map { case (a, f) ⇒ f(a) }
  }

  implicit def left[T] = new Idiom[({type λ[α] = Either[α, T]})#λ] {
    def pure[A](a: ⇒ A) = Left(a)
    def map[A, B](f: A ⇒ B) = _.left map f
    def app[A, B](ef: Either[A ⇒ B, T]) = ea ⇒ for (f ← ef.left; a ← ea.left) yield f(a)
  }

  implicit def right[T] = new Idiom[({type λ[α] = Either[T, α]})#λ] {
    def pure[A](a: ⇒ A) = Right(a)
    def map[A, B](f: A ⇒ B) = _.right map f
    def app[A, B](ef: Either[T, A ⇒ B]) = ea ⇒ for (f ← ef.right; a ← ea.right) yield f(a)
  }

  implicit val id = new Idiom[({type λ[α] = α})#λ] {
    def pure[A](a: ⇒ A) = a
    def map[A, B](f: A ⇒ B) = f
    def app[A, B](f: A ⇒ B) = f
  }

  implicit def partialFunction[R] = new Idiom[({type λ[α] = PartialFunction[R, α]})#λ] {
    def pure[A](a: ⇒ A) = { case _ ⇒ a }
    def map[A, B](f: A ⇒ B) = g ⇒ { case r if g isDefinedAt r ⇒ f(g(r)) }
    def app[A, B](f: PartialFunction[R, A ⇒ B]) = g ⇒ { case r if f isDefinedAt r ⇒ f(r)(g(r)) }
  }

  implicit def function[R] = new Idiom[({type λ[α] = R ⇒ α})#λ] {
    def pure[A](a: ⇒ A) = _ ⇒ a
    def map[A, B](f: A ⇒ B) = g ⇒ r ⇒ f(g(r))
    def app[A, B](f: R ⇒ A ⇒ B) = g ⇒ t ⇒ f(t)(g(t))
  }

  implicit def function2[R, S] = new Idiom[({type λ[α] = R ⇒ S ⇒ α})#λ] {
    def pure[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ a
    def map[A, B](f: A ⇒ B) = g ⇒ r ⇒ s ⇒ f(g(r)(s))
    def app[A, B](f: R ⇒ S ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ f(r)(s)(g(r)(s))
  }

  implicit def function3[R, S, T] = new Idiom[({type λ[α] = R ⇒ S ⇒ T ⇒ α})#λ] {
    def pure[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ a
    def map[A, B](f: A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ f(g(r)(s)(t))
    def app[A, B](f: R ⇒ S ⇒ T ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ f(r)(s)(t)(g(r)(s)(t))
  }

  implicit def function4[R, S, T, U] = new Idiom[({type λ[α] = R ⇒ S ⇒ T ⇒ U ⇒ α})#λ] {
    def pure[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ (_: U) ⇒ a
    def map[A, B](f: A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ f(g(r)(s)(t)(u))
    def app[A, B](f: R ⇒ S ⇒ T ⇒ U ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ f(r)(s)(t)(u)(g(r)(s)(t)(u))
  }

  implicit def function5[R, S, T, U, V] = new Idiom[({type λ[α] = R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ α})#λ] {
    def pure[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ (_: U) ⇒ (_: V) ⇒ a
    def map[A, B](f: A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ v ⇒ f(g(r)(s)(t)(u)(v))
    def app[A, B](f: R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ v ⇒ f(r)(s)(t)(u)(v)(g(r)(s)(t)(u)(v))
  }

  implicit def function6[R, S, T, U, V, W] = new Idiom[({type λ[α] = R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ W ⇒ α})#λ] {
    def pure[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ (_: U) ⇒ (_: V) ⇒ (_: W) ⇒ a
    def map[A, B](f: A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ v ⇒ w ⇒ f(g(r)(s)(t)(u)(v)(w))
    def app[A, B](f: R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ W ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ v ⇒ w ⇒ f(r)(s)(t)(u)(v)(w)(g(r)(s)(t)(u)(v)(w))
  }
}
