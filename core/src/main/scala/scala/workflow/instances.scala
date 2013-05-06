package scala.workflow

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

trait MonadInstances {
  implicit val option = new Monad[Option] {
    def point[A](a: ⇒ A) = Option(a)
    def map[A, B](f: A ⇒ B) = _ map f
    def app[A, B](ff: Option[A ⇒ B]) = for (f ← ff; a ← _) yield f(a)
    def bind[A, B](f: A ⇒ Option[B]) = _ flatMap f
  }

  implicit val list = new Monad[List] {
    def point[A](a: ⇒ A) = List(a)
    def map[A, B](f: A ⇒ B) = _ map f
    def app[A, B](ff: List[A ⇒ B]) = for (f ← ff; a ← _) yield f(a)
    def bind[A, B](f: A ⇒ List[B]) = _ flatMap f
  }

  implicit val set = new Monad[Set] {
    def point[A](a: ⇒ A) = Set(a)
    def map[A, B](f: A ⇒ B) = _ map f
    def app[A, B](ff: Set[A ⇒ B]) = for (f ← ff; a ← _) yield f(a)
    def bind[A, B](f: A ⇒ Set[B]) = _ flatMap f
  }

  implicit val try_ = new Monad[Try] {
    def point[A](a: ⇒ A) = Try(a)
    def map[A, B](f: A ⇒ B) = _ map f
    def app[A, B](ff: Try[A ⇒ B]) = for (f ← ff; a ← _) yield f(a)
    def bind[A, B](f: A ⇒ Try[B]) = _ flatMap f
  }

  implicit val future = new Monad[Future] {
    def point[A](a: ⇒ A) = Future(a)
    def map[A, B](f: A ⇒ B) = _ map f
    def app[A, B](ff: Future[A ⇒ B]) = for (f ← ff; a ← _) yield f(a)
    def bind[A, B](f: A ⇒ Future[B]) = _ flatMap f
  }

  implicit val stream = new Monad[Stream] {
    def point[A](a: ⇒ A) = Stream(a)
    def map[A, B](f: A ⇒ B) = _ map f
    def app[A, B](fs: Stream[A ⇒ B]) = for (f ← fs; a ← _) yield f(a)
    def bind[A, B](f: A ⇒ Stream[B]) = _ flatMap f
  }

  val zipStream = new Monad[Stream] {
    def point[A](a: ⇒ A) = Stream.continually(a)
    def map[A, B](f: A ⇒ B) = _ map f
    def app[A, B](fs: Stream[A ⇒ B]) = _ zip fs map { case (a, f) ⇒ f(a) }
    def bind[A, B](f: A ⇒ Stream[B]) = _ flatMap f
  }

  implicit def left[T] = new Monad[({type λ[α] = Either[α, T]})#λ] {
    def point[A](a: ⇒ A) = Left(a)
    def map[A, B](f: A ⇒ B) = _.left map f
    def app[A, B](ef: Either[A ⇒ B, T]) = ea ⇒ for (f ← ef.left; a ← ea.left) yield f(a)
    def bind[A, B](f: A ⇒ Either[B, T]) = _.left flatMap f
  }

  implicit def right[T] = new Monad[({type λ[α] = Either[T, α]})#λ] {
    def point[A](a: ⇒ A) = Right(a)
    def map[A, B](f: A ⇒ B) = _.right map f
    def app[A, B](ef: Either[T, A ⇒ B]) = ea ⇒ for (f ← ef.right; a ← ea.right) yield f(a)
    def bind[A, B](f: A ⇒ Either[T, B]) = _.right flatMap f
  }

  implicit val id = new Monad[({type λ[α] = α})#λ] {
    def point[A](a: ⇒ A) = a
    def map[A, B](f: A ⇒ B) = f
    def app[A, B](f: A ⇒ B) = f
    def bind[A, B](f: A ⇒ B) = f
  }

  implicit def partialFunction[R] = new Monad[({type λ[α] = PartialFunction[R, α]})#λ] {
    def point[A](a: ⇒ A) = { case _ ⇒ a }
    def map[A, B](f: A ⇒ B) = g ⇒ { case r if g isDefinedAt r ⇒ f(g(r)) }
    def app[A, B](f: PartialFunction[R, A ⇒ B]) = g ⇒ { case r if (g isDefinedAt r) && (f isDefinedAt r) ⇒ f(r)(g(r)) }
    def bind[A, B](f: A ⇒ PartialFunction[R, B]) = g ⇒ { case r if (g isDefinedAt r) && (f(g(r)) isDefinedAt r) ⇒ f(g(r))(r) }
  }

  implicit def function[R] = new Monad[({type λ[α] = R ⇒ α})#λ] {
    def point[A](a: ⇒ A) = _ ⇒ a
    def map[A, B](f: A ⇒ B) = g ⇒ r ⇒ f(g(r))
    def app[A, B](f: R ⇒ A ⇒ B) = g ⇒ r ⇒ f(r)(g(r))
    def bind[A, B](f: A ⇒ R ⇒ B) = g ⇒ r ⇒ f(g(r))(r)
  }

  implicit def function2[R, S] = new Monad[({type λ[α] = R ⇒ S ⇒ α})#λ] {
    def point[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ a
    def map[A, B](f: A ⇒ B) = g ⇒ r ⇒ s ⇒ f(g(r)(s))
    def app[A, B](f: R ⇒ S ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ f(r)(s)(g(r)(s))
    def bind[A, B](f: A ⇒ R ⇒ S ⇒ B) = g ⇒ r ⇒ s ⇒ f(g(r)(s))(r)(s)
  }

  implicit def function3[R, S, T] = new Monad[({type λ[α] = R ⇒ S ⇒ T ⇒ α})#λ] {
    def point[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ a
    def map[A, B](f: A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ f(g(r)(s)(t))
    def app[A, B](f: R ⇒ S ⇒ T ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ f(r)(s)(t)(g(r)(s)(t))
    def bind[A, B](f: A ⇒ R ⇒ S ⇒ T ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ f(g(r)(s)(t))(r)(s)(t)
  }

  implicit def function4[R, S, T, U] = new Monad[({type λ[α] = R ⇒ S ⇒ T ⇒ U ⇒ α})#λ] {
    def point[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ (_: U) ⇒ a
    def map[A, B](f: A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ f(g(r)(s)(t)(u))
    def app[A, B](f: R ⇒ S ⇒ T ⇒ U ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ f(r)(s)(t)(u)(g(r)(s)(t)(u))
    def bind[A, B](f: A ⇒ R ⇒ S ⇒ T ⇒ U ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ f(g(r)(s)(t)(u))(r)(s)(t)(u)
  }

  implicit def function5[R, S, T, U, V] = new Monad[({type λ[α] = R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ α})#λ] {
    def point[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ (_: U) ⇒ (_: V) ⇒ a
    def map[A, B](f: A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ v ⇒ f(g(r)(s)(t)(u)(v))
    def app[A, B](f: R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ v ⇒ f(r)(s)(t)(u)(v)(g(r)(s)(t)(u)(v))
    def bind[A, B](f: A ⇒ R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ v ⇒ f(g(r)(s)(t)(u)(v))(r)(s)(t)(u)(v)
  }

  implicit def function6[R, S, T, U, V, W] = new Monad[({type λ[α] = R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ W ⇒ α})#λ] {
    def point[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ (_: U) ⇒ (_: V) ⇒ (_: W) ⇒ a
    def map[A, B](f: A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ v ⇒ w ⇒ f(g(r)(s)(t)(u)(v)(w))
    def app[A, B](f: R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ W ⇒ A ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ v ⇒ w ⇒ f(r)(s)(t)(u)(v)(w)(g(r)(s)(t)(u)(v)(w))
    def bind[A, B](f: A ⇒ R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ W ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ v ⇒ w ⇒ f(g(r)(s)(t)(u)(v)(w))(r)(s)(t)(u)(v)(w)
  }
}
