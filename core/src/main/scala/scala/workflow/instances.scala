package scala.workflow

import language.higherKinds

import concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import util.Try

trait FunctorInstances {
  implicit def tupleL[T] = new Functor[({type λ[α] = (α, T)})#λ] {
    def map[A, B](f: A ⇒ B) = { case (lhs, rhs) ⇒ (f(lhs), rhs) }
  }

  def tuple[T] = tupleL[T]

  implicit def tupleR[T] = new Functor[({type λ[α] = (T, α)})#λ] {
    def map[A, B](f: A ⇒ B) = { case (lhs, rhs) ⇒ (lhs, f(rhs)) }
  }

  def pair[T] = tupleR[T]

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
  val zipStream = new Idiom[Stream] {
    def point[A](a: ⇒ A) = Stream.continually(a)
    def app[A, B](fs: Stream[A ⇒ B]) = _ zip fs map { case (a, f) ⇒ f(a) }
  }
}

trait MonadInstances extends Auxiliary {
  implicit val option = new Monad[Option] with MonadComposition[Option] {
    def point[A](a: ⇒ A) = Option(a)
    def bind[A, B](f: A ⇒ Option[B]) = _ flatMap f
    def & [G[_]](g: Monad[G]) = new Monad[({type λ[α] = G[Option[α]]})#λ] {
      def point[A](a: ⇒ A) = g.point(Option(a))
      def bind[A, B](f: A ⇒ G[Option[B]]) = g.bind {
        case Some(a) ⇒ f(a)
        case None    ⇒ g.point(None)
      }
    }
  }

  implicit val list = new Monad[List] with MonadComposition[List] {
    def point[A](a: ⇒ A) = List(a)
    def bind[A, B](f: A ⇒ List[B]) = _ flatMap f
    def & [G[_]](g: Monad[G]) = new Monad[({type λ[α] = G[List[α]]})#λ] {
      def point[A](a: ⇒ A) = g.point(List(a))
      def bind[A, B](f: A ⇒ G[List[B]]) = g.bind {
        _ map f reduce {
          (a, b) ⇒ g.app(g.map((x: List[B]) ⇒ (y: List[B]) ⇒ x ++ y)(a))(b)
        }
      }
    }
  }

  implicit val set = new Monad[Set] {
    def point[A](a: ⇒ A) = Set(a)
    def bind[A, B](f: A ⇒ Set[B]) = _ flatMap f
  }

  implicit val try_ = new Monad[Try] {
    def point[A](a: ⇒ A) = Try(a)
    def bind[A, B](f: A ⇒ Try[B]) = _ flatMap f
  }

  implicit val future = new Monad[Future] {
    def point[A](a: ⇒ A) = Future(a)
    def bind[A, B](f: A ⇒ Future[B]) = _ flatMap f
  }

  implicit val stream = new Monad[Stream] {
    def point[A](a: ⇒ A) = Stream(a)
    def bind[A, B](f: A ⇒ Stream[B]) = _ flatMap f
  }

  implicit def left[T] = new Monad[({type λ[α] = Either[α, T]})#λ] with MonadComposition[({type λ[α] = Either[α, T]})#λ] {
    def point[A](a: ⇒ A) = Left(a)
    def bind[A, B](f: A ⇒ Either[B, T]) = _.left flatMap f
    def & [G[_]](g: Monad[G]) = new Monad[({type λ[α] = G[Either[α, T]]})#λ] {
      def point[A](a: ⇒ A) = g.point(Left(a))
      def bind[A, B](f: A ⇒ G[Either[B, T]]) = g.bind {
        case Left(a)  ⇒ f(a)
        case Right(t) ⇒ g.point(Right(t))
      }
    }
  }

  implicit def right[T] = new Monad[({type λ[α] = Either[T, α]})#λ] with MonadComposition[({type λ[α] = Either[T, α]})#λ] {
    def point[A](a: ⇒ A) = Right(a)
    def bind[A, B](f: A ⇒ Either[T, B]) = _.right flatMap f
    def & [G[_]](g: Monad[G]) = new Monad[({type λ[α] = G[Either[T, α]]})#λ] {
      def point[A](a: ⇒ A) = g.point(Right(a))
      def bind[A, B](f: A ⇒ G[Either[T, B]]) = g.bind {
        case Left(t)  ⇒ g.point(Left(t))
        case Right(a) ⇒ f(a)
      }
    }
  }

  def either[T] = right[T]

  implicit val id = new Monad[({type λ[α] = α})#λ] with MonadComposition[({type λ[α] = α})#λ] {
    def point[A](a: ⇒ A) = a
    def bind[A, B](f: A ⇒ B) = f
    def & [G[_]](g: Monad[G]) = g
  }

  implicit def partialFunction[R] = new Monad[({type λ[α] = PartialFunction[R, α]})#λ] {
    def point[A](a: ⇒ A) = { case _ ⇒ a }
    def bind[A, B](f: A ⇒ PartialFunction[R, B]) = g ⇒ { case r if (g isDefinedAt r) && (f(g(r)) isDefinedAt r) ⇒ f(g(r))(r) }
  }

  implicit def function[R] = new Monad[({type λ[α] = R ⇒ α})#λ] {
    def point[A](a: ⇒ A) = _ ⇒ a
    def bind[A, B](f: A ⇒ R ⇒ B) = g ⇒ r ⇒ f(g(r))(r)
  }

  implicit def function2[R, S] = new Monad[({type λ[α] = R ⇒ S ⇒ α})#λ] {
    def point[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ a
    def bind[A, B](f: A ⇒ R ⇒ S ⇒ B) = g ⇒ r ⇒ s ⇒ f(g(r)(s))(r)(s)
  }

  implicit def function3[R, S, T] = new Monad[({type λ[α] = R ⇒ S ⇒ T ⇒ α})#λ] {
    def point[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ a
    def bind[A, B](f: A ⇒ R ⇒ S ⇒ T ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ f(g(r)(s)(t))(r)(s)(t)
  }

  implicit def function4[R, S, T, U] = new Monad[({type λ[α] = R ⇒ S ⇒ T ⇒ U ⇒ α})#λ] {
    def point[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ (_: U) ⇒ a
    def bind[A, B](f: A ⇒ R ⇒ S ⇒ T ⇒ U ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ f(g(r)(s)(t)(u))(r)(s)(t)(u)
  }

  implicit def function5[R, S, T, U, V] = new Monad[({type λ[α] = R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ α})#λ] {
    def point[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ (_: U) ⇒ (_: V) ⇒ a
    def bind[A, B](f: A ⇒ R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ v ⇒ f(g(r)(s)(t)(u)(v))(r)(s)(t)(u)(v)
  }

  implicit def function6[R, S, T, U, V, W] = new Monad[({type λ[α] = R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ W ⇒ α})#λ] {
    def point[A](a: ⇒ A) = (_: R) ⇒ (_: S) ⇒ (_: T) ⇒ (_: U) ⇒ (_: V) ⇒ (_: W) ⇒ a
    def bind[A, B](f: A ⇒ R ⇒ S ⇒ T ⇒ U ⇒ V ⇒ W ⇒ B) = g ⇒ r ⇒ s ⇒ t ⇒ u ⇒ v ⇒ w ⇒ f(g(r)(s)(t)(u)(v)(w))(r)(s)(t)(u)(v)(w)
  }

  implicit def state[S] = new Monad[({type λ[α] = State[α, S]})#λ] {
    def point[A](a: ⇒ A) = State[A, S]((a, _))
    def bind[A, B](f: A ⇒ State[B, S]) = state ⇒ State[B, S](state.run andThen { case (a, s) ⇒ f(a).run(s) })
  }

  implicit def writer[O : Monoid] = new Monad[({type λ[α] = Writer[α, O]})#λ] {
    private val monoid = implicitly[Monoid[O]]
    def point[A](a: ⇒ A) = Writer(a, monoid.unit)
    def bind[A, B](f: A ⇒ Writer[B, O]) = {
      case Writer(a, o) ⇒
        val Writer(b, o2) = f(a)
        Writer(b, monoid.append(o, o2))
    }
  }

  implicit def reader[E] = new Monad[({type λ[α] = Reader[E, α]})#λ] {
    def point[A](a: ⇒ A) = Reader(_ ⇒ a)
    def bind[A, B](f: A ⇒ Reader[E, B]) = {
      case Reader(r) ⇒
        Reader(e ⇒ f(r(e)).run(e))
    }
  }

  implicit def cont[R] = new Monad[({type λ[α] = Cont[R, α]})#λ] {
    def point[A](a: ⇒ A) = Cont(_(a))
    def bind[A, B](f: A ⇒ Cont[R, B]) = cont ⇒ Cont(g ⇒ cont.run(a ⇒ f(a).run(g)))
  }
}
