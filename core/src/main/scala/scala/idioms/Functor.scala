package scala.idioms

import language.higherKinds

trait Functor[F[_]] {
  def map[A, B](f: A ⇒ B): F[A] ⇒ F[B]
  def $ [G[_]](g: Functor[G]) = new FunctorT(this, g)
}

class FunctorT[F[_], G[_]](f: Functor[F], g: Functor[G]) extends Functor[({type λ[α] = F[G[α]]})#λ] {
  def map[A, B](h: A ⇒ B) = f map (g map h)
}

object Functor extends FunctorInstances

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
