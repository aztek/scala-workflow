package scala.idioms

import language.higherKinds

trait Pointed[F[_]] extends Functor[F] {
  def pure[A](a: ⇒ A): F[A]
}

class PointedT[F[_], G[_]](f: Pointed[F], g: Pointed[G]) extends FunctorT(f, g) with Pointed[({type λ[α] = F[G[α]]})#λ] {
  def pure[A](a: ⇒ A) = f pure (g pure a)
}
