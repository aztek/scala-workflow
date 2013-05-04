package scala.idioms

import language.higherKinds

class FunctorT[F[_], G[_]](f: Functor[F], g: Functor[G]) extends Functor[({type λ[α] = F[G[α]]})#λ] {
  def map[A, B](h: A ⇒ B) = f map (g map h)
}

class PointedT[F[_], G[_]](f: Pointed[F], g: Pointed[G]) extends FunctorT(f, g) with Pointed[({type λ[α] = F[G[α]]})#λ] {
  def pure[A](a: ⇒ A) = f pure (g pure a)
}

class SemiIdiomT[F[_], G[_]](f: SemiIdiom[F], g: SemiIdiom[G]) extends FunctorT(f, g) with SemiIdiom[({type λ[α] = F[G[α]]})#λ] {
  def app[A, B](h: F[G[A ⇒ B]]) = f app (f map (g.app[A, B] _))(h)
}

class IdiomT[F[_], G[_]](f: Idiom[F], g: Idiom[G]) extends SemiIdiomT(f, g) with Idiom[({type λ[α] = F[G[α]]})#λ] {
  def pure[A](a: ⇒ A) = new PointedT(f, g).pure(a)
}
