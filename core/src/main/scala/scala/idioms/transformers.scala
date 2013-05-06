package scala.idioms

import language.higherKinds

class FunctorT[F[_], G[_]](f: Functor[F], g: Functor[G]) extends Functor[({type λ[α] = F[G[α]]})#λ] {
  def map[A, B](h: A ⇒ B) = f map (g map h)
}

class SemiIdiomT[F[_], G[_]](f: SemiIdiom[F], g: SemiIdiom[G]) extends SemiIdiom[({type λ[α] = F[G[α]]})#λ] {
  def map[A, B](h: A ⇒ B) = f map (g map h)
  def app[A, B](h: F[G[A ⇒ B]]) = f app (f map (g.app[A, B] _))(h)
}

class IdiomT[F[_], G[_]](f: Idiom[F], g: Idiom[G]) extends Idiom[({type λ[α] = F[G[α]]})#λ] {
  def point[A](a: ⇒ A) = f.point(g.point(a))
  def map[A, B](h: A ⇒ B) = f map (g map h)
  def app[A, B](h: F[G[A ⇒ B]]) = f app (f map (g.app[A, B] _))(h)
}
