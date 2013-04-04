package scala.idioms

import language.higherKinds

trait SemiIdiom[F[_]] {
  def map[A, B](f: A ⇒ B): F[A] ⇒ F[B]
  def app[A, B](f: F[A ⇒ B]): F[A] ⇒ F[B]
  def $ [G[_]](g: SemiIdiom[G]) = new SemiIdiomT(this, g)
}

class SemiIdiomT[F[_], G[_]](f: SemiIdiom[F], g: SemiIdiom[G]) extends SemiIdiom[({type λ[α] = F[G[α]]})#λ] {
  def map[A, B](h: A ⇒ B) = f map (g map h)
  def app[A, B](h: F[G[A ⇒ B]]) = f app (f map (g.app[A, B] _))(h)
}

object SemiIdiom {
  implicit def tuple[T] = new SemiIdiom[({type λ[α] = (T, α)})#λ] {
    def map[A, B](f: A ⇒ B) = { case (lhs, rhs) ⇒ (lhs, f(rhs)) }
    def app[A, B](ef: (T, A ⇒ B)) = { case (lhs, rhs) ⇒ (lhs, ef._2(rhs)) }
  }
}
