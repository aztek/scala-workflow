package scala.idioms

import language.higherKinds

trait SemiIdiom[F[_]] extends Functor[F] {
  def app[A, B](f: F[A ⇒ B]): F[A] ⇒ F[B]
  def $ [G[_]](g: SemiIdiom[G]) = new SemiIdiomT(this, g)
}

class SemiIdiomT[F[_], G[_]](f: SemiIdiom[F], g: SemiIdiom[G]) extends FunctorT(f, g) with SemiIdiom[({type λ[α] = F[G[α]]})#λ] {
  def app[A, B](h: F[G[A ⇒ B]]) = f app (f map (g.app[A, B] _))(h)
}

object SemiIdiom {
  val zipList = new SemiIdiom[List] {
    def map[A, B](f: A ⇒ B) = _ map f
    def app[A, B](ff: List[A ⇒ B]) = xs ⇒ (ff, xs).zipped map (_ apply _)
  }
}
