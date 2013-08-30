package scala.workflow

import language.higherKinds

trait FunctorComposition[F[_]] { f: Functor[F] ⇒
  def & [G[_]](g: Functor[G]) = new Functor[({type λ[α] = G[F[α]]})#λ] {
    def map[A, B](h: A ⇒ B) = g map (f map h)
  }
}

trait SemiIdiomComposition[F[_]] { f: SemiIdiom[F] ⇒
  def & [G[_]](g: SemiIdiom[G]) = new SemiIdiom[({type λ[α] = G[F[α]]})#λ] {
    def map[A, B](h: A ⇒ B) = g map (f map h)
    def app[A, B](h: G[F[A ⇒ B]]) = g app (g map f.app[A, B])(h)
  }
}

trait IdiomComposition[F[_]] { f: Idiom[F] ⇒
  def & [G[_]](g: Idiom[G]) = new Idiom[({type λ[α] = G[F[α]]})#λ] {
    def point[A](a: ⇒ A) = g.point(f.point(a))
    def app[A, B](h: G[F[A ⇒ B]]) = g app (g map f.app[A, B])(h)
  }
}

/* Note, that whereas other algebraic structures can be composed, monads (and
 * semi-monads) in general can't, therefore there's no default implementation
 * of '&' method. To make a particular monad instance composable, one should
 * mix `MonadComposition` to Monad object and define '&' method.
 *
 * '&' method is called 'monad transformer' elsewhere.
 */
trait SemiMonadComposition[F[_]] { f: SemiMonad[F] ⇒
  def & [G[_]](g: SemiMonad[G]): SemiMonad[({type λ[α] = G[F[α]]})#λ]
}

trait MonadComposition[F[_]] { f: Monad[F] ⇒
  def & [G[_]](g: Monad[G]): Monad[({type λ[α] = G[F[α]]})#λ]
}