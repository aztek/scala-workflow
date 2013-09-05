package scala.workflow

import language.higherKinds

trait FunctorComposition[F[_]] { f: Functor[F] ⇒
  def $ [G[_]](g: Functor[G]) = new Functor[({type λ[α] = F[G[α]]})#λ] {
    def map[A, B](h: A ⇒ B) = f map (g map h)
  }
  def & [G[_]](g: Functor[G]) = g $ this
}

trait SemiIdiomComposition[F[_]] { f: SemiIdiom[F] ⇒
  def $ [G[_]](g: SemiIdiom[G]) = new SemiIdiom[({type λ[α] = F[G[α]]})#λ] {
    def map[A, B](h: A ⇒ B) = f map (g map h)
    def app[A, B](h: F[G[A ⇒ B]]) = f app (f map g.app[A, B])(h)
  }
  def & [G[_]](g: SemiIdiom[G]) = g $ this
}

trait IdiomComposition[F[_]] { f: Idiom[F] ⇒
  def $ [G[_]](g: Idiom[G]) = new Idiom[({type λ[α] = F[G[α]]})#λ] {
    def point[A](a: ⇒ A) = f.point(g.point(a))
    def app[A, B](h: F[G[A ⇒ B]]) = f app (f map g.app[A, B])(h)
  }
  def & [G[_]](g: Idiom[G]) = g $ this
}

/* Monads and semi-monads in general cannot be composed. Instead, some
 * particular monads can provide their own specific implementation of
 * `$` or `&` method. Note, that even having, say, `$` method defined, monad
 * might not be able to define `&`. To capture this notion, `Monad[F]` can be
 * extended to either `LeftComposableMonad[F]` or `RightComposableMonad[F]`.
 * In the first case it is supposed to be able to produce `F[G[_]]` monad and
 * therefore implement `$` method. In the second case it is supposed to be able
 * to produce `G[F[_]]` monad and therefore implement `$` method.
 *
 * So, for monads `f` and `g` their composition `f $ g` will be a monad either
 * when `f` is left-composable or `g` is right-composable.
 *
 * The same holds for semi-monads.
 *
 * Methods `$` and `&` are called 'monad transformer' elsewhere, although
 * separation of left- and right- composability is usually not introduced.
 */
trait SemiMonadComposition[F[_]] { f: SemiMonad[F] ⇒
  def $ [G[_]](g: RightComposableSemiMonad[G]): SemiMonad[({type λ[α] = F[G[α]]})#λ] = g & f
  def & [G[_]](g: LeftComposableSemiMonad[G]):  SemiMonad[({type λ[α] = G[F[α]]})#λ] = g $ f
}

trait LeftComposableSemiMonad[F[_]] extends SemiMonad[F] {
  def $ [G[_]](g: SemiMonad[G]): SemiMonad[({type λ[α] = F[G[α]]})#λ]
  override def $ [G[_]](g: RightComposableSemiMonad[G]) = $(g.asInstanceOf[SemiMonad[G]])
}

trait RightComposableSemiMonad[F[_]] extends SemiMonad[F] {
  def & [G[_]](g: SemiMonad[G]): SemiMonad[({type λ[α] = G[F[α]]})#λ]
  override def & [G[_]](g: LeftComposableSemiMonad[G]) = &(g.asInstanceOf[SemiMonad[G]])
}

trait MonadComposition[F[_]] { f: Monad[F] ⇒
  def $ [G[_]](g: RightComposableMonad[G]): Monad[({type λ[α] = F[G[α]]})#λ] = g & this
  def & [G[_]](g: LeftComposableMonad[G]):  Monad[({type λ[α] = G[F[α]]})#λ] = g $ this
}

trait LeftComposableMonad[F[_]] extends Monad[F] {
  def $ [G[_]](g: Monad[G]): Monad[({type λ[α] = F[G[α]]})#λ]
  override def $ [G[_]](g: RightComposableMonad[G]) = $(g.asInstanceOf[Monad[G]])
}

trait RightComposableMonad[F[_]] extends Monad[F] {
  def & [G[_]](g: Monad[G]): Monad[({type λ[α] = G[F[α]]})#λ]
  override def & [G[_]](g: LeftComposableMonad[G]) = &(g.asInstanceOf[Monad[G]])
}