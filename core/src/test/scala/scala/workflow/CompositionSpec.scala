package scala.workflow

import language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/** Specification for rules of workflow composition */
class CompositionSpec extends FlatSpec with ShouldMatchers {
  behavior of "Rules of workflow composition"

  import CompositionSpec._

  "Composition of two functors" should "produce a functor" in {
    (functor.f $ functor.g).isInstanceOf[Functor[Any]] should equal (true)
    (functor.f & functor.g).isInstanceOf[Functor[Any]] should equal (true)
  }

  "Composition of two semi-idioms" should "produce a semi-idiom" in {
    (semiidiom.f $ semiidiom.g).isInstanceOf[SemiIdiom[Any]] should equal (true)
    (semiidiom.f & semiidiom.g).isInstanceOf[SemiIdiom[Any]] should equal (true)
  }

  "Composition of two idioms" should "produce an idiom" in {
    (idiom.f $ idiom.g).isInstanceOf[Idiom[Any]] should equal (true)
    (idiom.f & idiom.g).isInstanceOf[Idiom[Any]] should equal (true)
  }

  "Composition of two non-composable monads" should "produce an idiom" in {
    (monad.f $ monad.g).isInstanceOf[Idiom[Any]] should equal (true)
    (monad.f & monad.g).isInstanceOf[Idiom[Any]] should equal (true)
  }

  "Composition of a functor and a semi-idiom" should "produce a functor" in {
    (functor.f $ semiidiom.g).isInstanceOf[Functor[Any]] should equal (true)
    (functor.f & semiidiom.g).isInstanceOf[Functor[Any]] should equal (true)
    (semiidiom.g $ functor.f).isInstanceOf[Functor[Any]] should equal (true)
    (semiidiom.g & functor.f).isInstanceOf[Functor[Any]] should equal (true)
  }

  "Composition of a functor and an idiom" should "produce a functor" in {
    (functor.f $ idiom.g).isInstanceOf[Functor[Any]] should equal (true)
    (functor.f & idiom.g).isInstanceOf[Functor[Any]] should equal (true)
    (idiom.g $ functor.f).isInstanceOf[Functor[Any]] should equal (true)
    (idiom.g & functor.f).isInstanceOf[Functor[Any]] should equal (true)
  }

  "Composition of a functor and a semi-monad" should "produce a functor" in {
    (functor.f $ semimonad.g).isInstanceOf[Functor[Any]] should equal (true)
    (functor.f & semimonad.g).isInstanceOf[Functor[Any]] should equal (true)
    (semimonad.g $ functor.f).isInstanceOf[Functor[Any]] should equal (true)
    (semimonad.g & functor.f).isInstanceOf[Functor[Any]] should equal (true)
  }

  "Composition of a functor and a monad" should "produce a functor" in {
    (functor.f $ monad.g).isInstanceOf[Functor[Any]] should equal (true)
    (functor.f & monad.g).isInstanceOf[Functor[Any]] should equal (true)
    (monad.g $ functor.f).isInstanceOf[Functor[Any]] should equal (true)
    (monad.g & functor.f).isInstanceOf[Functor[Any]] should equal (true)
  }

  "Composition of a semi-idiom and an idiom" should "produce a semi-idiom" in {
    (semiidiom.f $ idiom.g).isInstanceOf[SemiIdiom[Any]] should equal (true)
    (semiidiom.f & idiom.g).isInstanceOf[SemiIdiom[Any]] should equal (true)
    (idiom.g $ semiidiom.f).isInstanceOf[SemiIdiom[Any]] should equal (true)
    (idiom.g & semiidiom.f).isInstanceOf[SemiIdiom[Any]] should equal (true)
  }

  "Composition of a semi-idiom and a semi-monad" should "produce a semi-idiom" in {
    (semiidiom.f $ semimonad.g).isInstanceOf[SemiIdiom[Any]] should equal (true)
    (semiidiom.f & semimonad.g).isInstanceOf[SemiIdiom[Any]] should equal (true)
    (semimonad.g $ semiidiom.f).isInstanceOf[SemiIdiom[Any]] should equal (true)
    (semimonad.g & semiidiom.f).isInstanceOf[SemiIdiom[Any]] should equal (true)
  }

  "Composition of a semi-idiom and a monad" should "produce a semi-idiom" in {
    (semiidiom.f $ monad.g).isInstanceOf[SemiIdiom[Any]] should equal (true)
    (semiidiom.f & monad.g).isInstanceOf[SemiIdiom[Any]] should equal (true)
    (monad.g $ semiidiom.f).isInstanceOf[SemiIdiom[Any]] should equal (true)
    (monad.g & semiidiom.f).isInstanceOf[SemiIdiom[Any]] should equal (true)
  }

  "Composition of a idiom and a semi-monad" should "produce a semi-idiom" in {
    (idiom.f $ semimonad.g).isInstanceOf[SemiIdiom[Any]] should equal (true)
    (idiom.f & semimonad.g).isInstanceOf[SemiIdiom[Any]] should equal (true)
    (semimonad.g $ idiom.f).isInstanceOf[SemiIdiom[Any]] should equal (true)
    (semimonad.g & idiom.f).isInstanceOf[SemiIdiom[Any]] should equal (true)
  }

  "Composition of a idiom and a monad" should "produce an idiom" in {
    (idiom.f $ monad.g).isInstanceOf[Idiom[Any]] should equal (true)
    (idiom.f & monad.g).isInstanceOf[Idiom[Any]] should equal (true)
    (monad.g $ idiom.f).isInstanceOf[Idiom[Any]] should equal (true)
    (monad.g & idiom.f).isInstanceOf[Idiom[Any]] should equal (true)
  }

  "Composition of a semi-monad and a monad" should "produce a semi-monad" in {
    (idiom.f $ monad.g).isInstanceOf[Idiom[Any]] should equal (true)
    (idiom.f & monad.g).isInstanceOf[Idiom[Any]] should equal (true)
    (monad.g $ idiom.f).isInstanceOf[Idiom[Any]] should equal (true)
    (monad.g & idiom.f).isInstanceOf[Idiom[Any]] should equal (true)
  }

  "Composition of a left-composable semi-monad and a semi-monad" should "produce a semi-monad" in {
    (semimonad.left.f $ semimonad.g).isInstanceOf[SemiMonad[Any]] should equal (true)
    (semimonad.g & semimonad.left.f).isInstanceOf[SemiMonad[Any]] should equal (true)
    (semimonad.left.g & semimonad.left.f).isInstanceOf[SemiMonad[Any]] should equal (true)
    (semimonad.left.f $ semimonad.left.g).isInstanceOf[SemiMonad[Any]] should equal (true)
    (semimonad.right.g & semimonad.left.f).isInstanceOf[SemiMonad[Any]] should equal (true)
    (semimonad.left.f $ semimonad.right.g).isInstanceOf[SemiMonad[Any]] should equal (true)
  }

  "Composition of a semi-monad and a right-composable semi-monad" should "produce a semi-monad" in {
    (semimonad.g $ semimonad.right.f).isInstanceOf[SemiMonad[Any]] should equal (true)
    (semimonad.right.f & semimonad.g).isInstanceOf[SemiMonad[Any]] should equal (true)
    (semimonad.right.g $ semimonad.right.f).isInstanceOf[SemiMonad[Any]] should equal (true)
    (semimonad.right.f & semimonad.right.g).isInstanceOf[SemiMonad[Any]] should equal (true)
    (semimonad.left.g $ semimonad.right.f).isInstanceOf[SemiMonad[Any]] should equal (true)
    (semimonad.right.f & semimonad.left.g).isInstanceOf[SemiMonad[Any]] should equal (true)
  }

  "Composition of a left-composable monad and a monad" should "produce a monad" in {
    (monad.left.f $ monad.g).isInstanceOf[Monad[Any]] should equal (true)
    (monad.g & monad.left.f).isInstanceOf[Monad[Any]] should equal (true)
    (monad.left.g & monad.left.f).isInstanceOf[Monad[Any]] should equal (true)
    (monad.left.f $ monad.left.g).isInstanceOf[Monad[Any]] should equal (true)
    (monad.right.g & monad.left.f).isInstanceOf[Monad[Any]] should equal (true)
    (monad.left.f $ monad.right.g).isInstanceOf[Monad[Any]] should equal (true)
  }

  "Composition of a monad and a right-composable monad" should "produce a monad" in {
    (monad.g $ monad.right.f).isInstanceOf[Monad[Any]] should equal (true)
    (monad.right.f & monad.g).isInstanceOf[Monad[Any]] should equal (true)
    (monad.right.g $ monad.right.f).isInstanceOf[Monad[Any]] should equal (true)
    (monad.right.f & monad.right.g).isInstanceOf[Monad[Any]] should equal (true)
    (monad.left.g $ monad.right.f).isInstanceOf[Monad[Any]] should equal (true)
    (monad.right.f & monad.left.g).isInstanceOf[Monad[Any]] should equal (true)
  }
}

object CompositionSpec {
  case class F[A](a: A)
  case class G[A](a: A)

  object functor {
    val f = new Functor[F] {
      def map[A, B](h: A ⇒ B) = f ⇒ F(h(f.a))
    }

    val g = new Functor[G] {
      def map[A, B](h: A ⇒ B) = g ⇒ G(h(g.a))
    }
  }

  object semiidiom {
    val f = new SemiIdiom[F] {
      def map[A, B](h: A ⇒ B) = f ⇒ F(h(f.a))
      def app[A, B](h: F[A ⇒ B]) = f ⇒ F(h.a(f.a))
    }

    val g = new SemiIdiom[G] {
      def map[A, B](h: A ⇒ B) = g ⇒ G(h(g.a))
      def app[A, B](h: G[A ⇒ B]) = g ⇒ G(h.a(g.a))
    }
  }

  object idiom {
    val f = new Idiom[F] {
      def point[A](a: ⇒ A) = F(a)
      def app[A, B](h: F[A ⇒ B]) = f ⇒ F(h.a(f.a))
    }

    val g = new Idiom[G] {
      def point[A](a: ⇒ A) = G(a)
      def app[A, B](h: G[A ⇒ B]) = g ⇒ G(h.a(g.a))
    }
  }

  object semimonad {
    val f = new SemiMonad[F] {
      def map[A, B](h: A ⇒ B) = f ⇒ F(h(f.a))
      def app[A, B](h: F[A ⇒ B]) = f ⇒ F(h.a(f.a))
      def bind[A, B](h: A ⇒ F[B]) = f ⇒ h(f.a)
    }

    val g = new SemiMonad[G] {
      def map[A, B](h: A ⇒ B) = g ⇒ G(h(g.a))
      def app[A, B](h: G[A ⇒ B]) = g ⇒ G(h.a(g.a))
      def bind[A, B](h: A ⇒ G[B]) = g ⇒ h(g.a)
    }

    object left {
      val f = new LeftComposableSemiMonad[F] { s ⇒
        def map[A, B](h: A ⇒ B) = f ⇒ F(h(f.a))
        def app[A, B](h: F[A ⇒ B]) = f ⇒ F(h.a(f.a))
        def bind[A, B](h: A ⇒ F[B]) = f ⇒ h(f.a)
        def $[H[_]](h: SemiMonad[H]) = new SemiMonad[({type λ[α] = F[H[α]]})#λ] {
          def map[A, B](g: A ⇒ B) = s map (h map g)
          def app[A, B](g: F[H[A ⇒ B]]) = s app (s map h.app[A, B])(g)
          def bind[A, B](f: A ⇒ F[H[B]]) = {
            case F(ha) ⇒ F(h.bind((a: A) ⇒ f(a).a)(ha))
          }
        }
      }

      val g = new LeftComposableSemiMonad[G] { s ⇒
        def map[A, B](h: A ⇒ B) = g ⇒ G(h(g.a))
        def app[A, B](h: G[A ⇒ B]) = g ⇒ G(h.a(g.a))
        def bind[A, B](h: A ⇒ G[B]) = g ⇒ h(g.a)
        def $[H[_]](h: SemiMonad[H]) = new SemiMonad[({type λ[α] = G[H[α]]})#λ] {
          def map[A, B](f: A ⇒ B) = s map (h map f)
          def app[A, B](f: G[H[A ⇒ B]]) = s app (s map h.app[A, B])(f)
          def bind[A, B](g: A ⇒ G[H[B]]) = {
            case G(ha) ⇒ G(h.bind((a: A) ⇒ g(a).a)(ha))
          }
        }
      }
    }

    object right {
      val f = new RightComposableSemiMonad[F] { s ⇒
        def map[A, B](h: A ⇒ B) = f ⇒ F(h(f.a))
        def app[A, B](h: F[A ⇒ B]) = f ⇒ F(h.a(f.a))
        def bind[A, B](h: A ⇒ F[B]) = f ⇒ h(f.a)
        def &[H[_]](h: SemiMonad[H]) = new SemiMonad[({type λ[α] = H[F[α]]})#λ] {
          def map[A, B](g: A ⇒ B) = h map (s map g)
          def app[A, B](g: H[F[A ⇒ B]]) = h app (h map s.app[A, B])(g)
          def bind[A, B](f: A ⇒ H[F[B]]) = h.bind { fa ⇒ f(fa.a) }
        }
      }

      val g = new RightComposableSemiMonad[G] { s ⇒
        def map[A, B](h: A ⇒ B) = g ⇒ G(h(g.a))
        def app[A, B](h: G[A ⇒ B]) = g ⇒ G(h.a(g.a))
        def bind[A, B](h: A ⇒ G[B]) = g ⇒ h(g.a)
        def &[H[_]](h: SemiMonad[H]) = new SemiMonad[({type λ[α] = H[G[α]]})#λ] {
          def map[A, B](f: A ⇒ B) = h map (s map f)
          def app[A, B](f: H[G[A ⇒ B]]) = h app (h map s.app[A, B])(f)
          def bind[A, B](g: A ⇒ H[G[B]]) = h.bind { ga ⇒ g(ga.a) }
        }
      }
    }
  }

  object monad {
    val f = new Monad[F] {
      def point[A](a: ⇒ A) = F(a)
      def bind[A, B](h: A ⇒ F[B]) = f ⇒ h(f.a)
    }

    val g = new Monad[G] {
      def point[A](a: ⇒ A) = G(a)
      def bind[A, B](h: A ⇒ G[B]) = g ⇒ h(g.a)
    }

    object left {
      val f = new LeftComposableMonad[F] {
        def point[A](a: ⇒ A) = F(a)
        def bind[A, B](h: A ⇒ F[B]) = f ⇒ h(f.a)
        def $[H[_]](h: Monad[H]) = new Monad[({type λ[α] = F[H[α]]})#λ] {
          def point[A](a: ⇒ A) = F(h.point(a))
          def bind[A, B](f: A ⇒ F[H[B]]) = {
            case F(ha) ⇒ F(h.bind((a: A) ⇒ f(a).a)(ha))
          }
        }
      }

      val g = new LeftComposableMonad[G] {
        def point[A](a: ⇒ A) = G(a)
        def bind[A, B](h: A ⇒ G[B]) = g ⇒ h(g.a)
        def $[H[_]](h: Monad[H]) = new Monad[({type λ[α] = G[H[α]]})#λ] {
          def point[A](a: ⇒ A) = G(h.point(a))
          def bind[A, B](g: A ⇒ G[H[B]]) = {
            case G(ha) ⇒ G(h.bind((a: A) ⇒ g(a).a)(ha))
          }
        }
      }
    }

    object right {
      val f = new RightComposableMonad[F] {
        def point[A](a: ⇒ A) = F(a)
        def bind[A, B](h: A ⇒ F[B]) = f ⇒ h(f.a)
        def &[H[_]](h: Monad[H]) = new Monad[({type λ[α] = H[F[α]]})#λ] {
          def point[A](a: ⇒ A) = h.point(F(a))
          def bind[A, B](f: A ⇒ H[F[B]]) = h.bind { fa ⇒ f(fa.a) }
        }
      }

      val g = new RightComposableMonad[G] {
        def point[A](a: ⇒ A) = G(a)
        def bind[A, B](h: A ⇒ G[B]) = g ⇒ h(g.a)
        def &[H[_]](h: Monad[H]) = new Monad[({type λ[α] = H[G[α]]})#λ] {
          def point[A](a: ⇒ A) = h.point(G(a))
          def bind[A, B](g: A ⇒ H[G[B]]) = h.bind { ga ⇒ g(ga.a) }
        }
      }
    }
  }
}