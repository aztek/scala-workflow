package scala.workflow

import language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/** Specification for rules of workflow composition */
class CompositionSpec extends FlatSpec with ShouldMatchers {
  behavior of "Rules of workflow composition"

  import CompositionSpec._

  /* Type arguments of workflow instances can only be checked at compile
     time (unless Manifests or TypeTags are used) due to type erasure.
     Successful compilation of this specification is enough to prove
     that everything is working. */

  "Composition of two functors" should "produce a functor" in {
    functor.f $ functor.g: Functor[({type λ[α] = F[G[α]]})#λ]
    functor.f & functor.g: Functor[({type λ[α] = G[F[α]]})#λ]
  }

  "Composition of two semi-idioms" should "produce a semi-idiom" in {
    semiidiom.f $ semiidiom.g: SemiIdiom[({type λ[α] = F[G[α]]})#λ]
    semiidiom.f & semiidiom.g: SemiIdiom[({type λ[α] = G[F[α]]})#λ]
  }

  "Composition of two idioms" should "produce an idiom" in {
    idiom.f $ idiom.g: Idiom[({type λ[α] = F[G[α]]})#λ]
    idiom.f & idiom.g: Idiom[({type λ[α] = G[F[α]]})#λ]
  }

  "Composition of two non-composable monads" should "produce an idiom" in {
    monad.f $ monad.g: Idiom[({type λ[α] = F[G[α]]})#λ]
    monad.f & monad.g: Idiom[({type λ[α] = G[F[α]]})#λ]
  }

  "Composition of a functor and a semi-idiom" should "produce a functor" in {
    functor.f $ semiidiom.g: Functor[({type λ[α] = F[G[α]]})#λ]
    functor.f & semiidiom.g: Functor[({type λ[α] = G[F[α]]})#λ]
    semiidiom.g $ functor.f: Functor[({type λ[α] = G[F[α]]})#λ]
    semiidiom.g & functor.f: Functor[({type λ[α] = F[G[α]]})#λ]
  }

  "Composition of a functor and an idiom" should "produce a functor" in {
    functor.f $ idiom.g: Functor[({type λ[α] = F[G[α]]})#λ]
    functor.f & idiom.g: Functor[({type λ[α] = G[F[α]]})#λ]
    idiom.g $ functor.f: Functor[({type λ[α] = G[F[α]]})#λ]
    idiom.g & functor.f: Functor[({type λ[α] = F[G[α]]})#λ]
  }

  "Composition of a functor and a semi-monad" should "produce a functor" in {
    functor.f $ semimonad.g: Functor[({type λ[α] = F[G[α]]})#λ]
    functor.f & semimonad.g: Functor[({type λ[α] = G[F[α]]})#λ]
    semimonad.g $ functor.f: Functor[({type λ[α] = G[F[α]]})#λ]
    semimonad.g & functor.f: Functor[({type λ[α] = F[G[α]]})#λ]
  }

  "Composition of a functor and a monad" should "produce a functor" in {
    functor.f $ monad.g: Functor[({type λ[α] = F[G[α]]})#λ]
    functor.f & monad.g: Functor[({type λ[α] = G[F[α]]})#λ]
    monad.g $ functor.f: Functor[({type λ[α] = G[F[α]]})#λ]
    monad.g & functor.f: Functor[({type λ[α] = F[G[α]]})#λ]
  }

  "Composition of a semi-idiom and an idiom" should "produce a semi-idiom" in {
    semiidiom.f $ idiom.g: SemiIdiom[({type λ[α] = F[G[α]]})#λ]
    semiidiom.f & idiom.g: SemiIdiom[({type λ[α] = G[F[α]]})#λ]
    idiom.g $ semiidiom.f: SemiIdiom[({type λ[α] = G[F[α]]})#λ]
    idiom.g & semiidiom.f: SemiIdiom[({type λ[α] = F[G[α]]})#λ]
  }

  "Composition of a semi-idiom and a semi-monad" should "produce a semi-idiom" in {
    semiidiom.f $ semimonad.g: SemiIdiom[({type λ[α] = F[G[α]]})#λ]
    semiidiom.f & semimonad.g: SemiIdiom[({type λ[α] = G[F[α]]})#λ]
    semimonad.g $ semiidiom.f: SemiIdiom[({type λ[α] = G[F[α]]})#λ]
    semimonad.g & semiidiom.f: SemiIdiom[({type λ[α] = F[G[α]]})#λ]
  }

  "Composition of a semi-idiom and a monad" should "produce a semi-idiom" in {
    semiidiom.f $ monad.g: SemiIdiom[({type λ[α] = F[G[α]]})#λ]
    semiidiom.f & monad.g: SemiIdiom[({type λ[α] = G[F[α]]})#λ]
    monad.g $ semiidiom.f: SemiIdiom[({type λ[α] = G[F[α]]})#λ]
    monad.g & semiidiom.f: SemiIdiom[({type λ[α] = F[G[α]]})#λ]
  }

  "Composition of a idiom and a semi-monad" should "produce a semi-idiom" in {
    idiom.f $ semimonad.g: SemiIdiom[({type λ[α] = F[G[α]]})#λ]
    idiom.f & semimonad.g: SemiIdiom[({type λ[α] = G[F[α]]})#λ]
    semimonad.g $ idiom.f: SemiIdiom[({type λ[α] = G[F[α]]})#λ]
    semimonad.g & idiom.f: SemiIdiom[({type λ[α] = F[G[α]]})#λ]
  }

  "Composition of a idiom and a monad" should "produce an idiom" in {
    idiom.f $ monad.g: Idiom[({type λ[α] = F[G[α]]})#λ]
    idiom.f & monad.g: Idiom[({type λ[α] = G[F[α]]})#λ]
    monad.g $ idiom.f: Idiom[({type λ[α] = G[F[α]]})#λ]
    monad.g & idiom.f: Idiom[({type λ[α] = F[G[α]]})#λ]
  }

  "Composition of a semi-monad and a monad" should "produce a semi-monad" in {
    idiom.f $ monad.g: Idiom[({type λ[α] = F[G[α]]})#λ]
    idiom.f & monad.g: Idiom[({type λ[α] = G[F[α]]})#λ]
    monad.g $ idiom.f: Idiom[({type λ[α] = G[F[α]]})#λ]
    monad.g & idiom.f: Idiom[({type λ[α] = F[G[α]]})#λ]
  }

  "Composition of a left-composable semi-monad and a semi-monad" should "produce a semi-monad" in {
    semimonad.left.f $ semimonad.g: SemiMonad[({type λ[α] = F[G[α]]})#λ]
    semimonad.g & semimonad.left.f: SemiMonad[({type λ[α] = F[G[α]]})#λ]
    semimonad.left.g & semimonad.left.f: SemiMonad[({type λ[α] = F[G[α]]})#λ]
    semimonad.left.f $ semimonad.left.g: SemiMonad[({type λ[α] = F[G[α]]})#λ]
    semimonad.right.g & semimonad.left.f: SemiMonad[({type λ[α] = F[G[α]]})#λ]
    semimonad.left.f $ semimonad.right.g: SemiMonad[({type λ[α] = F[G[α]]})#λ]
  }

  "Composition of a semi-monad and a right-composable semi-monad" should "produce a semi-monad" in {
    semimonad.g $ semimonad.right.f: SemiMonad[({type λ[α] = G[F[α]]})#λ]
    semimonad.right.f & semimonad.g: SemiMonad[({type λ[α] = G[F[α]]})#λ]
    semimonad.right.g $ semimonad.right.f: SemiMonad[({type λ[α] = G[F[α]]})#λ]
    semimonad.right.f & semimonad.right.g: SemiMonad[({type λ[α] = G[F[α]]})#λ]
    semimonad.left.g $ semimonad.right.f: SemiMonad[({type λ[α] = G[F[α]]})#λ]
    semimonad.right.f & semimonad.left.g: SemiMonad[({type λ[α] = G[F[α]]})#λ]
  }

  "Composition of a left-composable semi-monad and a monad" should "produce a semi-monad" in {
    semimonad.left.f $ monad.g: SemiMonad[({type λ[α] = F[G[α]]})#λ]
    monad.g & semimonad.left.f: SemiMonad[({type λ[α] = F[G[α]]})#λ]
    semimonad.left.f $ monad.left.g: SemiMonad[({type λ[α] = F[G[α]]})#λ]
    monad.left.g & semimonad.left.f: SemiMonad[({type λ[α] = F[G[α]]})#λ]
    semimonad.left.f $ monad.right.g: SemiMonad[({type λ[α] = F[G[α]]})#λ]
    monad.right.g & semimonad.left.f: SemiMonad[({type λ[α] = F[G[α]]})#λ]
  }

  "Composition of a monad and a right-composable semi-monad" should "produce a semi-monad" in {
    semimonad.right.f & monad.g: SemiMonad[({type λ[α] = G[F[α]]})#λ]
    monad.g $ semimonad.right.f: SemiMonad[({type λ[α] = G[F[α]]})#λ]
    semimonad.right.f & monad.right.g: SemiMonad[({type λ[α] = G[F[α]]})#λ]
    monad.right.g $ semimonad.right.f: SemiMonad[({type λ[α] = G[F[α]]})#λ]
    semimonad.right.f & monad.left.g: SemiMonad[({type λ[α] = G[F[α]]})#λ]
    monad.left.g $ semimonad.right.f: SemiMonad[({type λ[α] = G[F[α]]})#λ]
  }

  "Composition of a left-composable monad and a monad" should "produce a monad" in {
    monad.left.f $ monad.g: Monad[({type λ[α] = F[G[α]]})#λ]
    monad.g & monad.left.f: Monad[({type λ[α] = F[G[α]]})#λ]
    monad.left.g & monad.left.f: Monad[({type λ[α] = F[G[α]]})#λ]
    monad.left.f $ monad.left.g: Monad[({type λ[α] = F[G[α]]})#λ]
    monad.right.g & monad.left.f: Monad[({type λ[α] = F[G[α]]})#λ]
    monad.left.f $ monad.right.g: Monad[({type λ[α] = F[G[α]]})#λ]
  }

  "Composition of a monad and a right-composable monad" should "produce a monad" in {
    monad.g $ monad.right.f: Monad[({type λ[α] = G[F[α]]})#λ]
    monad.right.f & monad.g: Monad[({type λ[α] = G[F[α]]})#λ]
    monad.right.g $ monad.right.f: Monad[({type λ[α] = G[F[α]]})#λ]
    monad.right.f & monad.right.g: Monad[({type λ[α] = G[F[α]]})#λ]
    monad.left.g $ monad.right.f: Monad[({type λ[α] = G[F[α]]})#λ]
    monad.right.f & monad.left.g: Monad[({type λ[α] = G[F[α]]})#λ]
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