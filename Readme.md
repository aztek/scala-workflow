Scala Idioms
============
`scala-idioms` is a library, that allows you to organize applicative
computations nicely with the help of enhanced version of _idiom brackets_.
The main source of inspiration is the paper [Applicative Programming with
Effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.html) by Conor
McBride and Ross Paterson. The syntax is somewhat borrowed from [idioms
notation](http://www.cs.st-andrews.ac.uk/~eb/Idris/donotation.html) of
[Idris](http://idris-lang.org) programming language.

`scala-idioms` is built around [untyped macros](http://docs.scala-lang.org/overviews/macros/untypedmacros.html)
that is an experimental feature of [Macro Paradise](http://docs.scala-lang.org/overviews/macros/paradise.html).

Quick examples
--------------
```scala
    import scala.idioms._

    idiom[Option] {
      $(Some(42) + 1) should equal (Some(43))
      $(Some(10) + Some(5) * Some(2)) should equal (Some(20))
    }

    idiom[List] {
      $(List(1, 2, 3) * 2) should equal (List(2, 4, 6))
      $(List("a", "b") + List("x", "y")) should equal (List("ax", "ay", "bx", "by"))
    }

    idiom[Future] {
      def slowPlus(x: Int, y: Int) = { Thread.sleep(900); x + y }
      val a = Future(slowPlus(1, 3))
      val b = Future(slowPlus(2, 4))
      Await.result($(a * b + 3), 1 second) should equal (27)
    }
```

What's an idiom?
----------------
Idiom (also called _applicative functor_) is an algebraic structure with two
operators `pure` and `app` of certain types. Here's how `Idiom` trait is
defined within `scala-idioms`.
```scala
    trait Idiom[F[_]] {
      def pure[T](t: ⇒ T): F[T]
      def app[A, B](f: F[A ⇒ B]): F[A] ⇒ F[B]
    }
```

Idiom brackets is a notation of the form `$(expression)`, that indicates, that
`expression` should be evaluated within idiomatic context defined by enclosing
`idiom` block. That idiomatic context is usually accosiated with some sort of
_computational effects_, such as computations that may or may not fail
(`Option`), non-deterministic computations (`List`) or delayed computations
(`Future`).

Syntax of idioms
----------------
Idiom block is defined with `idiom` macro that either takes an idiom instance
as an argument, or a type constructor `F[_]`, such that there is some idiom
instance `Idiom[F]` defined somewhere in the implicits scope.

There are plenty of idiom instances for common data types defined in
`scala.idioms.Idiom` object.

The following examples are equivalent.
```scala
    import scala.idioms.Idiom.list

    idiom[List] {
      $(List(2, 5) * List(3, 7))
    }
    
    idiom(list) {
      $(List(2, 5) * List(3, 7))
    }
```

Macro `$` takes idiomatic context from the closest `idiom` block.
Alternatively, you can provide type constructor, whose idiom instance will be
taken from the implicits scope.
```scala
    $[List](List(2, 5) * List(3, 7))
```

That way, `$` will disregard any enclosing `idiom` block and will work within
`Idiom[List]` context.

Idiom transformers
------------------
Idioms are known to be composable. There's special _idiom transformer_ class
`IdiomT`, that allows you, having instances of `Idiom[F]` and `Idiom[G]`, to 
get instance of `Idiom[F[G]]`. You can either create `IdiomT` object directly
with class constructor, or use `$` method of the class `Idiom`.

```scala
    idiom(list $ option) {
      val xs = List(Some(2), Some(3), None)
      $(xs * 10) should equal (List(Some(20), Some(30), None))
    }
```

Currently, you can only build idiom transformers using terms, but not types
(i.e. that would be really cool to be able to write `idiom[List $ Option]`,
but that feature is not supported yet).

When are idioms useful?
-----------------------
Short answer, they might be a shorter substitute of `for`-expressions.
Consider the examples below, where we parse JSON string to some `Person` object.
```scala
    def parse(json: Json): Option[Person] =
      for {
        name ← parseName(json)
        birthday ← parseBirthday(json)
      } yield {
        val id = parseId(json)     // say, we're confident, that
        val dept = parseDept(json) // those can be extracted
        Person(id, name, new Date(birthday), dept)
      }
```

Note, how the syntax gets rather awkward, when we separate pure values from
values, extracted from idiomatic context. Using idiom brackets, we can compose
the same code in almost imperative manner. 
```scala
    def parse2(json: Json): Option[Person] =
      idiom[Option] {
        val id = parseId(json)
        val name = parseName(json)
        val birthday = parseBirthday(json)
        val dept = parseDept(json)
        $(Person(id, name, new Date(birthday), dept))
      }
```

Keep in mind, though, that `for` defines more powerful _monadic_ computational
context and it might not be possible to express some things with just idiom
brackets. Specifically, working inside a monad, you can refer to results of
previous computations and that is something idioms can't do.

Any other interesting examples?
-------------------------------
McBride and Patterson's paper describes a simple evaluator for a language of
expressions. Following that examples, here's how it would look like here.

We start with the definition of abstract syntax tree of a language with
integers, variables and a plus.

```scala
    sealed trait Expr
    case class Var(id: String) extends Expr
    case class Val(value: Int) extends Expr
    case class Add(lhs: Expr, rhs: Expr) extends Expr
```

Variables are fetched from the environment of type `Env`.
```scala
    type Env = Map[String, Int]
    def fetch(x: String)(env: Env): Option[Int] = env.get(x)
```

The evaluator itself is a function of type `Expr ⇒ Env ⇒ Option[Int]`.
```scala
    def eval(expr: Expr)(env: Env): Option[Int] =
      expr match {
        case Var(x) ⇒ fetch(x)(env)
        case Val(value) ⇒ Some(value)
        case Add(x, y) ⇒ for {
          lhs ← eval(x)(env)
          rhs ← eval(y)(env)
        } yield lhs + rhs
      }
```

Note, that one have to explicitly pass the environment around and to have
rather clumsy syntax to compute the addition. This can be simplified, once
wrapped into the idiom `Env ⇒ Option[_]`, which can either be constructed by
hand, or composed of `Env ⇒ _` and `Option` idioms.
```scala
    def eval: Expr ⇒ Env ⇒ Option[Int] =
      idiom (function[Env] $ option) {
        case Var(x) ⇒ fetch(x)
        case Val(value) ⇒ $(value)
        case Add(x, y) ⇒ $(eval(x) + eval(y))
      }
```

How does it work?
-----------------
Originally, idiom brackets is a fairly simple syntactic transformation, that
wraps a function in `pure` call and replace each function application with
`app` call. That way, only a single function, applied to idiom instances is
supposed to be put inside the brackets.

Idiom brackets in `scala-idioms` are more flexible and currently support
arbitrarily deeply nested function applications. For that, some extra work
needs to be done, besides `pure` and `app` calls generation.

Since we can't just lift function arguments into the idiomatic context, the
main question is, how to decide which subexpressions should be lifted.

Current implementation uses untyped macros and takes untyped Scala AST as an
argument. Then we start by eagerly (i.e., starting with the most common)
typechecking all the subexpressions and find, which of them typechecks
successfully and the result type corresponds to the type of the idiom.

Consider the example below.
```scala
    idiom(option) {
      $(2 * 3 + Some(10) * Some(5))
    }
```

The whole expression does not typecheck. `2 * 3` typechecks to `Int` and
therefore doen't need to be lifted. `Some(10) + Some(5)`, does not typecheck,
but each of its arguments typechecks to `Option[Int]` (well, technically,
`Some[Int]`, but we can handle that), so both arguments get lifted.

Generated code will look like this.
```scala
    option.app(option.app(option.pure((x$1: Int) ⇒ (x$2: Int) ⇒ 2 * 3 + x$1 * x$2))(Some(10)))(Some(5))
```

Contributions
-------------
This project is still very experimental and comments and suggestions are highly
appreciated. Drop me a line [on twitter](http://twitter.com/aztek) or
[by email](mailto:evgeny.kotelnikov@gmail.com), or [open an issue](https://github.com/aztek/scala-idioms/issues/new)
here on GitHub.