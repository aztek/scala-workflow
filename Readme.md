Scala idioms
============
`scala-idioms` allows you to organize computations nicely in the predefined set
of computational contexts with the help of enhanced version of _idiom brackets_.
The main source of inspiration is the paper
[Applicative Programming with Effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.html)
by Conor McBride and Ross Paterson. The syntax is somewhat borrowed from
[idioms notation](http://www.cs.st-andrews.ac.uk/~eb/Idris/donotation.html) of
[Idris](http://idris-lang.org) programming language.

`scala-idioms` is built around [untyped macros](http://docs.scala-lang.org/overviews/macros/untypedmacros.html)
that is an experimental feature of [Macro Paradise](http://docs.scala-lang.org/overviews/macros/paradise.html).

![Travis CI Status](https://api.travis-ci.org/aztek/scala-idioms.png)

Quick examples
--------------
```scala
import idioms._

idiom[Option] {
  $(Some(42) + 1) should equal (Some(43))
  $(Some(10) + Some(5) * Some(2)) should equal (Some(20))
}

idiom[List] {
  $(List(1, 2, 3) * 2) should equal (List(2, 4, 6))
  $(List("a", "b") + List("x", "y")) should equal (List("ax", "ay", "bx", "by"))
}

idiom(zipList) {
  $(List(1, 2, 3, 4) * List(2, 3, 4)) should equal (List(2, 6, 12))
}

idiom(function[String]) {
  val chars   = (s: String) ⇒ s.length
  val letters = (s: String) ⇒ s.count(_.isLetter)
  val nonletters = $(chars - letters)
  nonletters("R2-D2") should equal (3)
}

idiom(map[String]) {
  $(Map("foo" → 10, "bar" → 5) * 2) should equal (Map("foo" → 20, "bar" → 10))
}
```

What's an idiom?
----------------
Following the original paper, idioms (also called _applicative functors_) are
algebraic structures with two operators `pure` and `app` of certain types, that
are actively used in Haskell and other functional programming languages to
express computations with effects. E.g., `Option` and `List` are examples of
idioms, representing, correspondingly, flawed and non-deterministic
computations.

In the same paper, _idiom brackets_ is a fairly trivial syntactic
transformation, that wraps a function in `pure` call and replace each function
application with `app` call. That way, only a single function, applied to idiom
instances, is supposed to be put inside the brackets.

`scala-idioms` tries to bring this notion to Scala with several enhancements.

Idioms hierarchy
----------------
Instead of a fixed applicative functor class, `scala-idioms` offers a refined
hierarchy of contexts, starting with the trait `Functor[F[_]]`.

```scala
trait Functor[F[_]] {
  def map[A, B](f: A ⇒ B): F[A] ⇒ F[B]
}
```

This `map` is the same `map` there is in `List`, `Option` and other standard
Scala classes. Of course, `scala-idioms` already has `Functor` instance for
them all. You can define your own instance for, say, tuples of two elements.

```scala
val intTuple = new Functor[({type λ[α] = (Int, α)})#λ] {
  def map[A, B](f: A ⇒ B) = { case (lhs, rhs) ⇒ (lhs, f(rhs)) }
}
```

The hacky thing with `#λ` is called _type lambda_ and is used to define type
constructor (sadly, you can't just write something like `(Int, _)` in Scala).

That instance now can be used to define _idiomatic context_, i.e. a set of
rules, that will be applied to change semantics of function application inside
idiom brackets (that look like `$` macro call). For example,

```scala
idiom(intTuple) {
  val foo = (42, "foo")
  $(foo + "bar")
}
```

will produce `(42, "foobar")`. Writing `$((42, "foo") + (13, "bar"))`, however,
will produce an error, because there's no way for `scala-idioms` to decide,
what should be done with `42` and `13` (addition, multiplication?).

Say, we want it to be addition. For that, an extention of `Functor[F[_]]`,
called `SemiIdiom[F[_]]` is needed.

```scala
trait SemiIdiom[F[_]] extends Functor[F] {
  def app[A, B](f: F[A ⇒ B]): F[A] ⇒ F[B]
}
```

Semi-idiom basically defines a way to `map` with a function of more then one
argument. Semi-idiom instance for a tuple migth look like

```scala
val intTuple = new SemiIdiom[({type λ[α] = (Int, α)})#λ] {
  def map[A, B](f: A ⇒ B) = { case (lhs, rhs) ⇒ (lhs, f(rhs)) }
  def app[A, B](ff: (Int, A ⇒ B)) = {
    case (i, a) ⇒
      val (i2, f) = ff
      (i + i2, f(a))
  }
}
```

So now

```scala
idiom(intTuple) {
  val foo = (42, "foo")
  val bar = (13, "bar")
  $(foo + bar)
}
```

will be `(55, "foobar")`. Neat!

But there's one silly case, where semi-idiom `intTuple` still wouldn't work.
This one: `$("foo")`. Again, `scala-idioms` has no idea of what should be put
as the first element of a tuple. There's another extension to `Functor[F[_]]`,
called `Pointed[F[_]]`, that is used to tell precisely that.

```scala
trait Pointed[F[_]] extends Functor[F] {
  def pure[A](a: ⇒ A): F[A]
}
```

Finally, there's `Idiom[F[_]]` trait, that incorporates all the methods from
`Functor[F[_]]`, `SemiIdiom[F[_]]` and `Pointed[F[_]]`. You can see now, how
instances of `Idiom[F[_]]` are the most powerful ones — they have no
restrictions on the number of idiom values inside the brackets.

```scala
trait Idiom[F[_]] extends SemiIdiom[F] with Pointed[F]
```

Most built-in instances in `scala-idioms` are `Idiom`s and indeen we can define
idiom for tuple.

```scala
val intTuple = new Idiom[({type λ[α] = (Int, α)})#λ] {
  def map[A, B](f: A ⇒ B) = { case (lhs, rhs) ⇒ (lhs, f(rhs)) }
  def pure[A](a: ⇒ A) = (0, a)
  def app[A, B](ff: (Int, A ⇒ B)) = {
    case (i, a) ⇒
      val (i2, f) = ff
      (i + i2, f(a))
  }
}
```

Now it can be used everywhere.

```scala
idiom(intTuple) {
  val foo = (42, "foo")
  val bar = (13, "bar")
  $(foo + "bar") should equal (42, "foobar")
  $("qux") should equal (0, "qux")
  $(foo + bar) should equal (55, "foobar")
}
```

Note, that refined idiom hierarchy is introduced because not all things, that
we would like to work with via idiom brackets, are applicative functors. You can
find examples of semi-idioms that are not idioms and fuctors that are not
semi-idioms in `SemiIdiomInstances` and `FunctorInstances` traits.

How does it work?
-----------------
Idiom brackets in `scala-idioms` are more flexible regarding the structure of
the expression inside and currently support arbitrarily deeply nested function
applications. For that, some extra work needs to be done, besides idiom method
calls generation.

Since we can't just lift function arguments into the idiomatic context, the
main question is, how to decide which subexpressions should be lifted.

Current implementation uses untyped macros and takes untyped Scala AST as an
argument. Then we start by eagerly typechecking all the subexpressions (i.e.,
starting with the most common subexpressions) and find, which of them
typechecks successfully and the result type corresponds to the type of the
idiom.

Consider the example below.

```scala
idiom(option) {
  $(2 * 3 + Some(10) * Some(5))
}
```

The whole expression does not typecheck. `2 * 3` typechecks to `Int` and
therefore doesn't need to be lifted. `Some(10) + Some(5)`, does not typecheck,
but each of its arguments typechecks to `Option[Int]` (well, technically,
`Some[Int]`, but we can handle that), so both arguments get lifted.

Generated code will look like this.

```scala
option.app(option.map((x$1: Int) ⇒ (x$2: Int) ⇒ 2 * 3 + x$1 * x$2)(Some(10)))(Some(5))
```

It is useful to observe, how the number of idiom values inside the brackets
affects the produced code and sets constrains on the enclosing idiomatic
context.

- No idiom values inside produce `pure` method call (and therefore require
  idiomatic context to implement `Pointed`)

- One idiom value inside produces `map` method call (and therefore require
  idiomatic context to implement `Functor`)

- More than one idiom value produce `map` and `app` methods (and therefore
  require idiomatic context to implement `SemiIdiom`)

Syntax of idioms
----------------
Idiom block is defined with `idiom` macro that either takes an idiom instance
as an argument, or a type constructor `F[_]`, such that there is some idiom
instance (`Functor[F]`, `SemiIdiom[F]`, `Pointed[F]` or `Idiom[F]`) defined
somewhere in the implicits scope.

The following examples are equivalent.

```scala
import idioms._

idiom[List] {
  $(List(2, 5) * List(3, 7))
}

idiom(list) {
  $(List(2, 5) * List(3, 7))
}
```

There are plenty of built-in idiom instances in traits `FunctorInstances`,
`SemiIdiomInstances` and `IdiomInstances`. They are all mixed to the package
object of `scala.idioms`, so once you have `scala.idioms._` imported, you get
access to them all. Alternatively, you can import just the macros
`import idioms.{idiom, $}` and access idiom instances from `Functor`,
`SemiIdiom` and `Idiom` objects.

Macro `$` takes idiomatic context from the closest `idiom` block.
Alternatively, you can provide type constructor, whose idiom instance will be
taken from the implicits scope.

```scala
$[List](List(2, 5) * List(3, 7))
```

That way, `$` will disregard any enclosing `idiom` block and will work within
`Idiom[List]` context.

Idioms composition
------------------
Functors, pointeds, semi-idioms and idioms are all composable. There are
special _idiom composition_ classes (`FunctorT`, `PointedT`, `SemiIdiomT` and
`IdiomT` correspondingly) that allow you, say, having instances of `Idiom[F]`
and `Idiom[G]`, to get instance of `Idiom[F[G]]`. You can either create
`IdiomT` object directly with class constructor, or use `$` method of the class
`Idiom`. You can also combine idioms of different classes with the same syntax,
the result idiom will be of the same class as the weaker argument.

```scala
idiom(list $ option) {
  val xs = List(Some(2), Some(3), None)
  $(xs * 10) should equal (List(Some(20), Some(30), None))
}
```

Currently, you can only combine idioms using terms, but not types (i.e. that
would be really cool to be able to write `idiom[List $ Option]`, but that
feature is not supported yet).

When are idioms useful?
-----------------------
Short answer, they might be a more concise substitute of `for`-expressions.
Consider the example below, where we parse JSON string to some `Person` object.

```scala
def parse(json: Json): Option[Person] =
  for {
    name ← parseName(json)
    birthday ← parseBirthday(json)
  } yield {
    val id = parseId(json)
    val dept = parseDept(json)
    Person(id, name, new Date(birthday), dept)
  }
```

Note, how the syntax gets rather awkward, when we separate pure values from
values, extracted from idiomatic context. With idiom brackets we can compose
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
### Evaluator for a language of expressions
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
  idiom(function[Env] $ option) {
    case Var(x) ⇒ fetch(x)
    case Val(value) ⇒ $(value)
    case Add(x, y) ⇒ $(eval(x) + eval(y))
  }
```

### Functional reactive programming
`scala-idioms` can be used as syntactic sugar for external libraries and
programming paradigms. In this example, a very simple version of [Functional
reactive programming](http://en.wikipedia.org/wiki/Functional_reactive_programming)
framework is implemented as `Idiom` instance.

`Cell` trait defines a unit of data, that can be assigned with `:=` and fetched
with `!`. Cells can depend on each other's values, much like they do in
spreadsheets.

```scala
trait Cell[T] {
  def ! : T
  def := (value: T) { throw new UnsupportedOperationException }
}
```

Idiom instance defines cells, that either contain atomic value that can be
reassign or dependent cells, that take value of some other cell to
compute their own (reassigning them doesn't make sense, thus the exception).

```scala
val frp = new Idiom[Cell] {
  def pure[A](a: ⇒ A) = new Cell[A] {
    private var value = a
    override def := (a: A) { value = a }
    def ! = value
  }
  def map[A, B](f: A ⇒ B) = a ⇒ new Cell[B] {
    def ! = f(a!)
  }
  def app[A, B](f: Cell[A ⇒ B]) = a ⇒ new Cell[B] {
    def ! = f!(a!)
  }
}
```

With that instance we can organize reactive computations with simple syntax.

```scala
idiom (frp) {
  val a = $(10)
  val b = $(5)

  val c = $(a + b * 2)

  (c!) should equal (20)

  b := 7

  (c!) should equal (24)
}
```

Contributions
-------------
This project is still very experimental and comments and suggestions are highly
appreciated. Drop me a line [on twitter](http://twitter.com/aztek) or
[by email](mailto:evgeny.kotelnikov@gmail.com), or [open an issue](https://github.com/aztek/scala-idioms/issues/new)
here on GitHub. I'm also occasionally on #scala IRC channel on Freenode.
