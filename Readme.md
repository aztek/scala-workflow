Scala workflow
==============
`scala-workflow` helps to nicely organize applicative and monadic computations
in Scala with 2.11 macros, resembling _`for`-comprehension_ and some enhanced
version of _idiom brackets_.

`scala-workflow` only requires [untyped macros](http://docs.scala-lang.org/overviews/macros/untypedmacros.html)
that is an experimental feature of [Macro Paradise](http://docs.scala-lang.org/overviews/macros/paradise.html).

![Travis CI Status](https://api.travis-ci.org/aztek/scala-idioms.png)

Contents
--------
*   [Quick start](#quick-start)
*   [Workflows](#workflows)
    *   [Hierarchy of workflows](#hierarchy-of-workflows)
    *   [Rewriting rules](#rewriting-rules)
    *   [Context definition](#context-definition)
    *   [Composing workflows](#composing-workflows)
*   [Examples](#examples)
    *   [Evaluator for a language of expressions](#evaluator-for-a-language-of-expressions)
    *   [Functional reactive programming](#functional-reactive-programming)
    *   [Monadic interpreter for stack programming language](#monadic-interpreter-for-stack-programming-language)
    *   [Point-free notation](#point-free-notation)
*   [Disclaimer](#disclaimer)

Quick start
-----------
Import `workflow` interface.

```scala
import workflow._
```

You will now have three macros at hand — `context`, `$` and `workflow`. Wrap a
block of code in `context` and the argument of `$` will begin to act funny.

```scala
context[Option] {
  $(Some(42) + 1) should equal (Some(43))
  $(Some(10) + Some(5) * Some(2)) should equal (Some(20))
}

context[List] {
  $(List(1, 2, 3) * 2) should equal (List(2, 4, 6))
  $(List("a", "b") + List("x", "y")) should equal (List("ax", "ay", "bx", "by"))
}
```

Enclosing `context` macro takes type constructor `F[_]` as an argument and
makes all the `$`s inside evaluate their arguments as if everything of type
`F[T]` there was of type `T`, except it retains _computational effects_,
associated with `F`.

The exact rules of evaluation are defined in an object, that extends special
`scala.workflow.Workflow[F[_]]` trait. Such objects can either be implicitly
summoned by type constructor name (like the ones shown above), or passed
explicitly, like `zipList` below.

```scala
context(zipList) {
  $(List(1, 2, 3, 4) * List(2, 3, 4)) should equal (List(2, 6, 12))
}
```

There are numerous of `Workflow[F[_]]` objects you can find in `instances.scala`
file. All of them are instances of _functors_, _applicative functors_ (also
called _idioms_), _monads_ and a couple of other intermediate algebraic
structures.

```scala
context(map[String]) {
  $(Map("foo" → 10, "bar" → 5) * 2) should equal (Map("foo" → 20, "bar" → 10))
}

context(function[String]) {
  val chars   = (s: String) ⇒ s.length
  val letters = (s: String) ⇒ s.count(_.isLetter)
  val nonletters = $(chars - letters)
  nonletters("R2-D2") should equal (3)
}
```

You can pass complex blocks of code to `$`.

```scala
def divide(x: Double, y: Double) = if (y == 0) None else Some(x / y)
context[Option] {
  $ {
    val x = divide(1, 2)
    val y = divide(4, x)
    divide(y, x)
  } should equal (Some(16))
}
```

Nested `context` and `$` calls might look awkward, so instead you can use
special syntactic sugar, called `workflow`.

```scala
workflow[Option] {
  val x = divide(1, 2)
  val y = divide(4, x)
  divide(y, x)
} should equal (Some(16))
```

Just like in `context`, you can pass either type constructor or workflow
object.

Workflows
---------
The goal of `scala-workflow` is to provide boilerplate-free syntax for
computations with effects, encoded with monads and idioms. _Workflow_
abstracts the concept of computation in effectful context.

Instances of `Workflow` trait provide methods, that are used for desugaring
code in correspondent effectful contexts. The more methods an instance has,
the more powerful it is, and the richer language features can be used.

The ultimate goal is to support the whole set of Scala language features. For
now, however, only literals, function applications and `val` definitions are
supported. But development of the project is still in progress and you are very
welcome to contribute.

### Hierarchy of workflows
The hierarchy of workflows is built around an empty `Workflow[F[_]]` trait and
several derived traits, that add methods. So far there are four of them:

```scala
trait Pointing[F[_]] extends Workflow[F] {
  def point[A](a: ⇒ A): F[A]
}

trait Mapping[F[_]] extends Workflow[F] {
  def map[A, B](f: A ⇒ B): F[A] ⇒ F[B]
}

trait Applying[F[_]] extends Workflow[F] with Mapping[F] {
  def app[A, B](f: F[A ⇒ B]): F[A] ⇒ F[B]
}

trait Binding[F[_]] extends Workflow[F] {
  def bind[A, B](f: A ⇒ F[B]): F[A] ⇒ F[B]
}
```

Each method corresponds to a particular feature of workflow context.
- `point` allows to put pure value inside the workflow. It is only generated
  when you call `$(a)` for some pure `a`.

- `map` is used to map over one lifted value in the expression. This is the
  same `map` you can find in Scalas `List`, `Option` and other classes.  

- `app` is used to map over more that one independently lifted values within the
  context. "Independently" means that you can evaluate lifted arguments in any
  order. Example of an expression with lifted values that depend on each other:
  `$(divide(divide(1, 2), 3))` (with `divide` definition taken from "Quick start"
  section). Both `divide` calls are lifted, but we can only evaluate outmost
  `divide` after the inner one has been evaluated. Note, that `app` cannot be used
  without `map`, hence the inheritance of the traits.

- `bind` is used to desugar expressions with arbitrary many arbitrary dependent
  lifted values.

You can define workflow instances simply by mixing above-mentioned traits, or
using one of predefined shortcuts, representing commonly used algebraic
structures.

```scala
trait Functor[F[_]] extends Mapping[F]

trait SemiIdiom[F[_]] extends Functor[F] with Applying[F]

trait Idiom[F[_]] extends SemiIdiom[F] with Pointing[F] {
  def map[A, B](f: A ⇒ B) = app(point(f))
}

trait SemiMonad[F[_]] extends SemiIdiom[F] with Binding[F]

trait Monad[F[_]] extends Idiom[F] with Binding[F] {
  def app[A, B](f: F[A ⇒ B]) = bind(a ⇒ bind((g: A ⇒ B) ⇒ point(g(a)))(f))
}
```

Note, that `Functor`/`Idiom`/`Monad` is merely a shortcut. You are not required
to implement any of it particularly to be able to use workflow contexts. They
are mostly convenient, because have some of the methods already implemented and
can be [composable](#composing-workflows).

### Rewriting rules
One important difference of `scala-workflow` from similar syntactic extension
is that it always require the least powerful interface of a workflow instance
for generated code. That means, that you can have idiom brackets kind of syntax
for functors (such as `Map[A, B]`) and `for`/`do`-notation kind of syntax for
monads without `return` (they are called `SemiMonad`s here).  

Current implementation uses untyped macros and takes untyped Scala AST as an
argument. Then we start by eagerly typechecking all the subexpressions (i.e.,
starting with the most nested subexpressions) and find, which of them
typechecks successfully with the result type corresponding to the type of the
workflow. If those are found, they are being replaces with their unlifted
counterparts, and the whole thing starts over again, until the whole expression
typechecks correctly and we have a list of lifted values at hand.

Consider the example below.

```scala
context(option) {
  $(2 * 3 + Some(10) * Some(5))
}
```

All the numbers typecheck and are not inside `Option`, so they are left as is.
`2 * 3` also typechecks to `Int` and is left as is. `Some(10)` and `Some(5)`
both typechecks to `Option[Int]` (well, technically, `Some[Int]`, but we can
handle that), so both arguments are lifted.

Generated code will look like this.

```scala
option.app(
  option.map(
    (x$1: Int) ⇒ (x$2: Int) ⇒
      2 * 3 + x$1 * x$2
  )(Some(10))
)(Some(5))
```

Special analysis takes care of dependencies between lifted values to be sure to
produce `bind` instead of `app` where needed.

Here are some of other examples of code rewriting within `Option` context.

Inside the `$`                  | Compiled code
                            --- | ---
`$(42)`                         | `option.point(42)`
`$(Some(42) + 1)`               | `option.map((x$1: Int) ⇒ x$1 + 1)(Some(42))`
`$(Some(2) * Some(3))`          | `option.app(option.map((x$1: Int) ⇒ (x$2: Int) ⇒ x$1 * x$2)(Some(2)))(Some(3))`
`$(divide(1, 2))`               | `divide(1, 2)`
`$(divide(Some(1.5), 2))`       | `option.bind((x$1: Double) ⇒ divide(x$1, 2))(Some(1.5))`
`$(divide(Some(1.5), Some(2)))` | `option.bind((x$1: Double) ⇒ option.bind((x$2: Int) ⇒ divide(x$1, x$2))(Some(2)))(Some(1.5))`
`$(divide(Some(1.5), 2) + 1)`   | `option.bind((x$1: Double) ⇒ option.map((x$2: Double) ⇒ x$2 + 1)(divide(x$1, 2)))(Some(1.5))`

### Context definition
Workflow context is defined with `context` macro that either takes a workflow
instance as an argument, or a type constructor `F[_]`, such that there is some
workflow instance defined somewhere in the implicits scope.

The following examples are equivalent.

```scala
context[List] {
  $(List(2, 5) * List(3, 7))
}

context(list) {
  $(List(2, 5) * List(3, 7))
}
```

Macro `$` takes workflow context from the closest `context` block.
Alternatively, you can provide type constructor, whose workflow instance will
be taken from the implicits scope.

```scala
$[List](List(2, 5) * List(3, 7))
```

That way, `$` will disregard any enclosing `context` block and will work within
`Workflow[List]` context.

Nested applications of `context` and `$` can be replaced with `workflow` macro.
You are encouraged to do so for complex blocks of code. You are discourage to
use `$` inside. `workflow` either takes a workflow instance as an argument, or
a type constructor `F[_]` and rewrites the block of code in the same way as `$`.

```scala
workflow(list) { List(2, 5) * List(3, 7) }
```

There are plenty of built-in workflow instances in traits `FunctorInstances`,
`SemiIdiomInstances`, `IdiomInstances` and `MonadInstances`. They are all
mixed to the package object of `scala.workflow`, so once you have `workflow._`
imported, you get access to them all. Alternatively, you can import just the
macros `import workflow.{context, workflow, $}` and access workflow instances
from `Functor`, `SemiIdiom`, `Idiom` and `Monad` objects.

### Composing workflows
Functors, semi-idioms and idioms are all composable, monads and semi-monads are
not. There are special composition classes (`FunctorCompose`, `SemiIdiomCompose`
and `IdiomCompose` correspondingly) that allow you, say, having instances of
`Idiom[F]` and `Idiom[G]`, to get instance of `Idiom[F[G]]`. You can either
create `IdiomCompose` object directly with class constructor, or use `$` method
of the class `Idiom`.

```scala
context(list $ option) {
  $(List(Some(2), Some(3), None) * 10) should equal (List(Some(20), Some(30), None))
}
```

You can also combine workflows of different classes with the same syntax, the
result workflow will implement the weaker interface of the two. For instance,
`map[String] $ option` will implement `Functor`, because it's weaker than
`option`'s `Monad`.

Examples
--------
### Evaluator for a language of expressions
[Original paper](http://strictlypositive.org/IdiomLite.pdf) about idiom brackets
by McBride and Patterson describes a simple evaluator for a language of
expressions. Following that example, here's how it would look like here.

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
wrapped into the workflow `Env ⇒ Option[_]`, which can either be constructed
by hand, or composed of `Env ⇒ _` and `Option` workflows.

```scala
def eval: Expr ⇒ Env ⇒ Option[Int] =
  context(function[Env] $ option) {
    case Var(x) ⇒ fetch(x)
    case Val(value) ⇒ $(value)
    case Add(x, y) ⇒ $(eval(x) + eval(y))
  }
```

### Functional reactive programming
`scala-workflow` can be used as syntactic sugar for external libraries and
programming paradigms. In this example, a very simple version of [Functional
reactive programming](http://en.wikipedia.org/wiki/Functional_reactive_programming)
framework is implemented as `Idiom` instance.

`Cell` trait defines a unit of data, that can be assigned with `:=` and fetched
with `!`. Cells can depend on each others values, much like they do in
spreadsheets.

```scala
trait Cell[T] {
  def ! : T
  def := (value: T) { throw new UnsupportedOperationException }
}
```

Workflow instance, implemented as `Idiom`, defines cells, that either contain
atomic value that can be reassign or dependent cells, that take value of some
other cell to compute their own (reassigning them doesn't make sense, hence
the exception).

```scala
val frp = new Idiom[Cell] {
  def point[A](a: ⇒ A) = new Cell[A] {
    private var value = a
    override def := (a: A) { value = a }
    def ! = value
  }
  def app[A, B](f: Cell[A ⇒ B]) = a ⇒ new Cell[B] {
    def ! = f!(a!)
  }
}
```

With that instance we can organize reactive computations with simple syntax.

```scala
context(frp) {
  val a = $(10)
  val b = $(5)

  val c = $(a + b * 2)

  (c!) should equal (20)

  b := 7

  (c!) should equal (24)
}
```

### Monadic interpreter for stack programming language
If you enjoy embedding monadic domain-specific languages in your Scala programs,
you might like syntactical perks `scala-workflow` could bring. Consider a
little embedded stack programming language.

We represent stack as a regular List, and the result of a program, that
manipulates with a stack, as either a modified stack or an error message
(such as "stack underflow").

```scala
type Stack = List[Int]
type Result = Either[String, Stack]
```

The evaluation of the program will use `state` monad, that will disregard
the result of any command, but preserve the state of the stack.

```scala
val stackLang = state[Result]

def execute(program: State[Unit, Result]) = program.state(Right(Nil))
```

We would like to define stack operators as `Stack ⇒ Result` functions. To be
able to use them within some `workflow`, we need to lift them into the monad.

```scala
def command(f: Stack ⇒ Result) = State[Unit, Result](st ⇒ ({}, right[String].bind(f)(st)))
```

With `command` helper we can now define a bunch of commands, working with stack.

```scala
def put(value: Int) = command {
  case stack ⇒ Right(value :: stack)
}

def dup = command {
  case head :: tail ⇒ Right(head :: head :: tail)
  case _ ⇒ Left("Stack underflow while executing `dup`")
}

def rot = command {
  case a :: b :: stack ⇒ Right(b :: a :: stack)
  case _ ⇒ Left("Stack underflow while executing `rot`")
}

def sub = command {
  case a :: b :: stack ⇒ Right((b - a) :: stack)
  case _ ⇒ Left("Stack underflow while executing `sub`")
}
```

Now, working inside `stackLang` context we can write programs as sequences of
stack commands and execute them to get modified state of the stack.

```scala
context(stackLang) {
  val programA = $ { put(5); dup; put(7); rot; sub }
  execute(programA) should equal(Right(List(2, 5)))

  val programB = $ { put(5); dup; sub; rot; dup }
  execute(programB) should equal(Left("Stack underflow while executing `rot`"))
}
```

### Point-free notation
If you're familiar with [SKI-calculus](http://en.wikipedia.org/wiki/SKI_combinator_calculus),
you might notice, that `point` and `app` methods of `function[R]` workflow 
are in fact `K` and `S` combinators. This means that you can construct any
closed lambda-term (in other words, any function) with just those two methods.

For instance, here's how you can define `I`-combinator (the identity function):

```scala
// I = S K K
def id[T] = function[T].app(function[T ⇒ T].point)(function[T].point)
```

Or `B`-combinator (Scalas `Function.compose` method or Haskells `(.)`):

```scala
// B = S (K S) K
def b[A, B, C] = function[A ⇒ B].app(function[A ⇒ B].point(function[C].app[A, B]))(function[C].point)
// More concisely with map
def b[A, B, C] = function[A ⇒ B].map(function[C].app[A, B])(function[C].point)
```

Aside from mind-wrenching examples like that, there's actually a useful
application for these workflows — they can be used in point-free notation,
i.e. for constructing complex functions without specifying function arguments.

Point-free notation support is rather limited in Scala, compared to Haskell,
but some things still could be done. Here are some examples to get you inspired.

```scala
context(function[Char]) {
  val isLetter: Char ⇒ Boolean = _.isLetter
  val isDigit:  Char ⇒ Boolean = _.isDigit

  // Traditionally
  val isLetterOrDigit = (ch: Char) ⇒ isLetter(ch) || isDigit(ch)

  // Combinatorially
  val isLetterOrDigit = $(isLetter || isDigit)
}
```

Scalas `Function.compose` and `Function.andThen` also come in handy.

```scala
context(function[Double]) {
  val sqrt: Double ⇒ Double = x ⇒ math.sqrt(x)
  val sqr:  Double ⇒ Double = x ⇒ x * x
  val log:  Double ⇒ Double = x ⇒ math.log(x)

  // Traditionally
  val f = (x: Double) ⇒ sqrt((sqr(x) - 1) / (sqr(x) + 1))

  // Combinatorially
  val f = sqrt compose $((sqr - 1) / (sqr + 1))

  // Traditionally
  val g = (x: Double) ⇒ (sqr(log(x)) - 1) / (sqr(log(x)) + 1)

  // Combinatorially
  val g = log andThen $((sqr - 1) / (sqr + 1))
}
```

Disclaimer
----------
This project is very experimental and your comments and suggestions are highly
appreciated. Drop me a line [on twitter](http://twitter.com/aztek) or
[by email](mailto:evgeny.kotelnikov@gmail.com), or [open an issue](./issues/new)
here on GitHub. I'm also occasionally on #scala IRC channel on Freenode.
