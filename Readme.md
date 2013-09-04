Scala workflow [![Travis CI Status](https://api.travis-ci.org/aztek/scala-workflow.png)](https://travis-ci.org/aztek/scala-workflow)
==============
`scala-workflow` helps to nicely organize applicative and monadic computations
in Scala with 2.11 macros, resembling _`for`-comprehension_ and some enhanced
version of _idiom brackets_.

`scala-workflow` only requires [untyped macros](http://docs.scala-lang.org/overviews/macros/untypedmacros.html)
that is an experimental feature of [Macro Paradise](http://docs.scala-lang.org/overviews/macros/paradise.html).

```
$ git clone https://github.com/aztek/scala-workflow
$ cd scala-workflow
$ sbt console
Welcome to Scala version 2.11.0
Type in expressions to have them evaluated.
Type :help for more information.

scala> import workflow._
import workflow._

scala> $[List](List(1, 2) * List(4, 5))
res0: List[Int] = List(4, 5, 8, 10)
```

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
    *   [Purely functional logging](#purely-functional-logging)
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
several derived traits, that add methods to it. So far there are four of them:

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

- `map` is used to map over one lifted value in an expression. This is the
  same `map` you can find in Scalas `List`, `Option` and other classes.  

- `app` is used to map over more that one independently lifted values within a
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
can be [composed](#composing-workflows).

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
workflow. If those are found, they are being replaces with their non-lifted
counterparts, and the whole thing starts over again, until the whole expression
typechecks correctly and there is a list of lifted values at hand.

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

#### Simple expressions
<table>
   <tr>
      <th>Inside the <code>$</code></th>
      <th>Generated code</th>
      <th>Pure Scala counterpart</th>
   </tr>
   <tr>
      <td><code>$(42)</code></td>
      <td><pre>option.point(42)</pre></td>
      <td><pre>Some(42)</pre></th>
   </tr>
   <tr>
      <td><code>$(Some(42) + 1)</code></td>
      <td><pre>option.map(
  (x$1: Int) ⇒
    x$1 + 1
)(Some(42))</pre></td>
      <td><pre>for {
  x ← Some(42)
} yield x + 1</pre>      
      </td>
   </tr>
   <tr>
      <td><code>$(Some(2) * Some(3))</code></td>
      <td><pre>option.app(
  option.map(
    (x$1: Int) ⇒
      (x$2: Int) ⇒
        x$1 * x$2
  )(Some(2))
)(Some(3))</pre></td>
      <td><pre>for {
  x ← Some(2)
  y ← Some(3)
} yield x * y</pre>      
      </td>
   </tr>
   <tr>
      <td><code>$(divide(1, 2))</code></td>
      <td><pre>divide(1, 2)</pre></td>
      <td><pre>divide(1, 2)</pre></td>
   </tr>
   <tr>
      <td><code>$(divide(Some(1.5), 2))</code></td>
      <td><pre>option.bind(
  (x$1: Double) ⇒
    divide(x$1, 2)
)(Some(1.5))</pre></td>
      <td><pre>for {
  x ← Some(1.5)
  y ← divide(x, 2)
} yield y</pre>      
      </td>
   </tr>
   <tr>
      <td><code>$(divide(Some(1.5), Some(2)))</code></td>
      <td><pre>option.bind(
  (x$1: Double) ⇒
    option.bind(
      (x$2: Int) ⇒
        divide(x$1, x$2)
    )(Some(2))
)(Some(1.5))</pre></td>
      <td><pre>for {
  x ← Some(1.5)
  y ← Some(2)
  z ← divide(x, y)
} yield z</pre>      
      </td>
   </tr>
   <tr>
      <td><code>$(divide(Some(1.5), 2) + 1)</code></td>
      <td><pre>option.bind(
  (x$1: Double) ⇒
    option.map(
      (x$2: Double) ⇒
        x$2 + 1
    )(divide(x$1, 2))
)(Some(1.5))</pre></td>
      <td><pre>for {
  x ← Some(1.5)
  y ← divide(x, 2)
} yield y + 1</pre>      
      </td>
   </tr>
</table>

#### Blocks of code
<table>
   <tr>
      <th>Inside the <code>$</code></th>
      <th>Generated code</th>
      <th>Pure Scala counterpart</th>
   </tr>
   <tr>
      <td><pre>$ {
  val x = Some(10)
  x + 2
}</pre></td>
      <td><pre>option.map(
  (x$1: Int) ⇒
    x$1 + 2
)(Some(10))</pre></td>
      <td><pre>for {
  x ← Some(10)
} yield x + 2</pre></td>
   </tr>
   <tr>
      <td><pre>$ {
  val x = Some(10)
  val y = Some(5)
  x + y
}</pre></td>
      <td><pre>option.bind(
  (x$1: Int) ⇒
    option.map(
      (x$2: Int) ⇒
        x$1 + x$2
    )(Some(5))
)(Some(10))</pre></td>
      <td><pre>for {
  x ← Some(10)
  y ← Some(5)
} yield x + y</pre></td>
   </tr>
   <tr>
      <td><pre>$ {
  val x = Some(10)
  val y = x − 3
  x * y
}</pre></td>
      <td><pre>option.map(
  (x$1: Int) ⇒
    val y = x$1 − 3
    x$1 * y
)(Some(10))</pre></td>
      <td><pre>for {
  x ← Some(10)
  y = x - 3
} yield x * y</pre></td>
   </tr>
   <tr>
      <td><pre>$(2 + {
  val x = Some(10)
  x * 2
})</pre></td>
      <td><pre>option.map(
  (x$1: Int) ⇒
    2 + x$1
)(option.map(
  (x$2: Int) ⇒
    x$2 * 2
)(Some(10)))</pre></td>
      <td><pre>2 + (for {
  x ← Some(10)
} yield x * 2)</pre></td>
   </tr>
</table>

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
imported, you get access to all of them. Alternatively, you can import just the
macros `import workflow.{context, workflow, $}` and access workflow instances
from `Functor`, `SemiIdiom`, `Idiom` and `Monad` objects.

### Composing workflows
Any pair of functors, semi-idioms or idioms can be composed. That is, for any
type constructors `F[_]` and `G[_]` one can build instances of `Functor[F[G[_]]]`
and `Functor[G[F[_]]]` with instances of `Functor[F]` and `Functor[G]` (the same
for semi-idioms and idioms).

The syntax for workflow composition is `f $ g`, where `f` and `g` are functor,
semi-idiom or idiom instances of type constructors `F[_]` and `G[_]`. The result
would be, correspondingly, functor, semi-idiom or idiom of type constructor
`F[G[_]]`.

```scala
context(list $ option) {
  $(List(Some(2), Some(3), None) * 10) should equal (List(Some(20), Some(30), None))
}
```

You can also combine workflows of different classes with the same syntax, the
result workflow will implement the weaker interface of the two. For instance,
`map[String] $ option` will implement `Functor`, because `map`s `Functor` is
weaker than `option`s `Monad`.

`$` method has a counterpart method `&`, that produces `G[F[_]]` workflow
instance. Naturally, `f $ g = g & f`.

Monads and semi-monads in general cannot be composed. Instead, some particular
monads can provide their own specific implementation of `$` or `&` method.
However, even having, say, `$` method defined, monad might not be able to
define `&`. That is, `Monad[F]` can either construct `Monad[F[G[_]]` or
`Monad[G[F[_]]]` with arbitrary `Monad[G]`.

To capture this notion, `Monad[F]` can be extended to either
`LeftComposableMonad[F]` or `RightComposableMonad[F]`. In the first case it is
supposed to be able to produce `Monad[F[G[_]]]` and therefore implement `$`
method. In the second case it is supposed to be able to produce `Monad[G[F[_]]]`
and therefore implement `$` method.

For example, `Option` is _right-composable_, i.e. can implement
`def & [G[_]](g: Monad[G]): Monad[G[Option[_]]]`, whereas
function monad `R ⇒ _` is _left-composable_ and can implement
`def $ [G[_]](g: Monad[G]): Monad[R ⇒ G[_]]`.

So, for monads `f` and `g` their composition `f $ g` will be a monad either
when `f` is left-composable or `g` is right-composable or visa versa for `&`.
When in the expression `f $ g` `f` is left-composable and `g` is right-composable,
`f`'s `$` method will be used to construct a composition. When in the expression
`g & f` `f` is left-composable and `g` is right-composable, `g`'s `&` method will
be used to construct a composition.

The same holds for semi-monads.

Check [`instances.scala`](https://github.com/aztek/scala-workflow/blob/master/core/src/main/scala/scala/workflow/instances.scala)
to ensure, which monads are left- or right- composable.

Methods `$` and `&` are called _monad transformer_ elsewhere, although
separation of left- and right- composability is usually not introduced.

Examples
--------
### Evaluator for a language of expressions
[Original paper](http://strictlypositive.org/IdiomLite.pdf) by McBride and
Patterson, that introduced idiom brackets, describes a simple evaluator for a
language of expressions. Following that example, here's how it would look like
here.

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

We represent stack as a regular `List`, and the result of a program, that
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

Aside from mind-boggling examples like that, there's actually a useful
application for these workflows — they can be used in point-free notation,
i.e. for construction of complex functions with no function arguments
specified.

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

### Purely functional logging
It is no secret for a functional programmer that monads are extremely powerful.
In fact, most of the features of imperative programming languages, that are
usually implemented with variations of mutable state and uncontrolled side
effects can be expressed with monads. However, functional purity often comes
with the price of rather cumbersome syntax, compared to equivalent imperative
constructs.

`scala-workflow` tries to bridge this gap. In this example, `Writer` monad is
used to allow snippets of imperative-looking code to mix pure computations and
logging in purely functional manner.

We start with a simple definition of a [semigroup](http://en.wikipedia.org/wiki/Semigroup)
and a [monoid](http://en.wikipedia.org/wiki/Monoid).

```scala
trait Semigroup[A] {
  def append: (A, A) ⇒ A
}

trait Monoid[A] extends Semigroup[A] {
  val unit: A
}
```

`Writer` monad represents computations, those results are accompanied with some
accumulated output. The value, produced by such computations is represented
with an auxiliary `Writer` class that contains the result and the output that
is made a monoid instance to capture the notion of "accumulation" (semigroup
would suffice just for accumulation, but we would also like to produce results
with empty output, hence a monoid).

```scala
case class Writer[R, O : Monoid](result: R, output: O)
```

A log file is basically a list of log entries, so we model it as is.

```scala
case class Log(entries: List[String])
```

It is of course possible to define monoid instance for this class, because
lists are themselves monoids.

```scala
implicit val logMonoid = new Monoid[Log] {
  val unit = Log(Nil)
  def append = {
    case (Log(oldEntries), Log(newEntries)) ⇒ Log(oldEntries ++ newEntries)
  }
}
```

With this instance defined in the implicits scope we can now construct
`writer[Log]` workflow. `log` function will lift log entry to `Writer` context.

```scala
def log(message: String) = Writer[Unit, Log]({}, Log(List(message)))
```

Having a snippet of code wrapped in `workflow(writer[Log])`, we can intersperse
pure computations with writing to a log, much like we would do with `log4j` or
similar framework.

```scala
val Writer(result, logEntries) = workflow(writer[Log]) {
  log("Lets define a variable")
  val x = 2

  log("And calculate a square of it")
  val square = x * x

  log("Also a cube and add them together")
  val cube = x * x * x
  val sum = square + cube

  log("This is all so silly")
  30 / 5
}
```

The result of computation is now stored in `result` and the log is in
`logEntries`. Note, that no side effects or mutable state whatsoever were
involved in this example.

```scala
result should equal (6)
logEntries should equal (Log(List("Lets define a variable",
                                  "And calculate a square of it",
                                  "Also a cube and add them together",
                                  "This is all so silly")))
```

Disclaimer
----------
This project is very experimental and your comments and suggestions are highly
appreciated. Drop me a line [on twitter](http://twitter.com/aztek) or
[by email](mailto:evgeny.kotelnikov@gmail.com), or [open an issue](./issues/new)
here on GitHub. I'm also occasionally on #scala IRC channel on Freenode.
