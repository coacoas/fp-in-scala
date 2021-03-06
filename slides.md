---
attr_reveal: ':frag t'
author:
- Bill Carlson
email: 'bill@coacoas.net'
reveal: split
reveal_plugins: '(highlight notes)'
reveal_theme: sky
reveal_trans: slide
title: Functional Programming in Scala
---

reveal~titleslide~:nil

Functional Programming
======================

Inspiration
===========

Or, let's face it... copying

[Runar Bjarnasson - Introduction to Functional
Programming](https://www.youtube.com/watch?v=aAtPi23nLcw)

Who am I?
=========

-   Bill Carlson
-   Innovation Developer at Cotiviti Labs
-   I get paid to write purely functional programs!

What is Functional Programming?
===============================

Programming with Functions

What is a Function?
-------------------

A function `f: A => B` maps any value from one set to **exactly one**
value in another set.

*And nothing else*

What does that mean?
--------------------

-   **Totality**: A function will return a single value for all values
    of `A`.
-   **Determinism**: Every time you call a function with the same
    arguments, you will *always* get the same result.
-   **Purity**: The result of the function is the *only* effect.

Referential Tranparency
-----------------------

You can replace all occurrences of a function call with the result of
that call without changing the program.

### Example

``` {.scala}
val string = "some text"
val s1 = string.reverse
val s2 = string.reverse
val s3 = s1 + s2
```

``` {.scala}
val sb = new StringBuilder("some text")
val s1 = sb.reverse
val s2 = sb.reverse
val s3 = s1 append s2
```

What about...
-------------

-   Console?
-   File access?
-   Network?
-   Exceptions?

<div class="NOTES">

We'll get to that...

</div>

Higher-Order Functions
======================

Oh, boy... live code...

<div class="NOTES">

val isEven: Int `> Boolean = i => i % 2 =` 0 def divisibleBy(k: Int):
Int `> Boolean = i -> i % k =` 0 val isEven = divisibleBy(2)

type Predicate\[A\] = A =&gt; Boolean val not\[A\](p: Predicate\[A\]):
Predicate\[A\] = a =&gt; !p(a) val and\[A\](p1: Predicate\[A\], p2:
Predicate\[A\]): Predicate\[A\] = a =&gt; p1(a) && p2(a) val or\[A\](p1:
Predicate\[A\], p2: Predicate\[A\]): Predicate\[A\] = a =&gt; p1(a) ||
p2(a)

val lift\[A\](f: (Boolean, Boolean) =&gt; Boolean): (Predicate\[A\],
Predicate\[A\]) =&gt; Predicate\[A\] = (p1, p2) =&gt; a =&gt; f(p1(a),
p2(a))

val and\[A\] = lift(\_ && **) val or\[A} = lift(** || \_)

</div>

Data Structures
===============

List
----

Empty List: `Nil`

Non-empty list (cons): `A :: List[A]`

To build large lists, just add to a smaller list:

``` {.scala}
val list1 = 2 :: 3 :: 4 :: 5 :: Nil  // List(2, 3, 4, 5)

val list2 = 1 :: list1
```

### It's time for more code!

<div class="NOTES">

def sum(ints: List\[Int\]): Int = ints match { case Nil =&gt; 0 case x
:: xs =&gt; x + sum1(xs) }

def sum2(ints: List\[Int\]): Int = { @annotation.tailrec def loop(acc:
Int, remaining: List\[Int\]): Int = remaining match { case Nil =&gt; acc
case x :: xs =&gt; loop(acc + x, xs) } loop(0, ints) }

What about product? mkString?

Generate foldLeft (and foldRight)

Show how to implement the above using foldLeft

def sum(xs: List\[Int\]): Int = foldLeft(xs, 0)(\_ + **) def product(xs:
List\[Int\]): Int = foldLeft(xs, 1)(** \* **) def mkString(xs:
List\[Int\]): String = foldLeft(xs, "")(** + \_.toString) def
reverse\[A\](xs: List\[A\]): List\[A\] = foldLeft(xs, List.empty\[A\]) {
(acc, i) =&gt; i :: acc }

def map\[A, B\](xs: List\[A\])(f: A =&gt; B): List\[B\] = foldRight(xs,
List.empty\[B\]) { (i, acc) =&gt; f(i) :: acc } def mapl\[A, B\](xs:
List\[A\])(f: A =&gt; B): List\[B\] = reverse(foldLeft(xs,
List.empty\[B\]) { (acc, i) =&gt; f(i) :: acc })

</div>

Option\[A\]
-----------

-   `Some[A]`
-   `None`

``` {.scala}
def fold[A](o: Option[A], z: B)(f: A => B): B = o match {
  case Some(a) => f(a)
  case None => z
}
```

Either\[E, A\]
--------------

-   `Left[E]`
-   `Right[A]`

``` {.scala}
def fold[E, A](e: Either[E, A], z: E => B, f: A => B): B = e match {
  case Left(e) => z(e)
  case Right(a) => f(a)
}
```

Interpreters
============

-   Your program *is* the data structure
-   Your interpreter is the fold
-   [GoF Interpreter
    Pattern](https://www.gofpatterns.com/behavioral-design-patterns/behavioral-patterns/interpreter-pattern.php)

Can this be extended?

Types
=====

What function is this?
----------------------

`def ????(i: Int): Int`

`def inc(i: Int) = i + 1`

`def timesTwo(i: Int) = i * 2`

`def abs(i: Int) = if (i < 0) -i else i`

**???**

`def ????[A](i: A): A`

`def identity[A](i: A): A = i`

<div class="NOTES">

Only true in the case of purity. Otherwise, it could also do something
like:

def bar\[A\](i: A) = i.asInstanceOf\[Int\] + 1

or

def bar\[A}(i: A) = throw new RuntimeException("BWAHAHAHAHA!")

</div>

\[T\]he purpose of abstracting is not to be vague, but to create a new
semantic level in which one can be absolutely precise.

-- Edsger W. Dijkstra, "The Humble Programmer"

Algebras
========

What about...
-------------

-   Console?
-   File access?
-   Network?
-   Exceptions?

<div class="NOTES">

Remember this slide?

</div>

### Console

``` {.scala}
sealed trait Console[A]
case class Print(s: String) extends Console[Unit]
case object Read extends Console[Option[String]]
```

### File

``` {.scala}
sealed trait File[A]
case class Open(p: Path) extends File[Unit]
case class Write(data: Array[Byte]) extends File[Unit]
case object Read extends File[Array[Byte]]
case object Truncate extends File[Unit]
```

### And so on...

An *algebra* is an abstract set of operations

Provides *laws* which must hold true

Using algebras, combinators, and folds, we simplify (evaluate) the
program to a single value.

...maybe a good topic for next time?

?
=

Thank you!
==========

Bill Carlson

<bill@coacoas.net>

[Twitter: @coacoas](https://twitter.com/coacoas)

[<https://github.com/coacoas>](https://github.com/coacoas)
