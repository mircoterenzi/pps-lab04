package tasks.typeclasses
import u03.Sequences.*
import Sequence.*
import u03.Optionals.*
import Optional.*

import scala.annotation.tailrec

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  trait Traversable[T[_]]:
    def accept[A](t: T[A], log: A => Unit): Unit

  def logAll[T[_]: Traversable, A](elem: T[A], log: A => Unit): Unit =
    val traversable = summon[Traversable[T]]
    traversable.accept(elem, log)

  given Traversable[Sequence] with
    def accept[A](t: Sequence[A], log: A => Unit): Unit = t match
      case Cons(h, t) => log(h); logAll(t, log)
      case _ => ()

  given Traversable[Optional] with
    def accept[A](t: Optional[A], log: A => Unit) : Unit = t match
      case Just(a) => log(a)
      case _ => ()

  @main def tryTraversable(): Unit =
    val seq: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
    val opt: Optional[Double] = Just(4.0)
    val emptySeq: Sequence[_] = Nil()
    val emptyOpt: Optional[_] = Empty()

    def log[A](a: A): Unit = println("The next element is: "+a)
    logAll(seq, log) // "The next element is: 10 The next element is: 20 The next element is: 30"
    logAll(opt, log) // "The next element is: 4.0"
    logAll(emptySeq, log) // Nothing is printed
    logAll(emptyOpt, log)
    
    def print() = println(_)
    logAll(seq, print()) // "10 20 30"
    logAll(opt, print()) // "4.0"
    logAll(emptySeq, print())
    logAll(emptyOpt, print())
