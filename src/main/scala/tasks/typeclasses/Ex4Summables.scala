package tasks.typeclasses
import u03.Sequences.*
import Sequence.*

import scala.annotation.tailrec

/*  Exercise 4: 
 *  - Complete the implementation of ad-hoc polymorphic sumAll, using summable.sum and summable.zero
 *  - Write givens also for Summable[Double], Summable[String]
 *  - Uncomment in the main and check if everything works
 */

object Ex4Summables:

  def sumAllInt(seq: Sequence[Int]): Int = seq match
    case Cons(h, t) => h + sumAllInt(t)
    case _ => 0

  trait Summable[A]:
    def sum(a1: A, a2: A): A
    def zero: A

  def sumAll[A: Summable](seq: Sequence[A]): A =
    val summable = summon[Summable[A]]
    @tailrec
    def _sumAll(seq: Sequence[A], acc: A): A = seq match
      case Cons(h, t) => _sumAll(t, summable.sum(h, acc))
      case _ => acc
    _sumAll(seq, summable.zero)

  given Summable[Int] with
    def sum(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  
  // write givens for Summable[Double] and Summable[String]

  given Summable[Double] with
    def sum(a1: Double, a2: Double): Double = a1 + a2
    def zero: Double = 0.0

  given Summable[String] with
    def sum(a1: String, a2: String): String = a2 + a1
    def zero: String = ""