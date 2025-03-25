package tasks.typeclasses

import org.junit.*
import org.junit.Assert.*
import u03.Sequences.*
import Sequence.*
import tasks.typeclasses.Ex4Summables.*

class SummablesTest:
  
  val si: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  val sd: Sequence[Double] = Cons(10.0, Cons(20.0, Cons(30.0, Nil())))
  val ss: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))

  @Test def testSumAllInt() =
    assertEquals(60, sumAllInt(si))

  @Test def testSumAllWithInt() =
    assertEquals(60, sumAll(si))

  @Test def testSumAllWithDouble() =
    assertEquals(60.0, sumAll(sd), 0)

  @Test def testSumAllWithString() =
    assertEquals("102030", sumAll(ss))
