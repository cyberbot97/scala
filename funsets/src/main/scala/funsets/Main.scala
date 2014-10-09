package funsets

import math.abs

object Main extends App {
  import FunSets._
  //println(contains(singletonSet(1), 1))

  val tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < tolerance
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      println(next)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }


  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y))(1.0)

  def ttt = (y: Int) => y + y

  println(ttt(10))

  //def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2 )(1.0)
  sqrt(2)
  println(averageDamp(x => x/x)(1.0))

  //def abs(x: Int) = if (x >= 0 ) x else -x

  def loop: Boolean = {println(1); loop}

  def x = loop

  //val y = loop

  def cube(x: Int) = x * x * x

  def sum(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sum(f, a + 1, b)

  def sumInts(a: Int, b: Int) = sum(x => x, a, b)

  println(sumInts(1, 10))
  println(sum(x => x, 1, 10))

  def curried(x: Int)(y: Int) = x / y

  def first(x: Int) = (y: Int) => x / y

  val second = first(10)

  println(second(2))



}
