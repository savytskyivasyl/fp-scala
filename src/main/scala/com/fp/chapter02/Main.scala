package com.fp.chapter02

import scala.annotation.tailrec

object Main {
  //2.1
  def fib(n: Int): Int = {
    @tailrec
    def step(n: Int, m1: Int, m2: Int): Int = {
      if(n <= 1) m1
      else step(n - 1, m2, m1 + m2)
    }
    step(n, 0, 1)
  }

  //2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) return true
      if(!ordered(as(n), as(n + 1))) return false
      loop(n + 1)
    }

    loop(0)
  }

  //2.3
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = b => f(a, b)
  def curry[A,B,C](f: (A,B) => C): A => B => C = a => b => f(a, b)

  //2.4*
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  //2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  def main(args: Array[String]): Unit = {
    def test(a: Int, b: Long) = (a + b).toString
    println(1 until 20 map fib)

    val s1 = partial1(5, test)(6)
    println(s1)

    val s2 = curry(test)(5)(6)
    println(s2)

    val s3 = uncurry(curry(test))(5, 7)
    println(s3)

    println(isSorted(Array(1, 2, 3, 4, 5), (x: Int, y: Int) => x < y))
  }
}
