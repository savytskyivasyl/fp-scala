package com.fp.chapter05

import com.fp.chapter05.Main.Stream._

import scala.annotation.tailrec

object Main {

  sealed trait Stream[+A] {
    //5.1
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    //5.2
    def take(n: Int): Stream[A] = this match {
      case Empty => empty
      case Cons(h, t) if n == 1 => cons(h(), empty)
      case Cons(h, t) => cons(h(), t().take(n - 1))
    }

    def drop(n: Int): Stream[A] = if (n == 0) this else this match {
      case Empty => empty
      case Cons(h, t) => t().drop(n - 1)
    }

    //5.3
    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Empty => empty
      case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
      case Cons(h, t) => empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    //5.4
    def forAll(f: A => Boolean): Boolean = foldRight(true)((a, b) => f(a) && b)

    //5.5
    def takeWhile2(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

    //5.6
    def headOption: Option[A] = foldRight[Option[A]](None)((a, b) => Some(a))

    //5.7
    def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

    def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

    def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)(cons(_, _))

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a) append b)
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def ones: Stream[Int] = cons(1, ones)
    //5.8
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    //5.9
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    //5.10
    def fib: Stream[Int] = {
      def step(i1: Int, i2: Int): Stream[Int] = cons(i1, step(i2, i1 + i2))

      step(0, 1)
    }

    //5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

    //5.12
    def ones2: Stream[Int] = unfold(1)(_ => Some((1, 1)))
    def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))
    def from2(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))
    def fib2: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))
  }

  def main(args: Array[String]): Unit = {
    println(fib2.take(10).toList)
  }
}
