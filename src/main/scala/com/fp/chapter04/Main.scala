package com.fp.chapter04

object Main {

  //4.1
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(v) => Some(f(v))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(v) => v
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

    def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] = flatMap(v => if(f(v)) this else None)
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  //4.2
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  //4.3
  def map2[A,B, C](o1: Option[A], o2: Option[B])(f: (A,B) => C): Option[C] =
    o1.flatMap(v1 => o2.map(v2 => f(v1, v2)))

  //4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    if (a.contains(None)) None else Some(a.map { case Some(v) => v})

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  //4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x), y)(_ :: _))

  //4.6
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => b
      case Right(a) => this
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] = this match {
      case Left(e) => Left(e)
      case Right(v1) => b match {
        case Left(e) => Left(e)
        case Right(v2) => Right(f(v1, v2))
      }
    }
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  //4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight[Either[E, List[A]]](Right(Nil))((x,y) => x.map2(y)(_ :: _))

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    a.foldRight[Either[E, List[B]]](Right(Nil))((x, y) => f(x).map2(y)(_ :: _))

  def main(args: Array[String]): Unit = {
    val ss: Option[Int] = Some(3)
    val l = List(Some(1), Some(), Some(5), None)
    val l2 = List(1, 2, 5)
  }
}
