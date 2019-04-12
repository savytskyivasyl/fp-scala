package com.fp.chapter03

import scala.annotation.tailrec

object Main {
  //3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case h :: t => t
  }

  //3.3
  def setHead[A](l: List[A], v: A): List[A] = l match {
    case Nil => List(v)
    case h :: t => v :: t
  }

  //3.4
  def drop[A](l: List[A], v: Int): List[A] = l match {
    case Nil => Nil
    case _ :: t if v > 0 => drop(t, v - 1)
    case _ :: _ => l
  }

  //3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case h :: t if f(h) => dropWhile(t, f)
    case _ :: _ => l
  }

  //3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ :: Nil => Nil
    case h :: t => h :: init(t)
  }

  //3.7
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case h :: t => f(h, foldRight(t, z)(f))
  }

  def sumR(l: List[Int]): Int = foldRight(l, 0)(_ + _)

  def productR(l: List[Int]): Int = foldRight(l, 1)(_ * _)

  //3.9
  def lengthR(l: List[Int]): Int = foldRight(l, 0)((x, y) => y + 1)

  //3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case h :: t => foldLeft(t, f(z, h))(f)
  }

  //3.11
  def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def length[A](l: List[A]): Int = foldLeft(l, 0)((y, x) => y + 1)

  //3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((y, x) => x :: y)

  //3.13
  def foldLeftR[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((x, y) => f(y, x))

  def foldRightL[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeftR(reverse(as), z)((x, y) => f(y, x))

  //3.14
  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRightL(a1, a2)((x, y) => x :: y)

  //3.15
  def concat[A](ll: List[List[A]]): List[A] =
    foldLeft(ll, List[A]())(append)

  //3.16
  def plusOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case h :: t => h + 1 :: plusOne(t)
  }

  //3.17
  def toString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case h :: t => h.toString :: toString(t)
  }

  //3.18
  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case h :: t => f(h) :: map(t)(f)
  }

  //3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case h :: t if f(h) => h :: filter(t)(f)
    case _ :: t => filter(t)(f)
  }

  //3.20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case h :: t => append(f(h), flatMap(t)(f))
  }

  //3.21
  def filter2[A, B](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else List())

  //3.22
  def zipWithAddition(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (h1 :: t1, h2 :: t2) => h1 + h2 :: zipWithAddition(t1, t2)
  }

  //3.23
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (h1 :: t1, h2 :: t2) => f(h1, h2) :: zipWith(t1, t2)(f)
  }

  //3.24
  def hasSubsequence[A](sup: List[Int], sub: List[Int]): Boolean = {
    @tailrec
    def startsWith(sup: List[Int], sub: List[Int]): Boolean = sup match {
      case Nil => sub == Nil
      case h :: t => sub match {
        case Nil => true
        case hb :: _ if hb != h => false
        case _ :: tb => startsWith(t, tb)
      }
    }

    (1 until length(sup)).map(drop(sup, _)).exists(x => startsWith(x, sub))
  }


  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  //3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  //3.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  //3.27
  def depth(tree: Tree[Int]): Int = {
    def step(tree: Tree[Int], layer: Int): Int =  tree match {
      case Leaf(_) => layer
      case Branch(left, right) => step(left, layer + 1) max step(right, layer + 1)
    }

    step(tree, 0)
  }

  //3.28
  def map[A, B](l: Tree[A])(f: A => B): Tree[B] = l match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  //3.29
  def fold[A, B](as: Tree[A])(transform: A => B)(combine: (B, B) => B): B = as match {
    case Leaf(value) => transform(value)
    case Branch(left, right) => combine(fold(left)(transform)(combine), fold(right)(transform)(combine))
  }

  def sizeViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)
  def maximumViaFold(tree: Tree[Int]): Int = fold(tree)(v => v)(_ max _)
  def depthViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((d1, d2) => 1 + (d1 max d2))
  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](tree)(a => Leaf(f(a)))(Branch(_, _))

  def main(args: Array[String]): Unit = {
    val l1 = 1 until 11 toList
    val l2 = 11 until 21 toList
    val l3 = 21 until 31 toList
    val l = List(l1, l2, l3)

    val leaf1 = Leaf(7)
    val leaf2 = Leaf(5)
    val leaf3 = Leaf(9)
    val leaf4 = Leaf(1)
    val branch1 = Branch(leaf1, Branch(leaf2, Branch(leaf1, leaf4)))
    val branch2 = Branch(leaf2, leaf3)
    val tree = Branch(branch1, branch2)

    println(map(tree)(_ + 1))
    println(mapViaFold(tree)(_ + 1))

    //3.8
    println(foldRight(l1, Nil:List[Int])(_ :: _))
  }

}
