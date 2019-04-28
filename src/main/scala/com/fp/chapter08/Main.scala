package com.fp.chapter08

import com.fp.chapter06.Main._
import com.fp.chapter08.Main.Prop._
import com.fp.chapter05.Main._

object Main {

  case class Gen[A](sample: State[RNG, A]) {
    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(a => Gen(State(s => sequence(List.fill(a)(sample.run))(s))))
  }

  object Gen {
    def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(s =>
      map(nonNegativeLessThan(stopExclusive - start))(k => k + start)(s)
    ))

    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    def boolean: Gen[Boolean] = Gen(State(s => map(nonNegativeLessThan(2))(_ > 0)(s)))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State(s => sequence(List.fill(n)(g.sample.run))(s)))

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(s => if(s) g1 else g2)

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A],Double)): Gen[A] = choose(0, Int.MaxValue).flatMap(s =>  if (s < g1._2 * Int.MaxValue/(g1._2 + g2._2)) g1._1 else g2._1 )
  }

  object Prop {
    type TestCases = Int
    type FailedCase = String
    type SuccessCount = Int
  }


  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case class Prop(run: (TestCases,RNG) => Result)

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  //  def forAll[A](a: Gen[A])(f: A => Boolean): Prop


  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(47)

    val (n1, rng2) = Gen.choose(5, 25).sample.run(rng)
    println(n1)

    val (n2, rng3) = Gen.boolean.sample.run(rng2)
    println(n2)

    val (n3, rng4) = Gen.listOfN(10, Gen.choose(5, 25)).sample.run(rng3)
    println(n3)

    val (n4, rng5) = Gen.boolean.listOfN(Gen.choose(1, 8)).sample.run(rng4)
    println(n4)

    val (n5, rng6) = Gen.union(Gen.choose(1, 3), Gen.choose(8, 10)).sample.run(rng5)
    println(n5)

    val (n6, rng7) = Gen.weighted((Gen.choose(1, 2), 0.67), (Gen.choose(3, 4), 0.33)).listOfN(Gen.choose(20, 21)).sample.run(rng6)
    println(n6)
  }
}
