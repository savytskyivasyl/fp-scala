package com.fp.chapter08

import com.fp.chapter06.Main._
import com.fp.chapter08.Main.Prop.{FailedCase, SuccessCount}

object Main {

  case class Gen[A](sample: State[RNG, A])

  object Gen {
    def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(s =>
      map(nonNegativeLessThan(stopExclusive - start))(k => k + start)(s)
    ))

    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    def boolean: Gen[Boolean] = Gen(State(s => map(nonNegativeLessThan(2))(_ > 0)(s)))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State(s => sequence(List.fill(n)(g.sample.run))(s)))
  }

  trait Prop {
    def check: Either[(FailedCase, SuccessCount), SuccessCount]
    //def && (p: Prop): Prop = new Prop {
   //   override def check: Boolean = this.check && p.check
   // }
  }

  object Prop {
    type FailedCase = String
    type SuccessCount = Int
  }

 // def listOf[A](a: Gen[A]): Gen[List[A]]

//  def forAll[A](a: Gen[A])(f: A => Boolean): Prop


  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(47)

    val (n1, rng2) = Gen.choose(5, 25).sample.run(rng)
    println(n1)

    val (n2, rng3) = Gen.boolean.sample.run(rng2)
    println(n2)

    val (n3, rng4) = Gen.listOfN(10, Gen.choose(5, 25)).sample.run(rng3)
    println(n3)
  }
}
