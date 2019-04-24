package com.fp.chapter06

object Main {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt

    (if (i != Int.MinValue) Math.abs(i) else 0, rng2)
  }

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    println(n1)

    val (n2, rng3) = nonNegativeInt(rng2)
    println(n2)
  }
}
