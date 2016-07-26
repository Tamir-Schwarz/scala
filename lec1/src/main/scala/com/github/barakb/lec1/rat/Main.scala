package com.github.barakb.lec1.rat

/**
  * Created by Barak Bar Orion
  * on 7/13/16.
  *
  * @since 12.0
  */
object Main {
  implicit def int2Rational(n: Int): Rational = new Rational(n)
  implicit def int2Rational(n: Double): Rational = new Rational(n.toInt)

  def main(args: Array[String]): Unit = {
    //    println(Rational(2, 4) + 1)
    //    println(1 + Rational(2, 4))
    //    println(new Rational(2, 4) + new Rational(2, 3))
    //    println(new Rational(2, 4) + 1)
    //    println(1 + Rational(2, 4))
    //    val r = new Rational(1,2)
    //    val r1 = new Rational(3,4)
    //    val r2 = r.*(r1)
    //    println(r2)
    //
    //    val r3 = r * r1
    //    println(r3)

    var r4 = new Rational(6, 2)
    val r5 = new Rational(4, 3)
    val r6 = r4 / r5
    println(r6)

    r4 = r4/r5
    println(r4)

    r4 = 3.2 / r4
    println(r4)




    println()
  }
}
