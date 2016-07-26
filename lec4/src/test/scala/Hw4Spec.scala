import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Barak Bar Orion
  * on 7/24/16.
  *
  * @since 12.0
  */
class Hw4Spec  extends FlatSpec with Matchers {
  import Hw4._
//  "Map on None" should "return None" in {
//    None.map(a => a) shouldBe None
//  }
  "Map on Right(1)" should "return Right(2)" in {
    Right(1).map( (x : Int) => x + 1)  shouldBe Right(2)
  }

  "Map on Left(Error)" should "return Left(Error)" in {
    Left("Error").map( (x : Int) => x + 1)  shouldBe Left("Error")
  }

  "flatMap on Right(42)" should "return Right(84)" in {
    Right(42) flatMap( (x : Int) => Right(x*2) ) shouldBe  Right(84)
  }

  "flatMap onLeft(42)" should "return Left(42)" in {
    Left(42) flatMap( (x : Int) => Right(x*2) ) shouldBe  Left(42)
  }

  "orElse on Right(42)" should "return Right(42)" in {
    Right(42) orElse Left("ERROR") shouldBe  Right(42)
  }

  "orElse on Left(1)" should "return Left(\"ERROR\")" in {
    Left(1) orElse Left("ERROR") shouldBe  Left("ERROR")
  }

  "Right(42) sequence on List(Right(1), Right(2), Right(3))" should "return Right(List(1,2, 3)" in {
    sequence (List(Right(1), Right(2), Right(3))) shouldBe Right(List(1,2, 3))
  }

  "Right(42) sequence on List(Right(1), Left(Error), Right(3))" should "return Left(Error)" in {
    sequence (List(Right(1), Left("Error"), Right(3))) shouldBe Left("Error")
  }

  "Right(42) traverse on List(1, 2, 3)" should "return Right(List(1,2, 3)" in {
    traverse(List(1, 2, 3))((n: Int)=> Right(n + 2))   shouldBe Right(List(3,4, 5))
  }

  "traverse on List(2,4,5,6) and " should "return Left(odd)" in {
    def f(i: Int): Either[String, Int] = {if (i % 2 == 0) Right(i+1) else Left("odd")}
    traverse(List(2,4,5,6))(f) shouldBe Left("odd")
  }

  //  "Right(42) traverse on List(Right(1), Left(Error), Right(3))" should "return Left(Error)" in {
  //    Right(42).traverse(List("1", "Erorr", "3"))((n: String)=> Right((n + 2).toString))   shouldBe Left("Error")
  //  }

}
