/**
  * Created by Barak Bar Orion
  * on 7/25/16.
  *
  * @since 12.0
  */
object Hw4 {

  sealed trait Either[+E, +A] {

    def map[B](f: A => B): Either[E, B] = this match  {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => b
      case Right(a) => Right(a)
    }

    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      es match {
        case Nil => Right(Nil)
        case head :: tail => head.flatMap(headvalue => sequence(tail).map( tailvalue => headvalue :: tailvalue))
      }
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      as match {
        case Nil => Right(Nil)
        case head :: tail => f(head).flatMap(headvalue => traverse(tail)(f).map(tailvalue => headvalue :: tailvalue))
      }
    }

  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]


  // example of usage:

  def main(args: Array[String]): Unit = {


      def mean(xs: IndexedSeq[Double]): Either[String, Double] =
        if (xs.isEmpty)
          Left("mean of empty list!")
        else
          Right(xs.sum / xs.length)


      def safeDiv(x: Int, y: Int): Either[Exception, Int] =
        try Right(x / y)
        catch {
          case e: Exception => Left(e)
        }


      def Try[A](a: => A): Either[Exception, A] =
        try Right(a)
        catch {
          case e: Exception => Left(e)
        }
  }
}
