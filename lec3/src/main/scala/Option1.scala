/**
  * Created by Barak Bar Orion
  * on 7/24/16.
  *
  * @since 12.0
  */
object Option1 {

  sealed abstract class Option[+A]{
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case Some(b) => Some(b)
      case None=> ob
    }

    def filter(f: A => Boolean): Option[A] = this match {
      // TODO handle None ?? , possible to do if else ?
      case Some(a) if !f(a) => None
      case Some(a) if f(a) => Some(a)
    }

    def lift[A,B](f: A => B): Option[A] => Option[B] = (a: Option[A]) => a.map(f)



    def sequence[A](a: List[Option[A]]): Option[List[A]] = {

      val func = (a: Option[A]) => a.getOrElse()
      a match {
        case Nil => None
        case h :: tail => h.flatMap(hh => sequence(tail).map(tltl => hh :: tltl))
      }

    }


    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(List())
      case hd :: tail => f(hd).flatMap(headValue => traverse(tail)(f).map(headValue :: _))
    }

  }

  object None extends Option[Nothing]
  case class Some[A](get : A) extends Option[A]


  def main(args: Array[String]): Unit = {

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)
  }

}
