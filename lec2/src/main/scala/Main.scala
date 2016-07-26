import math.{E, Pi}

/**
  * Created by Barak Bar Orion
  * on 7/20/16.
  *
  * @since 12.0
  */


abstract class Expr

case class Var(name: String) extends Expr

case class Number(num: Double) extends Expr

case class UnOp(operator: String, arg: Expr) extends Expr

case class BinOp(operator: String,
                 left: Expr, right: Expr) extends Expr


object Main {

  def simplifyTop(expr: Expr): Expr = expr match {
    case UnOp("-", UnOp("-", e)) => e
    case BinOp("+", e, Number(0)) => e
    case BinOp("+", Number(0), e) => e
    case BinOp("*", e, Number(1)) => e
    case BinOp("*", Number(1), e) => e
    case BinOp("*", e, Number(0)) => Number(0)
    case BinOp("*", Number(0), e) => Number(0)
    case _ => expr
  }

  def simplify(expr: Expr): Expr = expr match {
    case UnOp(operator, exp) => simplifyTop(UnOp(operator, simplify(exp)))
    case BinOp(operator, exp1, exp2) => simplifyTop(BinOp(operator, simplify(exp1), simplify(exp2)))
    case _ => expr
  }

  def simplify1(expr: Expr): Expr = expr match {
    // stop condition
    case Number(x) => {println("simplify1- in Number :" + expr); Number(x) }
    // stop conditions from top
    case UnOp("-", UnOp("-", e)) => {println("simplify1 - in UnOp :" + expr); simplifyTop(e)}
    case BinOp("+", e, Number(0)) => {println("simplify1 - in (+) BinOp-1 :" + expr); simplifyTop(e) } // Adding zero
    case BinOp("*", e, Number(1)) => {println("simplify1 - in (*) BinOp-2 :" + expr); simplifyTop(e) }

    case BinOp("+", Number(0), e) => {println("simplify1-2 - in (+) BinOp-1 :" + expr); simplifyTop(e) } // Adding zero
    case BinOp("*", Number(1), e) => {println("simplify1-2 - in (*) BinOp-2 :" + expr); simplifyTop(e) }

    case UnOp(op, Number(x)) => {println("simplify1 -  in UnOP " + expr); simplifyTop(expr)}
    case UnOp(op, e) =>{ println("simplify1 - in UnOp expr: " + expr + " e: " + e); simplifyTop(UnOp(op, simplify1(e)))}
    case BinOp(op, e1, e2) => {println("simplify1 -In BinOp e1: " + e1 + ", e2: " + e2); simplifyTop(BinOp(op, simplify1(e1), simplify1(e2)))}
    case _ => expr
  }

  //    def simplify1(expr: Expr): Expr = expr match {
  //      // stop condition
  //      case Number(x) => Number(x)
  //      case UnOp(op, Number(x)) => simplifyTop(expr)
  //
  //      case BinOp("+", e, Number(0)) => simplifyTop(simplify1(e))
  //      case BinOp("*", e, Number(1)) =>  simplifyTop(simplify1(e))
  //
  //      case BinOp("+", Number(0), e) => simplifyTop(simplify1(e))
  //      case BinOp("*", Number(1), e) =>  simplifyTop(simplify1(e))
  //
  //      case UnOp("-", UnOp("-", e)) => simplifyTop(simplify1(e))
  //
  //      case UnOp(op, e) =>  simplifyTop(UnOp(op, simplify1(e)))
  //      case BinOp(op, e1, e2) =>  simplifyTop(BinOp(op, simplify1(e1), simplify1(e2)))
  //    }


  //    val op = BinOp("+", Number(3), Number(0))
  //    val op = BinOp("+", UnOp("+", Number(1)), Number(3))
  //    val op = BinOp("+", UnOp("+", Number(1)), UnOp("*",Number(3)))
  //    val op = BinOp("+", BinOp("+", Number(1), Number(2)), UnOp("*",Number(3)))
  //    val op = BinOp("+", BinOp("*", Number(3), Number(4)), BinOp("+",Number(5), Number(6)))
  val op = BinOp("*", BinOp("+", Number(2), Number(3)), BinOp("*",Number(4), Number(5)))
  //    val op = BinOp("*", BinOp("+", Number(2), UnOp("-", UnOp("-", UnOp("-", UnOp("-", Number(0)))))), BinOp("*",Number(1), Number(1)))
  //    val op = BinOp("*", UnOp("-", UnOp("-", Number(1))), BinOp("+", Number(2), Number(2)))


  //    println("--------------- top ---------------")
  //    val top = simplifyTop(op)

  //    println("--------------- simplify ---------------")
  //    val res = simplify(op)
//  println("################# op #################")
//  println(op)
//
//  println("--------------- simplify1 ---------------")
//  val res1 = simplify1(op)

//  println("-------------------------------------------")


  //    println("################# top #################")
  //    println(top)

  //    println("################# res #################")
  //    println(res)

//  println("################# res1 #################")
//  println(res1)
}


