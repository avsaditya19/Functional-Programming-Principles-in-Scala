import week4._

object exprs {
  def show(e: Expr): String = e match {
    case Number(x) => x.toString
    case Sum(l, r) => show(l) + " + " + show(r)
    case Var(x) => x.toString
    case Prod(Sum(x, y), e2) => '(' + show(Sum(x, y)) + ')' + '*' + show(e2)
    case Prod(e1, e2) => show(e1) + " * " + show(e2)
  }

  show(Sum(Number(1), Number(2)))
  show(Number(5))
  show(Var("x"))
  show(Sum(Prod(Number(2), Var("x")), Var("y")))
  show(Prod(Sum(Number(2), Var("x")), Var("y")))
}





