package solver

object Process {
  // gives a "pretty-print" string form of the expression
  def stringify(e: Expression): String = e match {
    case Constant(c) => c.toString
    case Var(name) => name
    case Sum(l, r) => stringify(l) + " + " + stringify(r)
    case Prod(l @ Sum(_, _), r @ Sum(_, _)) => "(" + stringify(l) + ") * (" + stringify(r) + ")"
    case Prod(l @ Sum(_, _), r) => "(" + stringify(l) + ") * " + stringify(r)
    case Prod(l, r @ Sum(_, _)) => stringify(l) + " * (" + stringify(r) + ")"
    case Prod(l, r) => stringify(l) + " * " + stringify(r)
    case Power(b, e) => stringify(b) + "^" + stringify(e)
  }

  // evaluates a given expression e: Expression using
  // the variable settings in varAssn: Map[String, Double],
  // returning the evaluation result as a Double.

  // Example: eval(e, Map("x" -> 4.0)) evaluates the expression 
  // with the variable "x" set to 4.0.
  def eval(e: Expression, varAssn: Map[String, Double]): Double =  {
    def evalHelper(e: Expression): Double = e match {
      case Constant(n) => n
      case Var(s) => varAssn(s)
      case Prod(e1, e2) => evalHelper(e1) * evalHelper(e2)
      case Sum(e1, e2) => evalHelper(e1) + evalHelper(e2)
      case Power(e1, e2) => scala.math.pow(evalHelper(e1), evalHelper(e2))
      case Log(e1) => scala.math.log(evalHelper(e1))
    }
    evalHelper(e)
  }

  // symbolically differentiates an expression e: Expression with 
  // respect to the variable varName: String
  def differentiate(e: Expression, varName: String): Expression = {
    def differentiateHelper(e: Expression): Expression = e match {      
      case Var(_) => Constant(1)
      case Constant(_) => Constant(0)
      case Power(Var(x), Constant(e)) => Prod(Constant(e), Power(Var(x), Sum(Constant(e), Constant(-1))))
      case Prod(Constant(e1), e2) => Prod(Constant(e1), differentiateHelper(e2))
      case Prod(e1, Constant(e2)) => Prod(Constant(e2), differentiateHelper(e1))
      case Prod(e1, e2) => Sum(Prod(differentiateHelper(e1), e2), Prod(differentiateHelper(e2), e1))
      case Sum(e1, e2) => Sum(differentiateHelper(e1), differentiateHelper(e2))
      case Power(Constant(e1), e2) => Prod(Log(Constant(e1)), Power(Constant(e1), e2))
      case Power(e1, Constant(e2)) => Prod(Prod(Constant(e2), Power(e1, Sum(Constant(e2), Constant(-1)))), differentiateHelper(e1))      
    }
    differentiateHelper(e)
  }

  // forms a new expression that simplifies the given expression e: Expression
  // the goal of this function is produce an expression that is easier to
  // evalHelper and/or differentiate.  If there's a canonical form you'd like to
  // follow, use this function to put an expression in this form.
  // you may leave this function blank if can't find any use. 
  def simplify(e: Expression): Expression = {

    ???
  }

}
