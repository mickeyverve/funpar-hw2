package solver

object Solver extends App{
  // solves expString == 0 for the variable in varName with an initial guess
  // specified. We'll assume that the given expression has a root.

  def solve(expString: String, varName: String, guess: Double): Double = {
    val ex = Parser(expString)
    // TODO: complete the implementation. This will construct the 
    // appropriate functions and call Newton.solve
    // the f(x) and f'(x) functions
    def f(x: Double): Double = {
      Process.eval(ex.get, Map(varName -> x))
    }
    def df(x: Double): Double = {
      Process.eval(Process.differentiate(ex.get, varName), Map(varName -> x))
    }
    Newton.solve(f, df, guess).get
  }
}
