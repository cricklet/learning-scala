object Maths {

  def abs(x: Int): Int =
    if (x < 0) -x
    else x

  def formatAbs(x: Int) = {
    "Abs of %d is %d!".format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def formatFactorial(n: Int): String = {
    "Factorial of %d is %d.".format(n, factorial(n))
  }
}


println(Maths.formatAbs(-4))
println(Maths.formatFactorial(4))
