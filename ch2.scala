object Maths {

  def abs(x: Int): Int =
    if (x < 0) -x
    else x

  def formatAbs(x: Int) = {
    "Abs of %d is %d!".format(x, abs(x))
  }

  def factorial(n: Int): Int = {

    // Tail call elimination: when 'self-recursion' is compiled into the same sort
    // of byte-code as would be emitted for a `while` loop.

    // Goodbye StackOverflowError!

    // Note: sometimes tail call elimination isn't successful. To find out when 'scalac'
    // fails to eliminate a tail call, use 'tailrec.'

    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else
        // A call is in 'tail-position' if it simply returns the value of a recursive call.
        go(n - 1, n * acc)

    go(n, 1)
  }

  def formatFactorial(n: Int): String = {
    "Factorial of %d is %d.".format(n, factorial(n))
  }

  def fib(n: Int): Int = {
    def calc(v0: Int, v1: Int, steps: Int): Int =
      if (steps == n) v0
      else calc(v1, v0 + v1, steps + 1)

    calc(0, 1, 1)
  }
}


println(Maths.formatAbs(-4))
println(Maths.formatFactorial(4))

println(Maths.fib(1))
println(Maths.fib(2))
println(Maths.fib(3))
println(Maths.fib(4))
println(Maths.fib(5))
