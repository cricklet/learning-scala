object Maths {

  def abs(x: Int): Int =
    if (x < 0) -x
    else x

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

  def fib(n: Int): Int = {
    def calc(v0: Int, v1: Int, steps: Int): Int =
      if (steps == n) v0
      else calc(v1, v0 + v1, steps + 1)

    calc(0, 1, 0)
  }

}

def formatResult (name: String, n: Int, f: Int => Int): String =
  "The %s of %d is %d.".format(name, n, f(n))

println(formatResult("abs", -4, Maths.abs))
println(formatResult("fact", 4, Maths.factorial))

print("Fibonacci: ")
for (i <- 0 until 10) {
  print(Maths.fib(i) + " ")
}
println()
