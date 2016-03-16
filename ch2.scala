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

{
  // A function that works on *any* type is a polymorphic (or generic) function.

  // This is different from polymorphism in OOP, where the term implies
  // sub-typing or inheritance.

  // By convention, type parameter declarations are named A, B, C, etc.

  def index[A](arr: Array[A], f: A => Boolean): Int ={
    @annotation.tailrec
    def loop(i: Int): Int =
      if (i >= arr.length) -1
      else if (f(arr(i))) i
      else loop(i + 1)

    loop(0)
  }

  var arr = Array("apple", "orange", "banana")
  var banana = "banana"

  // A function literal (or anonymous function)
  var isBanana = (v: String) => v == "banana"

  println("Index of %s in %s is %d.".format(
    banana,
    arr.mkString("[", ", ", "]"),
    index(arr, isBanana)
  ))
}

{
  def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean) = {
    def loop(i: Int): Boolean =
      if (i >= arr.length) true
      else if (ordered(arr(i-1), arr(i))) loop(i + 1)
      else false

    loop(1)
  }

  var arr = Array(0, 1, 2, 3)
  var orderedInts = (x: Int, y: Int) => x < y

  println("%s is sorted: %b".format(
    arr.mkString("[", ", ", "]"),
    isSorted(arr, orderedInts)
  ))

  arr = Array(0, 1, 3, 2)

  println("%s is sorted: %b".format(
    arr.mkString("[", ", ", "]"),
    isSorted(arr, orderedInts)
  ))
}

{
  // A function literal is actually an object with an 'apply' method!
  val lessThan1 = (a: Int, b: Int) => a < b
  val lessThan2 = new Function2[Int, Int, Boolean] {
    def apply(a: Int, b: Int) = a < b
  }

  println("%d < %d = %s".format(3, 4, lessThan1(3, 4)))
  println("%d < %d = %s".format(3, 4, lessThan2(3, 4)))
}

{
  def partial [A, B, C] (a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  val add3 = partial(3, (a: Int, b: Int) => a + b)
  println("add3(%d) = %d".format(4, add3(4)))

  // It's finally time for some classic currying.
  def curry [A, B, C] (f: (A, B) => C): A => (B => C) =
    (a: A) => ((b: B) => f(a, b))

  val add4 = curry((a: Int, b: Int) => a + b)(4)
  println("add4(%d) = %d".format(4, add4(4)))

  // And some uncurrying.
  def uncurry [A, B, C] (f: (A) => (B => C)): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  val add = uncurry(curry((a: Int, b: Int) => a + b))
  println("add(%d, %d) = %d".format(3, 4, add(3, 4)))
}

{
  // Composition is easy to figure out given the type signature.
  def compose [A, B, C] (f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
