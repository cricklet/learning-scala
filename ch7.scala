// Here's a simple sum:
def sum1 (ints: Seq[Int]): Int =
  ints.foldLeft(0)(_+_)

// Let's do it recursively in a way that could conceivably be parallelized:
def sum2 (ints: Seq[Int]): Int =
  if (ints.size == 0) 0
  else if (ints.size == 1) ints.head
  else {
    val (l,r) = ints.splitAt(ints.length / 2)
    sum(l) + sum(r)
  }

// Let's think about parallel computations.
// We need to think of a dataa-type to represent our parallel computations.
// Obviously, that data type must contain the result.

// This takes an unevaluated expression and returns a computation that
// evaluates it in a separate thread.
def unit [A] (a: => A): Par[A]

// This extracts the result from a parallel computation.
def get[A] (a: Par[A]): A
