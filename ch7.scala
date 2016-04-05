// Here's a simple sum:
def sum1 (ints: Seq[Int]): Int =
  ints.foldLeft(0)(_+_)

// Let's do it recursively in a way that could conceivably be parallelized:
def sum2 (ints: IndexedSeq[Int]): Int =
  if (ints.size == 0) 0
  else if (ints.size == 1) ints.head
  else {
    val (l,r) = ints.splitAt(ints.length / 2)
    sum2(l) + sum2(r)
  }

// Let's think about parallel computations.
// We need to think of a dataa-type to represent our parallel computations.
// Obviously, that data type must contain the result.

object Par {
  // This takes an unevaluated expression and returns a computation that
  // evaluates it in a separate thread.
  def unit [A] (a: => A): Par[A]

  // This extracts the result from a parallel computation.
  def get[A] (a: Par[A]): A
}

// Here's how we woud write a parallelized sum given a 'Par' datatype.
def sum3 (ints: IndexedSeq[Int]): Int =
  if (ints.size == 0) 0
  else if (ints.size == 1) ints.head
  else {
    val (l,r) = ints.splitAt(ints.length / 2)
    val sumL: Par[Int] = Par.unit(sum3(l))
    val sumR: Par[Int] = Par.unit(sum3(r))
    Par.get(sumL) + Par.get(sumR)
  }

// What's wrong with Java's tools: Runnable & Thread?
// Everything returns 'Unit'!

// trait Runnable { def run: Unit }
// class Thread (r: Runnable) {
//   def start: Unit
//   def join: Unit
// }

// What's wrong with Future & ExecutorService?
// There's no way of composing futures and Future.get is blocking.

// trait Future [A] { def get: A }
// class ExecutorService {
//   def submit [A] (a: Callable[A]): Future[A]
// }

