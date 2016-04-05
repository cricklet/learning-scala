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
  def unit [A] (a: A): Par[A]

  def lazyUnit [A] (a: => A): Par[A] =
    fork(unit(a))

  // This extracts the result from a parallel computation.
  def get [A] (a: Par[A]): A

  // This let's you easily combine multiple computations
  def map2 [A, B, C] (a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]

  // This lets you spawn a new thread to compute a Par
  def fork [A] (a: Par[A]): Par[A]
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

// Let's also think about what lazy evaluation implies for parallelization.
// What happens when this is run?
//    Par.get(Par.unit(sum(l))) + Par.get(Par.unit(sum(r)))

// The first Par.get blocks while Par.unit(sum(l)) runs. This prevents Par.unit(sum(r))
// from even starting!

// In this version of sum, we combine two Pars via Par.map2
def sum4 (ints: IndexedSeq[Int]): Par[Int] =
  if (ints.size == 0) Par.unit(0)
  else if (ints.size == 1) Par.unit(ints.head)
  else {
    val (l,r) = ints.splitAt(ints.length / 2)
    Par.map2(sum4(l), sum4(r))(_ + _)
  }

// What happens if map2 is strict in both of it's arguments?
//   sum4(List(1,2,3,4))
//   map2(sum4(List(1,2)), sum4(List(3,4)))(add)
//   map2(map2(sum4(List(1)), sum4(List(2)))(add), sum4(List(3,4)))(add)
//   map2(map2(unit(1), unit(2))(add), sum4(List(3,4)))(add)

// At this point, unit(1) and unit(2) can begin processing.

//   map2(map2(unit(1), unit(2))(add), map2(sum4(List(3)), sum4(List(4)))(add))(add)
//   map2(map2(unit(1), unit(2))(add), map2(unit(3), unit(4))(add))(add)

// Thus, unit(1) and unit(2) begin computing before the second half of the list is
// even considered.

// What if we still strictly evaluate each argument of map2 but wait for the entire
// expression tree to be evaluated before starting the computations for each Par object?
// Well, we're going to have a lot of Par objects. It'll be pretty heavy.

// This is hard to reason about without actual code...
// But let's keep on pushing.

// Here's a sum() that explicitly splits off a new thread for the *recursive*
// step of calling sum on each sub-list.

// This is a great decision! We can let the programmer fundamentally control how
// parallel computations are created. map2 gets to stay strict which makes it easier
// to reason about concretely. unit can *also* be strict now!

def sum5 (ints: IndexedSeq[Int]): Par[Int] =
  if (ints.size == 0) Par.unit(0)
  else if (ints.size == 1) Par.unit(ints.head)
  else {
    val (l,r) = ints.splitAt(ints.length / 2)
    Par.map2(
      Par.fork(sum4(l)),
      Par.fork(sum4(r))
    )(_ + _)
  }
