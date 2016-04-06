
import java.util.concurrent._

// Here's my first attempt to come up with a representation for Par...
// sealed trait Par[+A]
// case class ForkedPar [+A] (a: () => A) extends Par[A]
// case class SimplePar [+A] (a: A) extends Par[A]

// Oh, actually, we're going to use ExecutorService, Callable, & Future.

// trait Future [A] { def get: A }
// trait Callable [A] { def call: A }
// class ExecutorService {
//   def submit [A] (a: Callable[A]): Future[A]
// }

// So, an ExecutorService turns a Callable into a Future.
// I think that we'll want to embed a:A into a Callable[A] and return a function
// which lets you execute that callable and turn it into a value

// type Par[A] = ExecutorService => A

// The book points out that returning a Future[A] is more flexible as it gives
// caller the ability to decide how long to wait for a computation, whether to
// cancel it, etc.

// This changes a little of the API we outlined earlier. run() must now return
// a Future.
type Par[A] = ExecutorService => Future[A]

object Par {
  private class UnitFuture [A] (get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
    def get() = get
  }

  // Promotes a constant value to a parallel computation
  def unit [A] (a: A): Par[A] =
    (e: ExecutorService) => new UnitFuture(a)

  // Combine the result of two parallel computations with 'f'
  def map2 [A, B, C] (a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (e: ExecutorService) => {
      val aFuture = a(e)
      val bFuture = b(e)
      new UnitFuture(f(aFuture.get, bFuture.get))
    }

  // Flag a computation for concurrent evaluation
  def fork [A] (a: Par[A]): Par[A] =
    // Given an executor
    (e: ExecutorService) => {
      // Submit a new job
      e.submit(new Callable[A] {
        // That runs the parallel computation 'a' using that executor
        def call: A = a(e).get
      })
    }

  // Promote a thunk value to a parallel compuation
  // & mark it for concurrent evaluation
  def lazyUnit [A] (a: => A): Par[A] =
    fork(unit(a))

  // Extract a value from a Par by actually performing the computation
  // def run [A] (a: Par[A]): Future[A]
}
