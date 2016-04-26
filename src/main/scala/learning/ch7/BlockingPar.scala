package learning.ch7

import java.util.concurrent.{Callable, ExecutorService, TimeUnit, Future}

object Blocking {
  private class UnitFuture [A] (v: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = v
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
    def get() = v
  }

  type Par[A] = ExecutorService => Future[A]

  object Par {
    // Promotes a constant value to a parallel computation
    def unit[A](a: A): Par[A] =
      (e: ExecutorService) => new UnitFuture(a)

    // Combine the result of two parallel computations with 'f'
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (e: ExecutorService) => {
        val aFuture = a(e)
        val bFuture = b(e)
        new UnitFuture(f(aFuture.get, bFuture.get))
      }

    // Flag a computation for concurrent evaluation
    def fork[A](a: Par[A]): Par[A] =
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
    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] =
      (a: A) => lazyUnit(f(a))

    def map[A, B](a: Par[A])(f: A => B): Par[B] =
      map2(a, unit(()))((a, _) => f(a))

    def join[A] (pP: Par[Par[A]]): Par[A] =
      (e: ExecutorService) => {
        val p = run(e)(pP).get
        run(e)(p)
      }

    def flatMap[A, B](pA: Par[A])(f: A => Par[B]): Par[B] =
      join(map(pA)(f))

    // To implement this, we really want 'map'
    def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
      map(parList)(_.sorted)

    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight(unit(Nil: List[A]))((aPar: Par[A], asPar: Par[List[A]]) =>
        map2(aPar, asPar)(_ :: _)
      )

    // What if instead of mapping on a Par, we want to map over a list in parallel?
    // Well, it's easy enough to map over the list, creating parallel computations
    // for each element. However, we need 'sequence' to combine them.
    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = {
      val fs: List[Par[B]] = as.map(asyncF(f))
      sequence(fs)
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val bs: List[Par[List[A]]] = as.map(asyncF(a => if (f(a)) List(a) else List()))
      map(sequence(bs))(_.flatten)
    }

    // Extract a value from a Par by actually performing the computation
    def run[A](es: ExecutorService)(a: Par[A]): Future[A] =
      a(es)
  }
}
