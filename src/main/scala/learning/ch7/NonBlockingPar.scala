package learning.ch7

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.{Callable, ExecutorService, CountDownLatch}
import lib.Actor

import annotation.tailrec

object Nonblocking {

  trait Future[+A] {
    // We need this function to be private because it is not functionally pure.
    // If we want our API to be functionally pure, we can't have this be public!
    private[ch7] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def unit[A](a: A): Par[A] =
      (es: ExecutorService) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] {
        def call = r
      })

    def fork[A](p: => Par[A]): Par[A] =
      (es: ExecutorService) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(p(es)(cb))
      }

    // To implement map2, we're going to need actors. An actor is a concurrent process
    // accepts messages from other threads and only occupies a thread when it is processing
    // a message.

    // Multiple threads can send messages to the actor at the same time, but the actor
    // can only process one at a time.
    def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C) = //: Par[C] =
      (es: ExecutorService) => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None

          val combiner = Actor[Either[A,B]](es)(_ match {
            case Left(a) =>
              if (br.isDefined) eval(es)(cb(f(a, br.get)))
              else ar = Some(a)
            case Right(b) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get, b)))
              else br = Some(b)
          })

          pa(es)(a => combiner ! Left(a))
          pb(es)(b => combiner ! Right(b))
        }
      }

    def map[A,B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A] // Mutable, threadsafe reference
      val latch = new CountDownLatch(1) // Wait for 'latch' to be called once
      p(es) { a => {
        ref.set(a);
        latch.countDown()
      } }
      latch.await() // Wait for result...
      ref.get()
    }

    def lazyUnit [A] (a: => A): Par[A] =
      fork(unit(a))

    def asyncF [A,B] (f: A => B): A => Par[B] =
      (a: A) => lazyUnit(f(a))

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
      map(parList)(_.sorted)

    def sequence [A] (ps: List[Par[A]]): Par[List[A]] =
      ps match {
        case Nil => unit(Nil)
        case pa :: pas =>
          // We have to fork here else we'll get stack overflows!
          map2(pa, fork(sequence(pas)))(_ :: _)
      }

    // What if instead of mapping on a Par, we want to map over a list in parallel?
    // Well, it's easy enough to map over the list, creating parallel computations
    // for each element. However, we need 'sequence' to combine them.
    def parMap [A, B] (as: List[A])(f: A => B): Par[List[B]] = {
      sequence(as.map(asyncF(f)))
    }

    def parFilter [A] (as: List[A])(f: A => Boolean): Par[List[A]] = {
      val bs: List[Par[List[A]]] = as.map(asyncF(a => if (f(a)) List(a) else List()))
      map(sequence(bs))(_.flatten)
    }
  }
}