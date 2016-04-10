
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference
import language.implicitConversions

object Nonblocking {
  trait Future[+A] {
    // We need this function to be private because it is not functionally pure.
    // If we want our API to be functionally pure, we can't have this be public!
    private[Nonblocking] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {
    def unit[A](a: A): Par[A] =
      (es: ExecutorService) => new Future[A] {
        def apply(cb: A => Unit): Unit =
            cb(a)
      }

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    def fork[A](p: => Par[A]): Par[A] =
      (es: ExecutorService) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(
            p(es) { a => cb(a) }
          )
      }

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A] // Mutable, threadsafe reference
      val latch = new CountDownLatch(1) // Wait for 'latch' to be called once
      p(es) { a => { ref.set(a); latch.countDown() } }
      latch.await() // Wait for result...
      ref.get()
    }
  }
}
