
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
    def run [A] (es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A] // Mutable, threadsafe reference
      val latch = new CountDownLatch(1) // Wait for 'latch' to be called once
      p(es) { a => { ref.set(a); latch.countDown() } }
      latch.await() // Wait for result...
      ref.get()
    }
  }
}
