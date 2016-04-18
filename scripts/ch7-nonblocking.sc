import java.util.concurrent.{Future, Executors}
import learning.ch7.Nonblocking.Par
import lib.Actor

val echo = Actor[String](Executors.newFixedThreadPool(4)) {
  x => println("Echoing: %s".format(x))
}

// Fire and forget a message (using '!')
echo ! "Hello!"
Thread.sleep(10000)

// Let's try concurrently mapping over lots of elements.
val p = Par.parMap(List.range(1, 10000))(math.sqrt(_))
Par.run(Executors.newFixedThreadPool(2))(p)
Thread.sleep(10000)