import java.util.concurrent.{Future, Executors}
import lib.Actor

val S = Executors.newFixedThreadPool(4)
val echo = Actor[String](S) {
  x => println("Echoing: %s".format(x))
}

// Fire and forget a message (using '!')
echo ! "Hello!"

// So, let's wait for the thread to finish
Thread.sleep(1000)