package lib

import java.util.concurrent.{Future, Executors}

import org.scalatest.FunSuite

class ActorTest extends FunSuite {
  test("actor should return message") {
    val S = Executors.newFixedThreadPool(4)
    val mult10 = Actor[Int](S) {
      x => println(x * 10)
    }

    // ! fires and forgets
    // I'm not sure exactly how to test this-- although I'm sure there is a way!
    mult10 ! 5
  }
}
