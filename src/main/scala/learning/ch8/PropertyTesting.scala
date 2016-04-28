package learning.ch8

object PropertyTesting {
  trait Prop {
    def &&(p: Prop): Prop
    def check(): Unit
  }

  trait Gen[A] {
    def listOf[A](gen: Gen[A]): Gen[List[A]]
    def choose(low: Int, high: Int): Gen[Int]
    def forAll[A](gen: Gen[A])(f: A => Boolean): Boolean
  }
}
