package learning.ch8


object PropertyTesting {
  trait Prop {
    def &&(p: Prop): Prop = {
      lazy val checked = p.check() && this.check()
      new Prop { def check() = checked }
    }
    def check(): Boolean
  }

  trait Gen[A] {}
  case class Gen[A](sample: State[RNG, A])
  object Gen {
    // def listOf[A](gen: Gen[A]): Gen[List[A]]
    // def choose(low: Int, high: Int): Gen[Int]
    // def forAll[A](gen: Gen[A])(f: A => Boolean): Boolean
  }
}
