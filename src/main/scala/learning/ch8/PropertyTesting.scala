package learning.ch8
import learning.ch6.{RNG, SimpleRNG, State}

object PropertyTesting {
  trait Prop {
    def &&(p: Prop): Prop = {
      lazy val checked = p.check() && this.check()
      new Prop { def check() = checked }
    }
    def check(): Boolean
  }

  case class Gen[A](sample: State[RNG, A])
  object Gen {
    // def listOf[A](gen: Gen[A]): Gen[List[A]]
    // def forAll[A](gen: Gen[A])(f: A => Boolean): Boolean
    def choose(low: Int, high: Int) = {
      Gen[Int](State[RNG,Int](
        rng => {
          val (n, rng1) = rng.nextInt
          ((n % (high - low) + low), rng1)
        }
      ))
    }
  }
}
