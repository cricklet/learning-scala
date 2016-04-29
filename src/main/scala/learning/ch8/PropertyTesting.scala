package learning.ch8
import learning.ch6.{RNG, State}

object PropertyTesting {
  trait Prop {
    def &&(p: Prop): Prop = {
      lazy val checked = p.check() && this.check()
      new Prop { def check() = checked }
    }
    def check(): Boolean
  }

  case class Gen[A](sample: State[RNG, A]) {
    def map[B](f: A => B): Gen[B] = {
      Gen[B](this.sample.map(f))
    }
    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      Gen[B](this.sample.flatMap(
        f(_).sample
      ))
    }
  }
  object Gen {
    // def listOf[A](gen: Gen[A]): Gen[List[A]]
    // def forAll[A](gen: Gen[A])(f: A => Boolean): Boolean

    def choose(low: Int, high: Int) =
      Gen[Int](State[RNG, Int](RNG.nonNegativeInt).map(
        n => (Math.abs(n) % (high - low) + low)
      ))

    def unit[A](a: => A): Gen[A] =
      Gen[A](State[RNG,A](rng => (a, rng)))

    def boolean(): Gen[Boolean] =
      Gen[Boolean](State[RNG,Boolean](RNG.boolean))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen[List[A]](
        State.sequence(
          List.fill(n)(
            State[RNG,A](g.sample.run)
          )
        )
      )
  }
}
