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

  case class Gen[A](sample: State[RNG, A]) {
    def map[B](f: A => B): Gen[B] = {
      Gen[B](State[RNG,B](rng0 => {
        val (a, rng1) = this.sample.run(rng0)
        (f(a), rng1)
      }))
    }
    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      Gen[B](State[RNG,B](rng0 => {
        val (a, rng1) = this.sample.run(rng0)
        val (b, rng2) = f(a).sample.run(rng1)
        (b, rng2)
      }))
    }
  }
  object Gen {
    // def listOf[A](gen: Gen[A]): Gen[List[A]]
    // def forAll[A](gen: Gen[A])(f: A => Boolean): Boolean
    def choose(low: Int, high: Int) = {
      Gen[Int](State[RNG,Int](
        rng => {
          val (n, rng1) = rng.nextInt
          ((Math.abs(n) % (high - low) + low), rng1)
        }
      ))
    }

    def unit[A](a: => A): Gen[A] =
      Gen[A](State[RNG,A](rng => (a, rng)))

    def boolean(): Gen[Boolean] =
      Gen[Boolean](State[RNG,Boolean](rng => {
        val (n, rng1) = rng.nextInt
        (n % 2 == 0, rng)
      }))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen[List[A]](
        State.sequence(
          List.fill(n)(
            State[RNG,A](rng => {
              g.sample.run(rng)
            })
          )
        )
      )
  }
}