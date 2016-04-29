package learning.ch6

case class State[S,+A](run: S => (A, S)) {
  def flatMap [B](g: A => State[S,B]): State[S,B] =
    State(state0 => {
      val (a, state1) = run(state0)
      g(a).run(state1)
    })

  def map [B] (f: A => B): State[S,B] =
    flatMap(a => State.unit(f(a)))

  def map2 [B, C] (other: State[S,B])(f: (A, B) => C): State[S,C] =
    flatMap(a => {
      other.flatMap(b => {
        State(s => (f(a, b), s))
      })
    })
}

object State {
  def unit [S,A] (a: A): State[S,A] =
    State(state => (a, state))

  def sequence [S,A] (l: List[State[S,A]]): State[S,List[A]] =
    l.foldRight(unit[S,List[A]](Nil))((next: State[S,A], prev: State[S,List[A]]) =>
      next.map2(prev)(_ :: _)
    )

  def modify [S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get [S]: State[S,S] = State(s => (s, s))
  def set [S](s: S): State[S, Unit] = State(_ => ((), s))
}

trait RNG {
  def nextInt: (Int, RNG) // Return both the random int AND a new RNG
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng0: RNG): (Int, RNG) = {
    val (n, rng1) = rng0.nextInt
    (Math.abs(n), rng1)
  }

  def boolean(rng0: RNG): (Boolean, RNG) = {
    val (n, rng1) = rng0.nextInt
    (((n % 2) == 0), rng1)
  }

}
