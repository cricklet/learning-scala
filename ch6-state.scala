
// Our type 'Rand' (in ch6-rand.scala) can be generalized to any kind of 'State'
// i.e. type State[S,+A] = S => (A,S)
//      type Rand[A] = State[RNG, A]

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

  def modify [S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get [S]: State[S,S] = State(s => (s, s))
  def set [S](s: S): State[S, Unit] = State(_ => ((), s))
}

// Let's implement a very simple state machine that counts:
def counter: State[Int, Int] =
  State((s: Int) => (s + 1, s + 1))

val (v0, s0) = counter.run(0)
val (v1, s1) = counter.run(s0)
val (v2, s2) = counter.run(s1)
val (v3, s3) = counter.run(s2)
println("Count from 0: %s, %s, %s, %s".format(v0, v1, v2, v3))

// Because State has 'map' and 'flatMap' defined, we can use it in
// for-comprehensions! Let's try counting using a for-comprehensions:
val count0: State[Int, Int] = for {
  x0 <- counter
  x1 <- counter
  x2 <- counter
  x3 <- counter
} yield x3

// This code, roughly translates to:
val count1: State[Int, Int] =
  counter.flatMap(x0 =>
    counter.flatMap(x1 =>
      counter.flatMap(x2 =>
        counter.map(x3 => x3))))

println("Using a for-comprehension to count: %s".format(count0.run(0)._1))
println("Replicating that code using nested flatMaps + map: %s".format(count1.run(0)._1))

// To better understand this, let's unroll a simpler loop:
val l0: List[(Int, Int)] = for {
  x <- List(0,1,2)
  y <- List(9,8)
} yield (x, y)

val l1: List[(Int, Int)] = List(0,1,2).flatMap(x =>
  List(9,8).map(y => (x, y)))

println("Using a for-comprehension to combine two lists: %s".format(l0))
println("Using a nested flatMaps + map to combine two lists: %s".format(l1))
