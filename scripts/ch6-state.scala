
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

// Okay, so what was the point of writing 'modify', 'get', and 'set'?
// 'get' clearly just returns the same state (while still being of type State)
// 'set' sets the state (and completely clobbers the previous value)
// 'modify'
//   The only parameter is a function 'f' that transforms some state (S => S)
//   We want to return a function of type State (i.e. S => (A, S)) where the
//   state S returned is generated via 'f'.

// Hmm 'get' and 'set' seem to make sense. 'modify' still doesn't...
// Let's try unrolling it!

def modifyViaNesting[S](f: S => S): State[S, Unit] =
  State.get.flatMap(s =>
    State.set(f(s)).map(_ => ()))

// This returns a State[Int,Unit] s.t. whatever state (Int) is passed into it,
// that state is incremented by 100.
modifyViaNesting((x: Int) => x + 100)

// For example:
val count2: State[Int, Int] = for {
  x0 <- counter
  x1 <- counter
  _  <- modifyViaNesting((x: Int) => x + 100)
  x2 <- counter
  x3 <- counter
} yield x3

val count3: State[Int, Int] = for {
  x0 <- counter
  x1 <- counter
  _  <- State.modify((x: Int) => x + 100)
  x2 <- counter
  x3 <- counter
} yield x3

println("Modifying (w/ flatMap + map) the state within a for-comprehension: %s".format(count2.run(0)._1))
println("Modifying (w/ comprehension) the state within a for-comprehension: %s".format(count3.run(0)._1))

// Alright, time to model a simple candy dispenser
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def insertCoin (): Machine = this match {
    case Machine(false, _, _) => this
    case Machine(_, 0, _) => this
    case Machine(true, _, _) => Machine(false, candies, coins + 1)
  }
  def turnKnob (): Machine = this match {
    case Machine(true, _, _) => this
    case Machine(_, 0, _) => this
    case Machine(false, _, _) => Machine(true, candies - 1, coins)
  }
  def input (input: Input): Machine = input match {
    case Coin => insertCoin()
    case Turn => turnKnob()
  }
}

// The rules are:
//  - Inserting a coin into a locked machine will cause it to unlock if there is candy left
//  - Turning the knob on an unlocked machine will cause it to dispense candy and become locked
//  - Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing
//  - A machine that's out of candy ignores all inputs

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
  for {
    _ <- State.sequence(inputs.map((i: Input) => State.modify((m: Machine) => m.input(i))))
    m <- State.get
  } yield (m.coins, m.candies)

// This way does suffer from one significant problem: it can't be tail call optimized...
// Hmm...
def simulateMachineVerbose(inputs: List[Input]): State[Machine, (Int, Int)] =
  inputs match {
    case i :: is =>
      for {
        _ <- State.modify[Machine](_.input(i))
        _ <- simulateMachineVerbose(is)
        m <- State.get[Machine]
      } yield (m.coins, m.candies)
    case _ =>
      for { m <- State.get }
      yield (m.coins, m.candies)
  }

{
  val res1 = simulateMachine(List(Coin, Turn, Turn, Coin, Coin, Turn, Turn)).run(Machine(true, 5, 0))
  println("Simulate machine with 5 coins after 2 coin turns: %s".format(res1))

  val res2 = simulateMachineVerbose(List(Coin, Turn, Turn, Coin, Coin, Turn, Turn)).run(Machine(true, 5, 0))
  println("Simulate machine (verbose) with 5 coins after 2 coin turns: %s".format(res2))
}

// To understand this better, let's show what it looks like for a single action:
val sim2: State[Machine, (Int,Int)] = for {
  _ <- State.modify[Machine]((m: Machine) => m.input(Turn))
  m <- State.get
} yield (m.coins, m.candies)

println("Simulate unlocked machine with 1 turn: %s".format(sim2.run(Machine(false, 1, 0))))
