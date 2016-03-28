import State._

// Traits are like interfaces from Java. They can be partially implemented
// (i.e. like an abstract class)

// Here's a trait describing a random number generator:
trait RNG {
  def nextInt: (Int, RNG) // Return both the random int AND a new RNG
}

case class SimpleRNG (seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

// Every time we generate a new random integer, we also generate a new RNG.
val rng0 = SimpleRNG(42)
val (n1, rng1) = rng0.nextInt
val (n2, rng2) = rng1.nextInt

println("Random numbers: %s, %s".format(n1, n2))

// This leads to some tedium
def randomPair(rng0: RNG): ((Int, Int), RNG) = {
  val (n1, rng1) = rng0.nextInt
  val (n2, rng2) = rng1.nextInt
  ((n1, n2), rng2)
}

println("Random pair: %s".format(randomPair(rng2)))

def positiveInt(rng0: RNG): (Int, RNG) = {
  val (n, rng1) = rng0.nextInt
  if (n < 0) (- (n + 1), rng1)
  else (n, rng1)
}

println("Positive int: %s".format(positiveInt(rng2)))

def double(rng0: RNG): (Double, RNG) = {
  val (p, rng1) = positiveInt(rng0)
  (p.toDouble / Int.MaxValue.toDouble, rng1)
}

println("Double: %s".format(double(rng2)))

// Such awkward! Does this mean functional is awkward?
// According to the authors, it does not; awkwardness is almost always a sign
// of some missing abstraction waiting to be discovered.

// In this case, all the functions of type "RNG => (A, RNG)" are 'state actions'
// or 'state transitions'.

// They can be combined using 'combinators' which are a kind of higher order
// functions.

// Here's a state action: (given some state RNG, generates an A and a new state)
type Rand[+A] = RNG => (A, RNG)

val randInt: Rand[Int] = _.nextInt // takes in RNG, spits out (Int, RNG)
println("Random int: %s".format(randInt(rng2)))

// This will help us because we want to avoid explicitly passing along RNG
// state! We kind of get a DSL that passes the state for us.

// Consider the 'unit' action which passes the RNG state through without using
// it-- insteadl returning a constant.
def unit [A] (a: A): Rand[A] =
  rng => (a, rng)

println("Unit of 1: %s".format(unit(1)(rng2)))

// Sometimes we want to transform the output of a 'state action' without
// modifying the state itself.
def map [A, B] (action: Rand[A]) (f: A => B): Rand[B] =
  rng => {
    val (a, rng2) = action(rng)
    (f(a), rng2)
  }

def positiveEven: Rand[Int] =
  map(positiveInt)(i => i - i % 2)

println("Positive even #: %s".format(positiveEven(rng2)))

def doubleViaMap: Rand[Double] =
  map(positiveInt)(i => i.toDouble / Int.MaxValue.toDouble)

println("Double via map: %s".format(doubleViaMap(rng2)))

def map2 [A, B, C] (ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng0 => {
    val (a, rng1) = ra(rng0)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

def both [A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
  map2(ra, rb)((_,_))

def intDouble: Rand[(Int, Double)] =
  both(randInt, double)

println("Int, double: %s".format(intDouble(rng2)))

// Sometimes we want to perform lots of state actions, but want it to appear
// as though we're only performing one state action. It's basically function
// composition.
def sequence [A] (actions: List[Rand[A]]): Rand[List[A]] =
  actions match {
    case f :: fs => map2(f, sequence(fs))(_::_)
    case Nil => (rng => (Nil, rng))
  }

println("Sequence of randoms: %s".format(
  sequence(List(randInt, positiveEven))(rng2)
))

def randomInts(n: Int): Rand[List[Int]] =
  sequence(List.fill(n)(randInt))

println("Sequence of random ints: %s".format(
  randomInts(10)(rng2)
))

def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
  rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

def nonNegativeLessThan(n: Int): Rand[Int] =
  flatMap(randInt)(x => {
    if (Int.MaxValue - x < n) nonNegativeLessThan(n)
    else unit(x % n)
  })

println("Rand less than 10: %s".format(
  nonNegativeLessThan(10)(rng2)
))

// We can implement map & map2 using flatMap.
def mapViaFlatMap [A, B] (action: Rand[A]) (f: A => B): Rand[B] =
  flatMap(action)(a => unit(f(a)))

def map2ViaFlatMap [A, B, C] (ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  flatMap(ra)(a => {
    flatMap(rb)(b => {
      unit(f(a, b))
    })
  })

println("mapViaFlatMap: %s".format(
  mapViaFlatMap(randInt)(_ % 100)(rng2)
))

println("map2ViaFlatMap: %s".format(
  map2ViaFlatMap(randInt, randInt)
  ((x, y) => (x % 100, y % 100))(rng2)
))

// Our type 'Rand' can be generalized to any kind of 'State':
// i.e. type State[S,+A] = S => (A,S)
//      type Rand[A] = State[RNG, A]

case class State[S,+A](run: S => (A, S)) {
  def flatMap [B](g: A => State[S,B]): State[S,B] =
    State(state0 => {
      val (a, state1) = run(state0)
      g(a).run(state1)
    })

  def map [B] (f: A => B): State[S,B] =
    flatMap(a => unit(f(a)))

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
}
