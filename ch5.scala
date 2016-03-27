import Stream._

{
  // Non-strictness: a proprety of a function where the function may choose not
  // to evaluate one or more of its arguments.

  // In contrast, a strict function always evaluates its arguments.

  // Functions in Scala are, by default, strict.

  // Boolean operators are non-strict.
  // For example, neither of these evaluate the right-side.
  true || { println("!!"); false }
  false && { println("!!"); true }

  // If statements are similarly non-strict.
  if (false) println("!!")
}
{
  // Let's try to write our own if statement:
  def badIf [A] (cond: Boolean, onTrue: () => A, onFalse: () => A): A =
    if (cond) onTrue() else onFalse()

  badIf(true, () => "It was true", () => "It was false")

  // Thunk: the unevaluated form of an expression.
  // In 'badIf', onTrue & onFalse are like explicitly created "thunks."

  // However, what we really want to do is pass in a normal expression
  // as a "thunk". This is sometimes referred to as call-by-name rather
  // than call-by-value.
  def goodIf [A] (cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  goodIf(true, println("This should be printed"), println("This should NOT be printed"))

  // Note that a "thunk" is evaluated as many times as it's called!
  var counter = 0
  def count() = { counter = counter + 1; counter }

  def evalThunks3 [A] (thunk: => A): A = { thunk; thunk; thunk }
  println("count() 3 times: %s".format(evalThunks3(count())))

  // You can use a thunk as a constructor param.
  class AttemptingToThunk (thunk: => Int) {
    def think (): Int = thunk
  }

  new AttemptingToThunk({ println("This won't run."); 3 })
  new AttemptingToThunk({ println("This will run."); 3 }).think()
}

{
  // Quick aside... I need to understand constructor parameters.
  class Counter (
    i: Int, // this is "val" by default!
    val j: Int,
    var k: Int
  ) {
    def count() = {
      // These are illegal because a "val" cannot change!
      // i = i + 1
      // j = j + 1

      // This is fine, because "var" can change.
      k = k + 1
      k
    }
  }

  val c = new Counter(0, 0, 0)
  println("Counting %d.".format(c.count()))
  println("Counting %d.".format(c.count()))
  println("Counting %d.".format(c.count()))
  println("Counting %d.".format(c.count()))

  class Scopes (
    a: Int, // just a param
    var b: Int, // public (mutable)
    val c: Int, // public
    private var d: Int, // class private
    private val e: Int, // class private
    private[this] val f: Int // instance private
  ) {
    override def equals(obj: Any) = obj match {
      case other: Scopes =>
        // a == other.a // Can't because 'a' is simply a constructor param, not a field
        b == other.b &&
        c == other.c &&
        d == other.d &&
        e == other.e
        // f == other.f // Can't because 'f' is private[this]!
      case _ => false
    }
  }

  val s = new Scopes(0,1,2,3,4,5)
  // s.a
  println(s.b)
  println(s.c)
  // s.d
  // s.e
  // s.f

  // Note that because 'a' is simply a constructor param and  'f' is 'private[this]',
  // they could not be included in the "equals" computations
  println("Scopes are the same: %s".format(new Scopes(0,0,0,0,0,0) == new Scopes(1,0,0,0,0,1)))

  // Running 'javap' on this results in:
  //   Compiled from "Scopes.scala"
  //   public class Scopes {
  //      public int b();
  //      public void b_$eq(int);
  //      public int c();
  //      public Scopes(int, int, int, int, int);
  //   }
}
{
  // Now, let's talk about the "lazy" keyword.
  // http://stackoverflow.com/questions/7484928/what-does-a-lazy-val-do

  val x = { println("This runs automatically."); 123 }
  lazy val y = { println("This runs lazily. It should only run once!"); 456 }

  println("When I evaluate 'y', it's resulting value will be cached.")

  println(y)
  println(y)
  println(y)

  // This is useful to have infinite data structures.
  class BadInfInt (value: Int) {
    val next = new BadInfInt(value + 1)
  }

  class InfInt (value: Int) {
    lazy val next = new InfInt(value + 1)
    override def toString = "%d".format(value)
  }

  // This creates a StackOverflowError!
  // new BadInfInt(1)

  val i = new InfInt(1)
  println("Here's a inf series of integers: %s, %s, %s...".format(i, i.next, i.next.next))
}
// Let's write a lazy list implementation.
sealed trait Stream[+A] {
  def printFirst[A] (num: Int): String =
    if (num <= 0) ""
    else this match {
      case SCons(fx, fxs) => fx().toString() + fxs().printFirst(num - 1)
      case SNil => ""
    }

  def take (n: Int): Stream[A] =
    this match {
      case SCons(fx, fxs) => {
        if (n <= 0) empty
        if (n == 1) cons(fx(), empty)
        else cons(fx(), fxs().take(n - 1))
      }
      case SNil => empty
    }

  def takeWhile (shouldTake: A => Boolean): Stream[A] =
    this match {
      case SCons(fx, fxs) =>
        if (shouldTake(fx())) cons(fx(), fxs().takeWhile(shouldTake))
        else empty
      case SNil => empty
    }

  def toList (): List[A] = this match {
    case SNil => Nil
    case SCons(fx, fxs) => fx() :: fxs().toList()
  }

  // Remember:
  // "z: => B" means that 'z' is lazily evaluated
  // "(A, =>B) => B" means that the second argument is lazily evaluated
  def foldRight [B] (value: => B) (g: (A, =>B) => B): B = this match {
    case SCons(fx, fxs) => g(fx(), fxs().foldRight(value)(g))
    case _ => value
  }

  def forAll (toBoolean: A => Boolean): Boolean =
    foldRight(true)((x, bool) => toBoolean(x) && bool)

  def takeWhileWithFold (shouldTake: A => Boolean): Stream[A] =
    foldRight(SNil: Stream[A])((x, xs) => {
      if (shouldTake(x)) cons(x, xs)
      else empty
    })

  def headOption (): Option[A] = this match {
    case SCons(fx, _) => Some(fx())
    case _ => None
  }

  def tail (): Stream[A] = this match {
    case SCons(_, fxs) => fxs()
    case _ => empty
  }

  def headOptionObfuscated (): Option[A] =
    foldRight(None: Option[A])((x, _) => Some(x))

  def map [B] (f: A => B): Stream[B] =
    foldRight(SNil: Stream[B])((a, bs) => {
      cons(f(a), bs)
    })

  def filter (f: A => Boolean): Stream[A] =
    foldRight(SNil: Stream[A])((a, as) => {
      if (f(a)) cons(a, as)
      else as
    })

  def append [AA>:A] (s: Stream[AA]): Stream[AA] =
    foldRight(s)((a, bs) => cons(a, bs))

  def flatMap [B] (g: A => Stream[B]): Stream[B] =
    foldRight(SNil: Stream[B])((a, bs) => g(a).append(bs))

  // I'm not quite convinced of the benifit of using higher order functions
  // like this... It seems less readable :/
  def mapViaUnfold [B] (f: A => B): Stream[B] =
    Stream.unfold(this: Stream[A])(_ match {
      case SCons(x, xs) => Some((f(x()), xs()))
      case _ => None
    })

  def takeViaUnfold (n: Int): Stream[A] =
    Stream.unfold((this, n))(_ match {
      case (_, 0) => None
      case (SCons(x, xs), n) => Some((x(), (xs(), n-1)))
      case _ => None
    })

  def takeWhileViaUnfold (f: A => Boolean): Stream[A] =
    Stream.unfold(this)(_ match {
      case SCons(fx, fxs) => {
        lazy val x = fx()
        lazy val xs = fxs()
        if (f(x)) Some(x, xs)
        else None
      }
      case _ => None
    })

  def zipWith [B, C] (otherStream: Stream[B])(f: (A, B) => C): Stream[C] =
    // Uh...
    Stream.unfold((this, otherStream))(_ match {
      case (SCons(fa, fas), SCons(fb, fbs)) => {
        val c = f(fa(), fb())
        Some(c, (fas(), fbs()))
      }
      case _ => None
    })

  def zipAll[B](otherStream: Stream[B]): Stream[(Option[A],Option[B])] =
    // Oh lord.
    Stream.unfold((this, otherStream))(_ match {
      case (SNil, SNil) => None
      case (SCons(fa, fas), SNil) => Some((Some(fa()), None), (fas(), empty))
      case (SNil, SCons(fb, fbs)) => Some((None, Some(fb())), (empty, fbs()))
      case (SCons(fa, fas), SCons(fb, fbs)) => {
        Some((Some(fa()), Some(fb())), (fas(), fbs()))
      }
    })

  // Implementing startsWith: perhaps I can use unfold + forAll()?
  def startsWith [B] (other: Stream[B]): Boolean =
    this.zipAll(other)
    .takeWhileViaUnfold({
      // Note: for an anonymous function like "_ match {}", I can omit the "_ match"
      case (_, None) => false
      case _ => true
    })
    .forAll({
      case (x, y) => x == y
    })

  def tails (): Stream[Stream[A]] = this match {
    case SNil => SNil
    case _ => cons(this, this.tail.tails())
  }

  def forAny (toBoolean: A => Boolean): Boolean = this match {
    case SCons(fx, fxs) => {
      if (toBoolean(fx())) true
      else fxs().forAny(toBoolean)
    }
    case _ => false
  }

  def hasSubsequence [B] (sub: Stream[B]): Boolean =
    tails().forAny(_.startsWith(sub))
}

// Unfortunately, you can't use a thunk (call-by-name) as a constructor
// parameter for case classes (because case class params are defaulted
// to public 'val's)

// So, we will have to use explicit thunks
case object SNil extends Stream[Nothing]
case class SCons[+A] (x: () => A, xs: () => Stream[A]) extends Stream[A]

// Luckily, we can write helper methods that hide those ugly explicit
// thunks behind an interface that can use call-by-name thunks.
object Stream {
  def cons[A] (x: => A, xs: => Stream[A]): Stream[A] = {
    lazy val y = x
    lazy val ys = xs
    SCons(() => y, () => ys)
  }
  def empty[A]: Stream[A] = SNil

  def unfold [A, S](state: S)(f: S => Option[(A, S)]): Stream[A] =
    f(state) match {
      case Some((a, newState)) => Stream.cons(a, unfold(newState)(f))
      case _ => SNil
    }

  def apply[A] (args: A*): Stream[A] =
    // This helper actually completely destroys the purpose of a lazy
    // Stream data type!!! The variadic argument array 'args' cannot
    // include thunks.
    //
    // In other words, if you do Stream({ println("Blah"); 1 }), then
    // "Blah" will be printed!
    if (args.isEmpty) empty
    else cons(args.head, apply(args.tail: _*))
}

// Man, this Stream constructor sucks. It defeats the purpose of the
// lazy Cons underlying it.
Stream({ println("Wtf :("); 1 })

// But that's okay. I guess I can just use 'cons'.
Stream.cons({ println("At least this doesn't run."); 1 }, Stream.empty)

// Well, considering I can't use the constructor...
val s = Stream.cons({ println("First."); 1 },
  Stream.cons({ println("Second."); 2 },
    Stream.cons({ println("Third."); 3 },
      Stream.cons({ println("Fourth."); 4 },
        Stream.empty)))) // Lol yay lisp

println("Generated stream of 4 elements: %s".format(s))
println("Here's the first 2: %s".format(s.printFirst(2)))
println("Here's the stream as a list: %s".format(s.toList()))
println("Here's the the first 2 as a stream: %s".format(s.take(2)))
println("Here's the the first 2 as a stream turned into a list: %s".format(s.take(2).toList()))
println("Take while less than 3: %s".format(s.takeWhile(x => x < 3).toList()))

// Gotta reset to test that my forAll works properly
val t = Stream.cons({ println("First."); 1 },
  Stream.cons({ println("Second."); 2 },
    Stream.cons({ println("Third."); 3 },
      Stream.cons({ println("Fourth."); 4 },
        Stream.empty)))) // Lol yay lisp

println("Is the stream all less than 3? %s".format(t.forAll(_ < 2)))
println("Is the stream all greater than -1? %s".format(t.forAll(_ > -1)))
println("Take while (w/ fold) less than 3: %s".format(s.takeWhileWithFold(x => x < 3).toList()))
println("Map increment? %s".format(t.map(_ + 1).toList()))
println("Filter even? %s".format(t.filter(_ % 2 == 0).toList()))
println("Appending Stream(5): %s".format(t.append(Stream(5)).toList()))

println("headOption of [1]: %s".format(Stream(1).headOption()))
println("headOption of []: %s".format(Stream().headOption()))

println("headOption of [1]: %s".format(Stream(1).headOptionObfuscated()))
println("headOption of []: %s".format(Stream().headOptionObfuscated()))

// Note: these functions are incremental!
val u = Stream.cons({ println("First."); 1 },
  Stream.cons({ println("Second."); 2 },
    Stream.cons({ println("Third."); 3 },
      Stream.cons({ println("Fourth."); 4 },
        Stream.empty)))) // Lol yay lisp

// This is similar to an optimized for-loop. In fact, many people describe
// streams as "first class loops."
println("Mapping the first 2 elements: %s".format(u.map(_+10).take(2).toList()))

// Let's do some infinite streams.
val infOnes: Stream[Int] = Stream.cons(1, infOnes)
println("5 ones: %s".format(infOnes.take(5).toList()))

def infConst [A] (a: A): Stream[A] = {
  infOnes.map(x => a)
}
println("5 bananas: %s".format(infConst("banana").take(5).toList()))

def infInc (n: Int): Stream[Int] = {
  Stream.cons(n, infInc(n+1))
}

println("Infinite increment for 5: %s".format(infInc(0).take(5).toList()))

def fib (): Stream[Int] = {
  lazy val _fib: (Int, Int) => Stream[Int] =
    (n0: Int, n1: Int) => Stream.cons(n0, _fib(n1, n0 + n1))
  _fib(0, 1)
}

println("Infinite fibonacci for 5: %s".format(fib().take(5).toList()))

// Recursive functions generally consume data by breaking it up into smaller
// and smaller chunks -- until the function terminates.

// Corecursive functions generally produce data. They terminate when they are
// no longer productive.

// Unfold is, of course, a corecursive function. Here, we can use it to generate
// infinite lists.

println("Unfold: 5 bananas: %s".format(
  Stream.unfold(None)(_ => Some(("banana", None)))
  .take(5).toList()
))
println("Unfold: infinite increment for 5: %s".format(
  Stream.unfold(0)(n => Some((n, n + 1)))
  .take(5).toList()
))
println("Unfold: infinite fib for 5: %s".format(
  Stream.unfold(
    (0, 1)
  )(_ match {
    case (n0, n1) => Some(n0, (n0, n1))
  })
  .take(5).toList()
))

// Let's make sure that our viaUnfold functions work
println("Implementing the other HOFs using unfold: %s".format(
  fib().mapViaUnfold(_+10).takeViaUnfold(10).takeWhileViaUnfold(_ < 15).toList()
))

// Just making sure these things work...
println("Implementing zipAll using unfold: %s".format(
  fib().take(10).zipAll(infInc(0).take(5)).toList()
))

// And that startsWith works...
val x = Stream(1,2,3)
val y = Stream(1,2,3,4,5)
println("%s starts with %s: %s".format(
  x.toList(),
  y.toList(),
  x.startsWith(y)
))
println("%s starts with %s: %s".format(
  x.toList(),
  x.toList(),
  x.startsWith(x)
))
println("%s starts with %s: %s".format(
  y.toList(),
  x.toList(),
  y.startsWith(x)
))

// And that tails works
println("Tails: %s".format(
  Stream(1,2,3).tails().map(_.toList()).toList()
))

// And that hasSubsequence works
val l = Stream(1,2,3,4,5)
println("Testing hasSubsequence. All of these should be true: %s".format(
  List(
    Stream(1,2),
    Stream(3,4),
    Stream(4,5)
  ).map(l.hasSubsequence(_))
))

println("Testing hasSubsequence. All of these should be false: %s".format(
  List(
    Stream(0,2),
    Stream(2,4),
    Stream(4,6),
    Stream(7)
  ).map(l.hasSubsequence(_))
))
