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
// Let's write a lazy list implementation.
sealed trait Stream[+A] {
  def printFirst[A] (num: Int): String =
    if (num <= 0) ""
    else this match {
      case SCons(fx, fxs) => fx().toString() + fxs().printFirst(num - 1)
      case SNil => ""
    }

  def take (n: Int): Stream[A] =
    if (n <= 0) SNil
    else this match {
      case SCons(fx, fxs) => Stream.cons(fx(), fxs().take(n - 1))
      case SNil => SNil
    }

  def takeWhile (shouldTake: A => Boolean): Stream[A] =
    this match {
      case SCons(fx, fxs) =>
        if (shouldTake(fx())) Stream.cons(fx(), fxs().takeWhile(shouldTake))
        else SNil
      case SNil => SNil
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
      if (shouldTake(x)) Stream.cons(x, xs)
      else SNil
    })

  def headOption (): Option[A] = this match {
    case SCons(fx, _) => Some(fx())
    case _ => None
  }

  def headOptionObfuscated (): Option[A] =
    foldRight(None: Option[A])((x, _) => Some(x))

  def map [B] (f: A => B): Stream[B] =
    foldRight(SNil: Stream[B])((a, bs) => {
      Stream.cons(f(a), bs)
    })

  def filter (f: A => Boolean): Stream[A] =
    foldRight(SNil: Stream[A])((a, as) => {
      if (f(a)) Stream.cons(a, as)
      else as
    })

  def append [AA>:A] (s: Stream[AA]): Stream[AA] =
    foldRight(s)((a, bs) => Stream.cons(a, bs))

  def flatMap [B] (g: A => Stream[B]): Stream[B] =
    foldRight(SNil: Stream[B])((a, bs) => g(a).append(bs))

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
