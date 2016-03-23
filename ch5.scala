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

  // You can use a thunk in a constructor argument
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
