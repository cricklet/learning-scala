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
  // as a "thunk":
  def goodIf [A] (cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  goodIf(true, println("This should be printed"), println("This should NOT be printed"))

  // Note that a "thunk" is evaluated as many times as it's called!
  var counter = 0
  def count() = { counter = counter + 1; counter }

  def evalThunks3 [A] (thunk: => A): A = { thunk; thunk; thunk }
  println("count() 3 times: %s".format(evalThunks3(count())))
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
