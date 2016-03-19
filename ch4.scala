// Hide 'Option' and 'Either'
import scala.{Option => _, Either => _, _}

{
  // What's so bad about exceptions?
  // Well, if you're trying to do FP, they're not referentially transparent!

  try {
    // An exception does different things depending on the cases handled by the surrounding try-catch
    throw new Exception("Uh oh")
  } catch {
    case e: Exception => println(e)
  }

  try {
    // Exceptions can have any type! They are *not* typesafe.
    val y: Int = throw new Exception("Butterfingers")
  } catch {
    case e: Exception => println(e)
  }

  // Java clearly understands that exceptions are dangerous-- afterall it uses checked expressions.
  // i.e. "private void xyz() throws BlahException {}" forces the caller to handle the exception.

  // However, this doesn't work for generic higher-order functions!

  // We can't possibly have "map[A,B](l: List[A])(f: A => B): List[B]" specifically re-raise all
  // possible exceptions. How would the caller of "map" know what exceptions it should handle?

  // Of course, exceptions *are* useful. They let us 'throw' anywhere and 'catch' with consolidated
  // error-handling logic.
}

{
  // Here's a function that throws
  def mean0 (xs: Seq[Double]): Double =
    if (xs.isEmpty) throw new Exception("Bullfrog")
    else xs.sum / xs.length

  // This will throw an error!
  // println("Mean of an empty list: %s".format(mean0(List())))

  // Here's a function that returns a bogus value
  def mean1 (xs: Seq[Double]): Double =
    if (xs.isEmpty) 0.0 / 0.0
    else xs.sum / xs.length

  // This bogus value could propogate and cause problems later :(
  println("Mean of an empty list: %s".format(mean1(List())))

  // Here's a function that takes an 'onEmpty' parameter
  def mean2 (xs: Seq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length

  // Unfortunately, this forces the *immediate* caller to know exactly how to handle undefined cases.
  println("Mean of an empty list: %s".format(mean2(List(), 0.0 / 0.0)))
}

// Instead, we can use 'Option' (which is like Haskell's "Maybe")
sealed trait Option[+A] {
  def map [B] (f: A => B): Option [B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }

  // "B >: A" means that A 'is a' B
  def getOrElse [B >: A] (default: => B): B = this match {
    case Some(v) => v
    case None => default
  }
}
case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

def mean (xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

println("Mean of an empty list: %s".format(mean(List())))
println("Mean of an non-empty list: %s".format(mean(List(1.0, 1.5))))

