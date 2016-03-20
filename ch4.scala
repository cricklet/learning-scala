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
// Let's call ours 'Optn' to avoid namespace collision.
sealed trait Optn[+A] {
  def map [B] (f: A => B): Optn [B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }

  // "B >: A" means that A 'is a' B
  // "default: => B" means that 'default' won't be evaluated until it's needed (laziness)
  def getOrElse [B >: A] (default: => B): B = this match {
    case Some(v) => v
    case None => default
  }

  //def flatMap [B] (f: A => Optn[B]): Optn [B] = {
  //  // this.map(f) returns Optn[Optn[B]] eeeek
  //  this.map(f) getOrElse None
  //}

  def flatMap [B] (f: A => Optn[B]): Optn [B] = this match {
    case Some(v) => f(v)
    case None => None
  }

  // def orElse [B >: A] (default: => Optn[B]): Optn [B] =
  //  map(a => Some(a)) getOrElse default

  def orElse [B >: A] (default: => Optn[B]): Optn [B] = this match {
    case None => default
    case _ => this // Some(a) => Some(a)
  }

  // def filter (f: A => Boolean): Optn [A] =
  //  flatMap(a => if (f(a)) Some(a) else None)

  def filter (f: A => Boolean): Optn [A] = this match {
    case Some(v) if f(v) => Some(v)
    case _ => None
  }
}
case class Some[+A] (get: A) extends Optn[A]
case object None extends Optn[Nothing]

def mean (xs: Seq[Double]): Optn[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

println("Mean of an empty list: %s".format(mean(List())))
println("Mean of an non-empty list: %s".format(mean(List(1.0, 1.5))))

// When transforming an 'Option,' you generally use 'map' or 'flatMap'.
// Consider:
//   val employee: Option[Employee] = getEmployee(...)

// If we want to transform employee with a function that always suceeds:
//   val department: Option[Department] = employee.map(_.department)

// If we want to transform employee with a function that sometimes fails
// (i.e. it returns an Option)
//   val manager: Option[Manager] = employee.map(_.manager)

def variance (xs: Seq[Double]): Optn[Double] = {
  // If we could calculate the variance of 'xs' after computing the mean 'm'
  // of 'xs' without calling *any* functions that could fail, then we could use
  // '.map()'.

  // Unfortunately, we need to call 'mean(...)' again. Thus, we need to use 'flatMap'.
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}
