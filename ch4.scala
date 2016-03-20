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

println("Variance of an empty list: %s".format(variance(List())))
println("Variance of an non-empty list: %s".format(variance(List(1.0, 1.5))))

// Sometimes, we want to turn an undefined 'Option' into an exception. In this case,
// we can simply do: option.getOrElse(throw new Exception("Uh oh"))

// This should generally *only be done* if no reasonable program would ever catch
// the exception.

// This matches what the Practical Programmer says about exceptions:

//   "We believe that exceptions should rarely be used as part of a
//    program's normal flow; exceptions should be reserved for unexpected
//    events. Assume that an uncaught exception will terminate your program
//    and ask yourself, 'Will this code still run if I remove all the
//    exception handlers?' If the answer is 'no,' then maybe exceptions are
//    being used in nonexceptional circumstances."

// In PP, they provide this example of a function that should throw an exception:
//     public void openPasswd() throws FileNotFoundException {
//        ipstream = new FileInputStream("/etc/passwd");
//        ...
//      }

// And this example of a function that should not throw an exception except in
// abnormal circumstances:
//     public void openFile(String filename) throws FileNotFoundException {
//       File f = new File(filename);
//       if (!f.exists())
//          return false; // error handling NOT exception throwing!
//
//       ipstream = new FileInputStream(f);
//       return true;
//     }

// It seems to me that PP's second case could benifit from Scala's 'Option'.
// After all, it's clear that 'openFile(...)' should ideally actually return the
// file stream that was opened.
//     def openFile(filename: String): Option[FileInputStream] = {
//       val f: File = new File(filename)
//       if (!f.exists()) None
//       else Some(new FileInputStream(f))
//     }
