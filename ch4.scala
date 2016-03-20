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
    case Sm(v) => Sm(f(v))
    case Nn => Nn
  }

  // "B >: A" means that A 'is a' B
  // "default: => B" means that 'default' won't be evaluated until it's needed (laziness)
  def getOrElse [B >: A] (default: => B): B = this match {
    case Sm(v) => v
    case Nn => default
  }

  //def flatMap [B] (f: A => Optn[B]): Optn [B] = {
  //  // this.map(f) returns Optn[Optn[B]] eeeek
  //  this.map(f) getOrElse Nn
  //}

  def flatMap [B] (f: A => Optn[B]): Optn [B] = this match {
    case Sm(v) => f(v)
    case Nn => Nn
  }

  // def orElse [B >: A] (default: => Optn[B]): Optn [B] =
  //  map(a => Sm(a)) getOrElse default

  def orElse [B >: A] (default: => Optn[B]): Optn [B] = this match {
    case Nn => default
    case _ => this // Sm(a) => Sm(a)
  }

  // def filter (f: A => Boolean): Optn [A] =
  //  flatMap(a => if (f(a)) Sm(a) else Nn)

  def filter (f: A => Boolean): Optn [A] = this match {
    case Sm(v) if f(v) => Sm(v)
    case _ => Nn
  }
}
case class Sm[+A] (get: A) extends Optn[A]
case object Nn extends Optn[Nothing]

def mean (xs: Seq[Double]): Optn[Double] =
  if (xs.isEmpty) Nn
  else Sm(xs.sum / xs.length)

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

{
  // How can functions that are built to work with simple values be applied
  // to Option values?

  // Lift!
  def lift [A, B] (f: A => B) : Option[A] => Option[B] =
    optA => optA.map(f)

  val optX: Option[Int] = Some(-4)
  val absX = lift(math.abs)(optX)
  println("abs of %s is %s".format(optX, absX))

  // What about combining two Option values using a function?
  def map2 [A, B, C] (a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(av), Some(bv)) => Some(f(av, bv))
  }

  // Although, I tentatively disagree with the book on this. I think writing
  // a function 'lift2' that operates like 'lift' is clearer...
  def lift2 [A, B, C] (f: (A, B) => C): (Option[A], Option[B]) => Option[C] =
    (optA, optB) => (optA, optB) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a), Some(b)) => Some(f(a, b))
    }

  val add = (x: Int, y: Int) => x + y
  val liftedAdd = lift2(add)

  println("add %s %s => %s".format(1, 2, add(1, 2)))
  println("liftedAdd %s %s => %s".format(Some(1), Some(2), liftedAdd(Some(1), Some(2))))
  println("liftedAdd %s %s => %s".format(Some(1), None, liftedAdd(Some(1), None)))
  println("liftedAdd %s %s => %s".format(None, Some(2), liftedAdd(None, Some(2))))
}
{
  def sequence [A] (a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case Some(v) :: xs => sequence(xs).map(v :: _)
    case None :: xs => sequence(xs)
  }

  val list0 = List(Some(0), None, Some(2), Some(3), None)
  println("%s turns into %s when passed through sequence".format(list0, sequence(list0)))
}
