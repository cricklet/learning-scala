
// List datatype, parameterized on type A
sealed trait List[+A]

// Data constructors of List
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// Let's construct some simple lists
Cons(2, Nil)
Cons(2, Cons(3, Nil))
val intList: List[Int] = Nil

// Because of the "covariant" type annotation [+A],
// we can include dogs in a list of animals.
class Animal
class Dog extends Animal

val animalList: List[Animal] = Cons(new Dog(), Nil)
// val dogList: List[Dog] = Cons(new Animal(), Nil) // The reverse is not true!

{
  // Quick sidebar: pattern matching!
  def sum (ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(0, xs) => sum(xs) // a silly example
    case Cons(x, xs) => x + sum(xs)
  }

  val list = Cons(3, Cons(0, Cons(2, Nil)))
  println("Sum of %s is %d".format(list, sum(list)))
}

{
  // Another quick sidebar: variadic function syntax
  // "ints: Int*" is a variable-length argument list like "*args" in python.
  def sum (ints: Int*): Int =
    if (ints.isEmpty) 0
    else (ints.head + sum(ints.tail:_*))

  // To unpack an array to pass into a function, use ":_*"
  val arr = Array(1, 2, 3)
  println("Sum of %s is %d".format(arr, sum(arr:_*)))

  println("Sum of %d, %d, %d is %d".format(1, 2, 3, sum(1, 2, 3)))
}

// We can create a "companion object" for our datatype.
object List {
  def fill [A] (n: Int, a: A): List[A] = n match {
    case 0 => Nil
    case n => Cons(a, fill(n - 1, a))
  }
}

List.fill(3, 1)
