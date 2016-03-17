
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

  // Pattern matching can also be used outside functions
  3 match {
    case 0 => "zero"
    case 1 => "one"
    case _ => "other"
  }

  val any: Any = "Banana"
  any match {
    case x: Int => "int"
    case y: String => "string"
    case _ => "other"
  }

  // Case classes (which Cons is!) can be used to match
  case class Person(name: String)

  def barakTest(p: Person): String = p match {
    case Person("Barak") => "wooo"
    case _ => "awww"
  }

  val p0 = Person("Kenrick")
  println("Test %s: %s".format(p0, barakTest(p0)))

  val p1 = Person("Barak")
  println("Test %s: %s".format(p1, barakTest(p1)))

  // This doesn't work!
  //class Person(name: String)
  //def barakTest(p: Person): String = p match {
  //  case Person("Barak") => "wooo"
  //  case _ => "awww"
  //}

  // Let's try a weird one from the book
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
  println("I expect %d to be: 1 + 2 = 3".format(x))
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

  def apply [A] (arr: A*): List[A] =
    if (arr.isEmpty) Nil
    else Cons(arr.head, apply(arr.tail:_*))

  def head [A] (l: List[A]): A = l match {
    case Nil => throw new Exception("Uh oh")
    case Cons(x, _) => x
  }

  def tail [A] (l: List[A]): List[A] = l match {
    case Nil => throw new Exception("Uh oh")
    case Cons(_, x) => x
  }

  def isEmpty [A] (l: List[A]): Boolean = l match {
    case Nil => true
    case _ => false
  }
}

println("Fill %d, %d: %s".format(3, 1, List.fill(3, 1)))
println("List constructor: %s".format(List(3, 2, 1)))

val list = List(3, 2, 1)
println("Head of %s: %s".format(list, List.head(list)))
println("Tail of %s: %s".format(list, List.tail(list)))
println("IsEmpty %s: %s".format(list, List.isEmpty(list)))
println("IsEmpty %s: %s".format(Nil, List.isEmpty(Nil)))

{
  // In general, immutable data structures use data sharing.
  // In this example, list1 shares list0's data and adds a value to it.
  val list0 = List(1, 2, 3)
  val list1 = Cons(0, list0)

  // Here, we replace the head of a list
  def setHead [A] (l: List[A], newHead: A): List[A] = {
    Cons(newHead, List.tail(l))
  }
  val list2 = setHead(list1, 5)

  println(list0)
  println(list1)
  println(list2)

  // Here, we drop some elements from a list
  def drop [A] (l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case n => l match {
      case Nil => Nil
      case _ => drop(List.tail(l), n - 1)
    }
  }

  println("Dropping the first %d elements of %s: %s".format(2, list1, drop(list1, 2)))
  println("Dropping the first %d elements of %s: %s".format(10, list1, drop(list1, 10)))

  def dropWhile [A] (l: List[A], shouldDrop: A => Boolean) : List[A] = {
    if (List.isEmpty(l)) Nil
    else if (shouldDrop(List.head(l))) dropWhile(List.tail(l), shouldDrop)
    else l
  }

  var isPositive = (x: Int) => x >= 0
  var list3 = List(1, 2, -1, 3, 4)
  println("Dropwhile positive for %s: %s".format(list3, dropWhile(list3, isPositive)))

  def append [A] (l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(x, xs) => Cons(x, append(xs, l2))
  }

  var list4 = List('a', 'b')
  var list5 = List('c', 'd')
  println("Appending %s and %s gives %s".format(list4, list5, append(list4, list5)))

  def removeLast [A] (l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, removeLast(xs))
  }
  println("Removing last from %s gives %s".format(list1, removeLast(list1)))
}
