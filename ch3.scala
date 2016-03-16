
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

