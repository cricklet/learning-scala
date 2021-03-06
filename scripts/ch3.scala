{
  // List datatype, parameterized on type A
  sealed trait List[+A]

  // Data constructors of List
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A] {
    override def toString = "C(%s, %s)".format(head, tail)
  }

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

    def dropWhile [A] (l: List[A], shouldDrop: A => Boolean) : List[A] = l match {
      // You can include 'if' statements within a 'case'!
      case Cons(x, xs) if shouldDrop(x) => dropWhile(xs, shouldDrop)
      case _ => l
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

    // Let's do some basic type inference by using a curried dropWhile.

    // By the time dropWhileInferred(list) is run, the resulting function
    // is typed with 'A = Int'. Thus, Scala can guess that 'shouldDrop'
    // has type 'Int => Boolean'.

    // Note the syntax: you simply have to break up the arguments into
    // multiple argument lists! The internals of the function are identical.
    def dropWhileInferred [A] (l: List[A])(shouldDrop: A => Boolean) : List[A] = l match {
      case Cons(x, xs) if shouldDrop(x) => dropWhile(xs, shouldDrop)
      case _ => l
    }

    println("Dropwhile positive (w/ inferred types) for %s: %s".format(
      list3, dropWhileInferred(list3)(x => x >= 0)
    ))

  }

  {
    // Let's talk about higher order functions. Though-- riding on the train and
    // reviewing foldr and foldl might be a bad idea...

    // As a review, here's the 'sum' function written in Ch 1.
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    // Let's generalize it to foldRight
    def foldRight [A, B] (list: List[A], value: B)(f: (A, B) => B): B =
      list match {
        case Nil => value
        case Cons(x, xs) => f(x, foldRight(xs, value)(f))
      }

    def sum2(ints: List[Int]) =
      foldRight(ints, 0)((x, y) => x + y)

    val ints = List(1,2,3)
    println("The sum of %s is %d".format(ints, sum2(ints)))

    def prod2(flts: List[Double]) =
      foldRight(flts, 1.0)(_*_)

    val flts = List(1.0, 1.5, 2.0)
    println("The prod of %s is %s".format(flts, prod2(flts)))

    // Interestingly, our List(...) constructor could be written using a fold.
    var newList = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_,_))
    println("We can easily copy lists using foldRight: %s".format(newList))

    // One way to think about this is that it transforms a list:
    //    Cons(1, Cons(2, Cons(3, Nil)))
    // By replacing Cons with 'f' and Nil with 'value'
    //       f(1,    f(2,    f(3, value)))

    def length [A] (list: List[A]): Int =
      foldRight(list, 0)((_, value) => value + 1)

    val list = List(1, 2, 3, 4)
    println("The length of %s is %s".format(list, length(list)))
  }

  {
    // Quick sidebar: underscore notation for anonymous functions

    // _*_ is the same as (x, y) => x * y
    val mult: (Int, Int) => Int = _*_
    println("mult(%s, %s) is %s".format(2, 3, mult(2, 3)))

    // _.head is the same as (xs => xs.head)
  }

  {
    // Time to apply that tail recursion stuff learned in ch 2!
    @annotation.tailrec
    def foldLeft [L, V] (list: List[L], value: V)(f: (V, L) => V): V =
      list match {
        case Nil => value
        case Cons(x, xs) => foldLeft(xs, f(value, x))(f)
      }

    // In the same way that we can think about foldRight as something like
    //     Cons(1, Cons(2, Nil)) => f(1, f(2, v))
    // We can think about foldLeft as something like
    //     Cons(1, Cons(2, Nil)) => f(f(v, 1), 2)

    val list = List(1, 2, 3)
    println("Prod of %s is %s".format(
      list,
      foldLeft(list, 1)(_*_)
    ))

    println("Reverse of %s is %s".format(
      list,
      foldLeft(list, Nil: List[Int])((v, x) => Cons(x, v))
    ))

    // Writing foldLeft in terms of foldRight... Uh...
    // foldRight: Cons(1, Cons(2, Nil)) => f(1, f(2, v))
    // foldLeft:  Cons(1, Cons(2, Nil)) => f(f(v, 1), 2)

    // Cons(1, Cons(2, Nil)) => g(g(?, 1), 2)
    // We want g(g(?, 1), 2) to resolve to f(1, f(2, v))

    // First step could be simply reversing the arguments:
    //   If g is: (a, b) => h(b, a)
    //   Then g(g(?, 1), 2) resolves to h(2, h(1, ?))

    // This now looks like a job for reverse!
    // Wait but I don't have macros...
    // I can't just run the expressions through reverse(...) without evaluating them.

    // Maybe I can do the reversing first?

    def foldRight [L, V] (list: List[L], value: V)(f: (L, V) => V): V = {
      val reversed = foldLeft(list, Nil: List[L])((v, x) => Cons(x, v))
      foldLeft(reversed, value)((a, b) => f(b, a))
    }

    // Eek, let's see if this works! I think it does!
    var newList = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_,_))
    println("Let's try our new foldRight out: %s".format(newList))

    // Alright let's do append with foldRight
    def append [A] (l1: List[A], l2: List[A]): List[A] = {
      foldRight(l1, l2)((x, l) => Cons(x, l))
    }

    var list4 = List('a', 'b')
    var list5 = List('c', 'd')
    println("Appending (w/ fold) %s and %s gives %s".format(list4, list5, append(list4, list5)))

    // Now, let's flatten a list of lists.
    // First, we start with: (I'm using 'cons' for the inner lists)
    //   Cons(cons(1, Nil), Cons(cons(2, cons(3, Nil)), Nil))

    // foldRight turns this into: g(cons(1, Nil), g(cons(2, cons(3, Nil)), ?))

    // What if g itself is a fold?
    //    g(cons(1, Nil), g(cons(2, cons(3, Nil)), ?))
    //                         f(2,    f(3, Nil))
    //         f(1,            f(2,    f(3, Nil)))
    def flatten [A] (ll: List[List[A]]): List[A] = {
      foldRight(ll, Nil: List[A])(append)
    }

    val ll = List(List(1,2), List(3,4))
    println("Flattening %s gives %s".format(ll, flatten(ll)))

    // Okay, now it's time to start mapping.
    // First-- let's transform a list by +1ing each of it's values.
    def mapPlus (l: List[Int], add: Int): List[Int] = {
      foldRight(l, Nil: List[Int])((v, result) => Cons(v + add, result))
    }

    val list6 = List(1, 2, 3)
    println("mapPlus(%s, %d) is %s".format(list6, 10, mapPlus(list6, 10)))

    // Actual map!
    def map [A, B] (l: List[A])(f: A => B): List[B] = {
      foldRight(l, Nil: List[B])((v, result) => Cons(f(v), result))
    }

    println("map(%s, _+3) is %s".format(list6, map(list6)(_+3)))

    // Filter
    def filter [A] (l: List[A])(keep: A => Boolean): List[A] = {
      foldRight(l, Nil: List[A])(
        (v, result) =>
        if (keep(v)) Cons(v, result)
        else result
      )
    }

    val list7 = List(1, 2, 3, 4, 5, 6)
    val isEven = (x: Int) => x % 2 == 0
    println("Filtering  %s for even values gives %s".format(list7, filter(list7)(isEven)))

    // Weird-ass flatMap
    def flatMap [A, B] (l: List[A])(f: A => List[B]): List[B] = {
      val nested: List[List[B]] = map(l)(f)
      flatten(nested)
    }

    println("flatMap(%s)(x => List(x, x+1, x+2)) gives %s".format(
      list6,
      flatMap(list6)(x => List(x, x+1, x+2))
    ))

    // Weird-ass filter
    def filterWeird [A] (l: List[A])(f: A => Boolean) = {
      flatMap(l)(
        v =>
        if (f(v)) List(v)
        else Nil
      )
    }

    println("Filtering (weird) %s for even values gives %s".format(list7, filterWeird(list7)(isEven)))

    // Construct new list by adding elements from two lists
    def zipAdd (l0: List[Int], l1: List[Int]): List[Int] = (l0, l1) match {
      case (Nil, _) | (_, Nil) => Nil
      case (Cons(x0, xs0), Cons(x1, xs1)) => Cons(x0 + x1, zipAdd(xs0, xs1))
    }

    println("zipAdd(List(1,2,3), List(10, 100, 200)) => %s".format(zipAdd(List(1,2,3), List(10, 100, 200))))

    // Generalized zipWith
    def zipWith [A, B, C] (l0: List[A], l1: List[B])(f: (A, B) => C): List[C] = (l0, l1) match {
      case (Nil, _) | (_, Nil) => Nil
      case (Cons(x0, xs0), Cons(x1, xs1)) => {
        Cons(f(x0, x1), zipWith(xs0, xs1)(f))
      }
    }

    val l0 = List[String]("str: %s", "int: %d")
    val l1 = List[Any]("asdf", 1)
    println("zipWith(...)(_.format(_)) => %s".format(zipWith(l0, l1)(_.format(_))))
  }
}

{
  println("Scala's default list implementation: %s".format(List(1, 2, 3)))
  println("Cons is actually '::': %s".format(1 :: 2 :: 3 :: Nil))

  @annotation.tailrec
  def isStart [A] (list: List[A], start: List[A]): Boolean = (list, start) match {
    case (_, Nil) => true
    case (l :: ls, s :: ss) if l == s => isStart(ls, ss)
    case _ => false
  }

  val l0 = List(1, 2, 3, 4)
  val l1 = List(1, 2)
  val l2 = List(2, 3)
  println("%s has start %s => %s".format(l0, l1, isStart(l0, l1)))
  println("%s has start %s => %s".format(l0, l2, isStart(l0, l2)))
  println("%s has start %s => %s".format(l1, l0, isStart(l1, l0)))

  @annotation.tailrec
  def hasSubsequence[A](list: List[A], sub: List[A]): Boolean = list match {
    case Nil => false
    case _ if isStart(list, sub) => true
    case _ => hasSubsequence(list.tail, sub)
  }

  val sup = List(1,2,3,4,5,6)
  val sub = List(3,4)

  println("%s has subsequence %s => %s".format(sup, sub, hasSubsequence(sup, sub)))
  println("%s has subsequence %s => %s".format(sub, sup, hasSubsequence(sub, sup)))
}

{
  // Algebraic data-types!
  // ADTs are a data-types with multiple data constructors.
  // Each data type is the union/sum of it's data constructors.
  // Each data constructor is the product of it's arguments.

  // Pairs & tuples are also algebraic data types.
  val p0 = ("George", 1)
  val p1 = Tuple2[String, Int]("George", 1)
  println("%s == %s".format(p0, p1))

  // Let's create a tree.
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  val t0 = Branch(
    Branch(Leaf(1), Leaf(2)),
    Leaf(3)
  )

  println("Here's a simple tree: %s".format(t0))

  def numNodes [A] (node: Tree[A]): Int = node match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + numNodes(l) + numNodes(r)
  }

  println("numNodes of that simple tree: %s".format(numNodes(t0)))

  def maxValue (node: Tree[Int]): Int = node match {
    case Leaf(v) => v
    case Branch(l, r) => maxValue(l) max maxValue(r)
  }

  println("maxValue of that simple tree: %s".format(maxValue(t0)))

  val t1 = Branch(
    Branch(Leaf(1), Branch(Leaf(2), Leaf(3))),
    Branch(Leaf(4), Leaf(5))
  )

  println("Here's a more complex tree: %s".format(t1))

  def depth [A] (node: Tree[A]): Int = node match {
    case Leaf(v) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  println("depth of the complex tree: %s".format(depth(t1)))

  def mapTree [A, B] (node: Tree[A])(f: A => B): Tree[B] = node match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(mapTree(l)(f), mapTree(r)(f))
  }

  println("mapTree(%s, _+3) => %s".format(t0, mapTree(t0)(_+3)))

  {
    // Why is it okay to expose the data constructors of a datatype?
    // i.e. doesn't that break encapsulation?
    //      In OO, the implementation details of constructors should be private...

    // In FP, however, we don't usually have delicate, internal state that
    // we try to keep safe by hiding it.
  }

  // fold for trees!
  def fold [A, B] (node: Tree[A])(f: A => B)(g: (B, B) => B): B = node match {
    case Leaf(v) => f(v)
    case Branch(l, r) =>
      g(fold(r)(f)(g), fold(l)(f)(g))
  }

  def numNodesViaFold [A] (node: Tree[A]): Int =
    fold(node)((v) => 1)(1 + _ + _)

  println("numNodes of that simple tree: %s".format(numNodesViaFold(t0)))

  def maxValueViaFold (node: Tree[Int]): Int =
    fold(node)(v => v)(_ max _)

  println("maxValue of that simple tree: %s".format(maxValueViaFold(t0)))

  def depthViaFold [A] (node: Tree[A]): Int =
    fold(node)(v => 1)((l, r) => (l max r) + 1)

  println("depth of the complex tree: %s".format(depthViaFold(t1)))

}
