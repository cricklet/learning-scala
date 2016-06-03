import learning.ch10.{Monoid}

val intAddition: Monoid[Int] = new Monoid[Int] {
  def op(x: Int, y: Int): Int = x + y
  def zero: Int = 0
}

intAddition.op(4, 3)

def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
  def op(o1: Option[A], o2: Option[A]) = o1 orElse o2
  def zero = None
}

def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  def op(f: A => A, g: A => A): A => A =
    f compose g
  def zero: A => A =
    (a: A) => a
}

def stringMonoid: Monoid[String] = new Monoid[String] {
  def op(x: String, y: String): String = x + y
  def zero: String = ""
}

stringMonoid.op("hello", "world")

val l = List("hello", "world", "this", "is", "kenrick")

// Note that both of these give the same result!
// This is because Monoids satisfy associativity & identity!
l.foldLeft(stringMonoid.zero)(stringMonoid.op)
l.foldRight(stringMonoid.zero)(stringMonoid.op)

// What exactly is a monoid, then? It's simply a type A and an implementation
// of Monoid[A] that satisfies the necessary laws.

def concatenate[A](as: List[A], m: Monoid[A]): A =
  as.foldLeft(m.zero)(m.op)

concatenate(l, stringMonoid)

def foldMapWithConcat[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
  concatenate(as.map(f), m)

def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
  as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

foldMapWithConcat(l, stringMonoid)(a => "." + a)
foldMap(l, stringMonoid)(a => "." + a)

// B => B is monoidal under the operations defined by endoMonoid[B]
// f.curried has type (A => (B => B))

def foldRightWithFoldMap[A,B](as: List[A])(b0: B)(f: (A, B) => B): B =
  foldMap(as, endoMonoid[B])(f.curried)(b0)

// We've found that we can fold left or right when dealing with monoidal operations.
// This means that we can do balanced folds.
//   op(a, op(b, op(c, d)))
//    == op(op(op(a, b), c), d)
//    == op((op(a, b), op(c, d)) # this is a balanced fold

def foldMapBalanced[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
  if (as.length > 1) {
    val (asL, asR) = as.splitAt(as.length / 2)
    m.op(
      foldMapBalanced(asL, m)(f),
      foldMapBalanced(asR, m)(f)
    )
  } else if (as.length == 1) {
    f(as(0))
  } else {
    m.zero
  }
}

foldMapBalanced(l.toArray[String], stringMonoid)(a => "." + a)

