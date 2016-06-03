package learning.ch10

trait Monoid[A] {
  // op must satisfy associativity: op(op(x,y),z) == op(x,op(y,z))
  def op(a1: A, a2: A): A
  // zero must satisfy identity: op(zero,x) == op(x,zero) == x
  def zero: A
}
