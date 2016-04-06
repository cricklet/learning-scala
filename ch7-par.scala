
object Par {
  // Promotes a constant value to a parallel computation
  def unit [A] (a: A): Par[A]

  // Combine the result of two parallel computations with 'f'
  def map2 [A, B, C] (a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]

  // Flag a computation for concurrent evaluation
  def fork [A] (a: Par[A]): Par[A]

  // Promote a thunk value to a parallel compuation
  // & mark it for concurrent evaluation
  def lazyUnit [A] (a: => A): Par[A] =

  // Extract a value from a Par by actually performing the computation
  def run [A] (a: Par[A]): A
}
