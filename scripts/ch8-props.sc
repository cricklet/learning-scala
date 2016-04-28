// We've introduced the idea that an API should be an algebra.
// i.e. a collection of data types & functions over those data
// types

// There are laws/properties that describe these functions.
// We can test these properties using property based testing!
def sum (l: List[Int]): Int =
  if (l.length == 0) 0
  else l.head + sum(l.tail)
sum(List(1,2,3))

// Here's some properties that should hold for sum:
//  - It should give the same answer no matter the order of
//    the elements in the list.
//  - If all the elements are the same, then the sum should
//    be length * value.
//  - The combined sum of two different lists should be the
//    same as the sum of both lists combined into one list.

// Here's some properties that should hold for max:
//  - It should give the same answer no matter the order of
//    the elements in the list.
//  - If all the elements are the same, then the sum should
//    be the value.
//  - If the max of one list is greater than the max of the
//    other list, then the max of both lists combined should
//    be the max of the first list.

// Property based testing is partly cool because the test
// framework can do stuff like:
//   - Find the smallest example s.t. the test fails.
//   - Exhaustively test all possibilities.
import learning.ch8.PropertyTesting.{Gen}
import learning.ch6.{SimpleRNG}
val rng = SimpleRNG(12345)
val gen = Gen.choose(0, 100)
gen.sample.run(rng)