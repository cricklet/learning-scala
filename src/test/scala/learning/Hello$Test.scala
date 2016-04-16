package learning

import org.scalatest.FunSuite

class Hello$Test extends FunSuite {
  test("hello returns hello!") {
    assert(Hello.hello() == "Hello!")
  }
}
