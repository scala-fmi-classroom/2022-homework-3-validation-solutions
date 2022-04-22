package homework3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ValidatedTest extends AnyFlatSpec with Matchers:
  "zip" should "combine valid instances" in {
    Valid(1).zip(Valid("a")) shouldBe Valid((1, "a"))
  }

  it should "combine errors from invalid instances" in {
    Invalid(1).zip(Invalid(Chain(2, 3))) shouldBe Invalid(Chain(1, 2, 3))
  }
