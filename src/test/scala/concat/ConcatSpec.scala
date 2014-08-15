package concat

import shapeless._
import org.specs2.mutable._

class ConcatSpec extends Specification {

  "Stack manipulation operations" should {

    "produce expected stack" in {
      val base = Concat()
      base.push(1).stack must beEqualTo(1 :: HNil)
      base.push(1).dup.stack must beEqualTo(1 :: 1 :: HNil)
      base.push(1).drop.stack must beEqualTo(HNil)
      base.push(1).push(2).swap.stack must beEqualTo(1 :: 2 :: HNil)
    }

  }

  "Numeric operations" should {

    "produce expected stack" in {
      val base = Concat().push(2).push(1)
      base.*.stack must beEqualTo((2 * 1) :: HNil)
      base.+.stack must beEqualTo((2 + 1) :: HNil)
    }

  }

  "Boolean operations" should {

    "execute conditionals correctly" in {
      val base = Concat().push(1).push(2)
      base.push(true).ifThen.stack must beEqualTo(2 :: HNil)
      base.push(false).ifThen.stack must beEqualTo(1 :: HNil)
    }

  }

}
