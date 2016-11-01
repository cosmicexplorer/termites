package termites

import org.scalatest.FunSpec

class MainTest extends FunSpec {
  describe("hello") {
    it("should work") {
      assert(true)
    }

    it("should allow basic construction") {
      assert(true)

      val st = new State
      val end = new State
      val trans = Transition[Char](st, end)
      val enpda = new ENPDA(st, end, Set(trans), Set(new NonterminalId))

      val enpda2 = new ENPDA(st, end, Set(trans), Set(new NonterminalId))

      lazy val a: ENPDA[Char] = enpda2 -> b | enpda
      lazy val b: ENPDA[Char] = a -> enpda
      lazy val c: ENPDA[Char] = enpda -> c | enpda2
    }
  }
}
