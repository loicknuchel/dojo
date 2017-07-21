package org.dojo.reversePolishNotation

import org.scalatest.{FunSpec, Matchers}

import scala.annotation.tailrec

// https://en.wikipedia.org/wiki/Reverse_Polish_notation
class ReversePolishNotationSpec extends FunSpec with Matchers {
  def eval(str: String): Int = {
    @tailrec
    def inner(args: List[String], stack: List[Int]): Int = {
      args.headOption match {
        case Some("+") => inner(args.tail, (stack.tail.head + stack.head) :: stack.tail.tail)
        case Some("*") => inner(args.tail, (stack.tail.head * stack.head) :: stack.tail.tail)
        case Some("-") => inner(args.tail, (stack.tail.head - stack.head) :: stack.tail.tail)
        case Some("/") => inner(args.tail, (stack.tail.head / stack.head) :: stack.tail.tail)
        case Some(arg) => inner(args.tail, arg.toInt :: stack)
        case None => stack.head
      }
    }
    inner(str.split(" ").toList, List())
  }

  describe("RPN") {
    it("should add") {
      eval("3 4 +") shouldBe 7
    }
    it("should multiply") {
      eval("3 4 *") shouldBe 12
    }
    it("should substract") {
      eval("3 4 -") shouldBe -1
    }
    it("should divide") {
      eval("4 2 /") shouldBe 2
    }
    it("should do several operations without parenthesis"){
      eval("3 4 - 5 +") shouldBe 4
    }
    it("should do several operations with priority"){
      eval("3 4 5 * +") shouldBe 23
    }
  }
}
