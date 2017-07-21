package org.sample

import org.scalatest.{FunSpec, Matchers}

class HelloWorld extends FunSpec with Matchers {
  describe("Hello World") {
    it("should say Hello") {
      def hello(name: String) = s"Hello $name"

      hello("World") shouldBe "Hello World"
    }
  }
}
