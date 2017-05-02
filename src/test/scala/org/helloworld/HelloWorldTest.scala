package org.helloworld

import org.scalatest.{FlatSpec, Matchers}

class HelloWorldTest extends FlatSpec with Matchers {
  "Hello world" should "print Hello World" in {
    "Hello world" shouldBe "Hello world"
  }
}
