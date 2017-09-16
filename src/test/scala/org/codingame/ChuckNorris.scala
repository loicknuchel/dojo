package org.codingame

import org.scalatest.{FunSpec, Matchers}

// cf https://www.codingame.com/ide/puzzle/chuck-norris
class ChuckNorris extends FunSpec with Matchers {
  type Binary = String // binary code, ex: 110001
  type BinaryGroup = String // binary code with the same symbol repeating, ex: 111 or 00 but not 100
  type Unary = String // unary code, ex: 00 0 0 000

  def asBinary(c: Char): Binary =
    c.toInt.toBinaryString.reverse.padTo(7, '0').reverse

  def asBinary(s: String): Binary =
    s.map(asBinary).mkString

  def split(b: Binary): Seq[BinaryGroup] = {
    b.reverse.foldLeft(Seq.empty[BinaryGroup]) { (cur, c) =>
      if (cur.headOption.flatMap(_.headOption).contains(c)) Seq(cur.head + c) ++ cur.tail
      else Seq(c.toString) ++ cur
    }
  }

  def binaryGroupAsUnary(b: BinaryGroup): Unary =
    b.head match {
      case '0' => "00 " + b.map(_ => '0')
      case '1' => "0 " + b.map(_ => '0')
    }

  def binaryAsUnary(b: Binary): Unary =
    split(b).map(binaryGroupAsUnary).mkString(" ")

  def asUnary(s: String): Unary =
    binaryAsUnary(asBinary(s))

  describe("asBinay") {
    it("should format Char to Binary") {
      asBinary('C') shouldBe "1000011"
      asBinary('%') shouldBe "0100101"
    }
    it("should format String to Binary") {
      asBinary("CC") shouldBe "10000111000011"
    }
  }
  describe("split") {
    it("should split parts of binary") {
      split("110001") shouldBe Seq("11", "000", "1")
      split("110100011") shouldBe Seq("11", "0", "1", "000", "11")
    }
  }
  describe("binaryGroupAsUnary") {
    it("should format binary group as unary") {
      binaryGroupAsUnary("1") shouldBe "0 0"
      binaryGroupAsUnary("11") shouldBe "0 00"
      binaryGroupAsUnary("111") shouldBe "0 000"
      binaryGroupAsUnary("0") shouldBe "00 0"
      binaryGroupAsUnary("00") shouldBe "00 00"
      binaryGroupAsUnary("000") shouldBe "00 000"
    }
  }
  describe("binaryAsUnary") {
    it("should format binary as unary") {
      binaryAsUnary("110001") shouldBe "0 00 00 000 0 0"
    }
  }
  describe("asUnary") {
    it("should format string as unary") {
      asUnary("CC") shouldBe "0 0 00 0000 0 000 00 0000 0 00"
      //asUnary("%") shouldBe "00"
    }
  }
  describe("debug") {
    it("should debug") {
      val str = "%"
      println("str: " + str)
      val bin = asBinary(str)
      println("bin: " + bin)
      val splitted = split(bin)
      println("splitted: " + splitted)
      val splittedUnary = splitted.map(binaryGroupAsUnary)
      println("splittedUnary: " + splittedUnary)
      val unary = splittedUnary.mkString(" ")
      println("unary: " + unary)
    }
  }
}
