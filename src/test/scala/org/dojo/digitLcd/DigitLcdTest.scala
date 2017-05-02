package org.dojo.digitLcd

import org.scalatest.{FunSpec, Matchers}

// May 2nd 2017 at Criteo coding dojo
class DigitLcdTest extends FunSpec with Matchers {
  type LcdString = String // a five line string representing the LCD display

  def formatNumber(in: Int): String = {
    in.toString
  }

  def charToLcd(in: Char): LcdString = in match {
    case '0' =>
      " -- \n" +
      "|  |\n" +
      "|  |\n" +
      "|  |\n" +
      " -- \n"
    case '1' =>
      "   |\n" +
      "   |\n" +
      "   |\n" +
      "   |\n" +
      "   |\n"
    case '2' =>
      " -- \n" +
      "   |\n" +
      " -- \n" +
      "|   \n" +
      " -- \n"
    case '3' =>
      " -- \n" +
      "   |\n" +
      " -- \n" +
      "   |\n" +
      " -- \n"
    case '4' =>
      "|  |\n" +
      "|  |\n" +
      " -- \n" +
      "   |\n" +
      "   |\n"
    case '5' =>
      " -- \n" +
      "|   \n" +
      " -- \n" +
      "   |\n" +
      " -- \n"
    case '6' =>
      " -- \n" +
      "|   \n" +
      " -- \n" +
      "|  |\n" +
      " -- \n"
    case '7' =>
      " -- \n" +
      "   |\n" +
      "   |\n" +
      "   |\n" +
      "    \n"
    case '8' =>
      " -- \n" +
      "|  |\n" +
      " -- \n" +
      "|  |\n" +
      " -- \n"
    case '9' =>
      " -- \n" +
      "|  |\n" +
      " -- \n" +
      "   |\n" +
      " -- \n"
    case ' ' =>
      "  \n" +
      "  \n" +
      "  \n" +
      "  \n" +
      "  \n"
    case _ => ???
  }

  def joinLcd(digits: Seq[LcdString], separator: String = " "): LcdString = {
    val digitLines = digits.map(_.split("\n"))
    val nbLines = digitLines.headOption.map(_.length).getOrElse(0)
    (0 until nbLines).map { lineIndex =>
      digitLines.map(digitLine => digitLine(lineIndex)).mkString(separator) + "\n"
    }.mkString
  }

  def printLcd(in: Int): LcdString = printLcd(formatNumber(in))
  def printLcd(in: String): LcdString = joinLcd(in.map(charToLcd))

  describe("formatNumber") {
    it("should format a number") {
      formatNumber(1) shouldBe "1"
      formatNumber(4) shouldBe "4"
      formatNumber(12) shouldBe "12"
      formatNumber(1024) shouldBe "1024"
    }
  }

  describe("charToLcd") {
    it("should print 1") {
      charToLcd('1') shouldBe
        "   |\n" +
        "   |\n" +
        "   |\n" +
        "   |\n" +
        "   |\n"
    }
    it("should print 2") {
      charToLcd('2') shouldBe
        " -- \n" +
        "   |\n" +
        " -- \n" +
        "|   \n" +
        " -- \n"
    }
    it("should print 3") {
      charToLcd('3') shouldBe
        " -- \n" +
        "   |\n" +
        " -- \n" +
        "   |\n" +
        " -- \n"
    }
  }

  describe("joinLcd") {
    it("should join 2 multiline strings") {
      joinLcd(List(
        " -- \n" +
        "   |\n" +
        " -- \n" +
        "   |\n" +
        " -- \n",
        " -- \n" +
        "   |\n" +
        " -- \n" +
        "   |\n" +
        " -- \n"
      )) shouldBe
        " --   -- \n" +
        "   |    |\n" +
        " --   -- \n" +
        "   |    |\n" +
        " --   -- \n"
    }
  }

  describe("printLCD") {
    it("should print 123") {
      printLcd(123) shouldBe
        "   |  --   -- \n" +
        "   |    |    |\n" +
        "   |  --   -- \n" +
        "   | |       |\n" +
        "   |  --   -- \n"
    }
  }
}
