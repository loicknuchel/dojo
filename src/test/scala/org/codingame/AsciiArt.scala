package org.codingame

import org.scalatest.{FunSpec, Matchers}

// cf https://www.codingame.com/ide/puzzle/ascii-art
class AsciiArt extends FunSpec with Matchers {
  type FormattedText = String
  type AsciiText = String
  type AsciiLetter = String

  case class Input(width: Int, alphabet: String, alphabetAscii: AsciiText, text: String)

  def read(): Input = {
    val width = readInt
    val height = readInt
    val text = readLine
    val alphabetAscii = (0 until height).map(_ => readLine + "\n").mkString
    Input(width, "abcdefghijklmnopqrstuvwxyz?", alphabetAscii, text)
  }

  def format(in: String): FormattedText = {
    in.toLowerCase.replaceAll("[^a-z]", "?")
  }

  def swap[T](rows: Seq[Seq[T]]): Seq[Seq[T]] = {
    rows.head.indices.map { lineIndex =>
      rows.map(line => line(lineIndex))
    }
  }

  def parse(width: Int, text: String, asciiText: AsciiText): Map[Char, AsciiLetter] = {
    val asciiLetters = swap(asciiText.split("\n").toSeq.map { line =>
      line.grouped(width).toSeq
    }).map(_.map(_ + "\n").mkString)
    text.zip(asciiLetters).toMap
  }

  def writeAscii(text: FormattedText, alphabet: Map[Char, AsciiLetter]): AsciiText = {
    swap(text.flatMap { letter =>
      alphabet.get(letter).map(_.split("\n").toSeq)
    }).map(_.mkString + "\n").mkString
  }

  def program(): AsciiText = {
    val Input(width, alphabet, alphabetAscii, text) = read()
    val alphabetMap = parse(width, alphabet, alphabetAscii)
    writeAscii(format(text), alphabetMap)
  }

  val alphabet = "abcdefghijklmnopqrstuvwxyz?"
  val alphabetAscii =
    " #  ##   ## ##  ### ###  ## # # ###  ## # # #   # # ###  #  ##   #  ##   ## ### # # # # # # # # # # ### ### \n" +
      "# # # # #   # # #   #   #   # #  #    # # # #   ### # # # # # # # # # # #    #  # # # # # # # # # #   #   # \n" +
      "### ##  #   # # ##  ##  # # ###  #    # ##  #   ### # # # # ##  # # ##   #   #  # # # # ###  #   #   #   ## \n" +
      "# # # # #   # # #   #   # # # #  #  # # # # #   # # # # # # #    ## # #   #  #  # # # # ### # #  #  #       \n" +
      "# # ##   ## ##  ### #    ## # # ###  #  # # ### # # # #  #  #     # # # ##   #  ###  #  # # # #  #  ###  #  \n"
  val alphabetMap = Map(
    'a' ->
      (" #  \n" +
        "# # \n" +
        "### \n" +
        "# # \n" +
        "# # \n"),
    'b' ->
      ("##  \n" +
        "# # \n" +
        "##  \n" +
        "# # \n" +
        "##  \n"),
    'c' ->
      (" ## \n" +
        "#   \n" +
        "#   \n" +
        "#   \n" +
        " ## \n"),
    'd' ->
      ("##  \n" +
        "# # \n" +
        "# # \n" +
        "# # \n" +
        "##  \n"),
    'e' ->
      ("### \n" +
        "#   \n" +
        "##  \n" +
        "#   \n" +
        "### \n"),
    'f' ->
      ("### \n" +
        "#   \n" +
        "##  \n" +
        "#   \n" +
        "#   \n"),
    'g' ->
      (" ## \n" +
        "#   \n" +
        "# # \n" +
        "# # \n" +
        " ## \n"),
    'h' ->
      ("# # \n" +
        "# # \n" +
        "### \n" +
        "# # \n" +
        "# # \n"),
    'i' ->
      ("### \n" +
        " #  \n" +
        " #  \n" +
        " #  \n" +
        "### \n"),
    'j' ->
      (" ## \n" +
        "  # \n" +
        "  # \n" +
        "# # \n" +
        " #  \n"),
    'k' ->
      ("# # \n" +
        "# # \n" +
        "##  \n" +
        "# # \n" +
        "# # \n"),
    'l' ->
      ("#   \n" +
        "#   \n" +
        "#   \n" +
        "#   \n" +
        "### \n"),
    'm' ->
      ("# # \n" +
        "### \n" +
        "### \n" +
        "# # \n" +
        "# # \n"),
    'n' ->
      ("### \n" +
        "# # \n" +
        "# # \n" +
        "# # \n" +
        "# # \n"),
    'o' ->
      (" #  \n" +
        "# # \n" +
        "# # \n" +
        "# # \n" +
        " #  \n"),
    'p' ->
      ("##  \n" +
        "# # \n" +
        "##  \n" +
        "#   \n" +
        "#   \n"),
    'q' ->
      (" #  \n" +
        "# # \n" +
        "# # \n" +
        " ## \n" +
        "  # \n"),
    'r' ->
      ("##  \n" +
        "# # \n" +
        "##  \n" +
        "# # \n" +
        "# # \n"),
    's' ->
      (" ## \n" +
        "#   \n" +
        " #  \n" +
        "  # \n" +
        "##  \n"),
    't' ->
      ("### \n" +
        " #  \n" +
        " #  \n" +
        " #  \n" +
        " #  \n"),
    'u' ->
      ("# # \n" +
        "# # \n" +
        "# # \n" +
        "# # \n" +
        "### \n"),
    'v' ->
      ("# # \n" +
        "# # \n" +
        "# # \n" +
        "# # \n" +
        " #  \n"),
    'w' ->
      ("# # \n" +
        "# # \n" +
        "### \n" +
        "### \n" +
        "# # \n"),
    'x' ->
      ("# # \n" +
        "# # \n" +
        " #  \n" +
        "# # \n" +
        "# # \n"),
    'y' ->
      ("# # \n" +
        "# # \n" +
        " #  \n" +
        " #  \n" +
        " #  \n"),
    'z' ->
      ("### \n" +
        "  # \n" +
        " #  \n" +
        "#   \n" +
        "### \n"),
    '?' ->
      ("### \n" +
        "  # \n" +
        " ## \n" +
        "    \n" +
        " #  \n")
  )
  describe("format") {
    it("should clean all characters except letters") {
      format("Loïc") shouldBe "lo?c"
    }
  }
  describe("swap") {
    it("should interchange two collections") {
      swap(Seq(Seq(1, 1, 1), Seq(2, 2, 2))) shouldBe Seq(Seq(1, 2), Seq(1, 2), Seq(1, 2))
    }
    it("should do nothing when called twice") {
      Seq(
        Seq(Seq(1, 1, 1), Seq(2, 2, 2))
      ).foreach { coll =>
        swap(swap(coll)) shouldBe coll
      }
    }
  }
  describe("parse") {
    it("should extract letters") {
      val result = parse(4, alphabet, alphabetAscii)
      result('a') shouldBe
        " #  \n" +
          "# # \n" +
          "### \n" +
          "# # \n" +
          "# # \n"
      result shouldBe alphabetMap
    }
  }
  describe("write") {
    it("should write ascii text") {
      writeAscii("abc", alphabetMap) shouldBe
        " #  ##   ## \n" +
          "# # # # #   \n" +
          "### ##  #   \n" +
          "# # # # #   \n" +
          "# # ##   ## \n"
    }
  }
}
