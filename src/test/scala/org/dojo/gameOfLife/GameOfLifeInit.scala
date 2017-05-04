package org.dojo.gameOfLife

import org.scalatest.{FunSpec, Matchers}

import scala.util.Random

/**
  * Rules :
  *   - 2D board with cells (Live or Dead)
  *   - a living cell stay alive if it has 2 or 3 cells around, otherwise it dies
  *   - a dead cell returns to life if there is exactly 3 cells around
  */
class GameOfLifeInit extends FunSpec with Matchers {

  case class Board() {
    def format: String = ???

    def evolve: Board = ???
  }

  object Board {
    def parse(game: String): Board = ???
  }

  describe("parse and format") {
    it("an empty game") {
      val game =
        """+---+
          ||   |
          ||   |
          ||   |
          |+---+""".stripMargin
      Board.parse(game).format shouldBe game
    }
    it("a game with one cell") {
      val game =
        """+---+
          ||   |
          || X |
          ||   |
          |+---+""".stripMargin
      Board.parse(game).format shouldBe game
    }
    it("a game with some cells") {
      val game =
        """+---+
          || X |
          ||XX |
          ||  X|
          |+---+""".stripMargin
      Board.parse(game).format shouldBe game
    }
    it("a small game") {
      val game =
        """+--+
          ||X |
          || X|
          |+--+""".stripMargin
      Board.parse(game).format shouldBe game
    }
    it("a big game") {
      val game =
        """+-------+
          ||     X |
          || X     |
          || X  X  |
          || X    X|
          |+-------+""".stripMargin
      Board.parse(game).format shouldBe game
    }
  }

  describe("Center cell") {
    describe("when Live") {
      it("should die when alone") {
        Board.parse(
          """+---+
            ||   |
            || X |
            ||   |
            |+---+""".stripMargin).evolve.format.split("\n")(2)(2) shouldBe ' '
      }
      it("should die when surrounded by 1 cell") {
        Board.parse(
          """+---+
            ||   |
            ||XX |
            ||   |
            |+---+""".stripMargin).evolve.format.split("\n")(2)(2) shouldBe ' '
      }
      it("should survive when surrounded by 2 cells") {
        Board.parse(
          """+---+
            ||   |
            ||XXX|
            ||   |
            |+---+""".stripMargin).evolve.format.split("\n")(2)(2) shouldBe 'X'
      }
      it("should survive when surrounded by 3 cells") {
        Board.parse(
          """+---+
            || X |
            ||XXX|
            ||   |
            |+---+""".stripMargin).evolve.format.split("\n")(2)(2) shouldBe 'X'
      }
      it("should die when surrounded by 4 cells") {
        Board.parse(
          """+---+
            || X |
            ||XXX|
            || X |
            |+---+""".stripMargin).evolve.format.split("\n")(2)(2) shouldBe ' '
      }
    }
    describe("when Dead") {
      it("should stay Dead when alone") {
        Board.parse(
          """+---+
            ||   |
            ||   |
            ||   |
            |+---+""".stripMargin).evolve.format.split("\n")(2)(2) shouldBe ' '
      }
      it("should stay Dead when surrounded by 1 cell") {
        Board.parse(
          """+---+
            ||   |
            ||X  |
            ||   |
            |+---+""".stripMargin).evolve.format.split("\n")(2)(2) shouldBe ' '
      }
      it("should stay Dead when surrounded by 2 cells") {
        Board.parse(
          """+---+
            ||   |
            ||X X|
            ||   |
            |+---+""".stripMargin).evolve.format.split("\n")(2)(2) shouldBe ' '
      }
      it("should born when surrounded by 3 cells") {
        Board.parse(
          """+---+
            || X |
            ||X X|
            ||   |
            |+---+""".stripMargin).evolve.format.split("\n")(2)(2) shouldBe 'X'
      }
      it("should stay Dead when surrounded by 4 cells") {
        Board.parse(
          """+---+
            || X |
            ||X X|
            || X |
            |+---+""".stripMargin).evolve.format.split("\n")(2)(2) shouldBe ' '
      }
    }
  }

  describe("Border cell") {
    describe("when Live") {
      it("should survive when surrounded by 2 cells") {
        Board.parse(
          """+---+
            ||XXX|
            ||   |
            ||   |
            |+---+""".stripMargin).evolve.format.split("\n")(1)(2) shouldBe 'X'
      }
    }
  }

  describe("Board") {
    it("should evolve") {
      val start =
        """+-----+
          ||X   X|
          || XX  |
          ||  XX |
          ||     |
          |+-----+""".stripMargin
      val step1 =
        """+-----+
          || X   |
          || XX  |
          || XXX |
          ||     |
          |+-----+""".stripMargin
      val enddd =
        """+-----+
          || XX  |
          ||X  X |
          || X X |
          ||  X  |
          |+-----+""".stripMargin
      Board.parse(start).evolve.format shouldBe step1
      Board.parse(step1).evolve.format shouldBe enddd
      Board.parse(enddd).evolve.format shouldBe enddd
    }

    ignore("should play") {
      def generate(width: Int, heigh: Int, aliveRatio: Double): String = {
        val footer = "+" + (0 until width).map(_ => '-').mkString + "+"
        val header = footer + "\n"
        val board = (0 until heigh).map { _ =>
          val row = (0 until width).map { _ =>
            if (Random.nextDouble() < aliveRatio) 'X' else ' '
          }.mkString
          "|" + row + "|\n"
        }.mkString
        header + board + footer
      }

      val maxGeneration = 200
      var history = List(generate(50, 10, 0.2))
      while (history.size == history.distinct.size && history.size < maxGeneration) {
        println(history.size + ":")
        println(history.head + "\n")
        history = Board.parse(history.head).evolve.format :: history
        Thread.sleep(300)
      }
    }
  }
}
