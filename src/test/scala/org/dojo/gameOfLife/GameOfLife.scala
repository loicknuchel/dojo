package org.dojo.gameOfLife

import org.scalatest.{FunSpec, Matchers}

import scala.util.Random

/**
  * Rules :
  *   - 2D board with cells (Live or Dead)
  *   - a cell stay alive if it has 2 or 3 cells around, otherwise it dies
  *   - a cell is created (from nothing) if there is 3 cells around
  */
// May 3rd 2017 at Arolla coding dojo
class GameOfLife extends FunSpec with Matchers {

  sealed trait Cell

  case object Dead extends Cell

  case object Live extends Cell

  type Neighborhood = Seq[Seq[Cell]]

  case class Board(cells: Seq[Seq[Cell]]) {
    def format: String = {
      def formatCell(cell: Cell): String = if (cell == Dead) " " else "X"
      def formatRow(row: Seq[Cell]): String = "|" + row.map(formatCell).mkString + "|\n"

      val header = "+" + cells.head.map(_ => "-").mkString + "+\n"
      (header + cells.map(formatRow).mkString + header).trim
    }

    def evolve: Board = {
      def countNeighbors(cell: Cell, aliveCount: Int): Int =
        if (cell == Dead) aliveCount else aliveCount - 1

      def alive(aliveCount: Int, cell: Cell): Cell = countNeighbors(cell, aliveCount) match {
        case 2 => cell
        case 3 => Live
        case _ => Dead
      }

      def countAlive(neighborhood: Neighborhood): Int =
        neighborhood.flatMap(_.filter(_ == Live)).length

      def neighborhood(board: Board, y: Int, x: Int): Neighborhood =
        board.cells.slice(y - 1, y + 2).map(_.slice(x - 1, x + 2))

      Board(cells.zipWithIndex.map { case (row, y) =>
        row.zipWithIndex.map { case (cell, x) =>
          alive(countAlive(neighborhood(this, y, x)), cell)
        }
      })
    }
  }

  object Board {
    def parse(game: String): Board = {
      Board(game.split("\n").drop(1).dropRight(1).map { row =>
        row.drop(1).dropRight(1).map { c =>
          if(c == 'X') Live else Dead
        }
      })
    }
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
