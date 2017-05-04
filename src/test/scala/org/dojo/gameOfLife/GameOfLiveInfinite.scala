package org.dojo.gameOfLife

import org.scalatest.{FunSpec, Matchers}

import scala.util.Random

/**
  * Rules :
  *   - 2D board with cells (Live or Dead)
  *   - a cell stay alive if it has 2 or 3 cells around, otherwise it dies
  *   - a cell is created (from nothing) if there is 3 cells around
  */
class GameOfLiveInfinite extends FunSpec with Matchers {

  sealed trait Cell

  case object Dead extends Cell

  case object Live extends Cell

  case class Format(y: Int, x: Int, height: Int, width: Int)

  case class Board(cells: Map[Int, Seq[Int]], fmt: Option[Format] = None) {
    def cell(y: Int, x: Int): Cell =
      if (cells.getOrElse(y, List()).contains(x)) Live else Dead

    def format: String = {
      def formatCell(cell: Cell): String = if (cell == Dead) " " else "X"

      def format(board: Board, yMin: Int, yMax: Int, xMin: Int, xMax: Int): String = {
        val header = "+" + (xMin to xMax).map(_ => "-").mkString + "+\n"
        (header + (yMin to yMax).map { y =>
          "|" + (xMin to xMax).map(x => formatCell(board.cell(y, x))).mkString + "|\n"
        }.mkString + header).trim
      }

      if (cells.isEmpty) {
        fmt.map { f =>
          val (yMin, yMax, xMin, xMax) = (f.y, f.y + f.height - 1, f.x, f.x + f.width - 1)
          format(this, yMin, yMax, xMin, xMax)
        }.getOrElse("++\n++")
      }
      else {
        val (yMin, yMax, xMin, xMax) = fmt.map { f => (f.y, f.y + f.height - 1, f.x, f.x + f.width - 1) }.getOrElse {
          (cells.keys.min, cells.keys.max, cells.flatMap(_._2).min, cells.flatMap(_._2).max)
        }
        format(this, yMin, yMax, xMin, xMax)
      }
    }

    def evolve: Board = {
      def countNeighbors(cell: Cell, aliveCount: Int): Int =
        if (cell == Dead) aliveCount else aliveCount - 1

      def alive(aliveCount: Int, cell: Cell): Cell = countNeighbors(cell, aliveCount) match {
        case 2 => cell
        case 3 => Live
        case _ => Dead
      }

      def countAlive(board: Board, y: Int, x: Int): Int =
        (y - 1 to y + 1).flatMap(board.cells.getOrElse(_, List())).count((x - 1 to x + 1).contains(_))

      if (cells.isEmpty) this
      else {
        val yMin = cells.keys.min - 1
        val yMax = cells.keys.max + 1
        val xMin = cells.flatMap(_._2).min - 1
        val xMax = cells.flatMap(_._2).max + 1
        Board(
          (yMin to yMax).flatMap { y =>
            (xMin to xMax).flatMap { x =>
              if (alive(countAlive(this, y, x), cell(y, x)) == Live)
                Some(y -> x)
              else
                None
            }
          },
          fmt // TODO : change fmt with new sizes ?
        )
      }
    }
  }

  object Board {
    def apply(cells: Seq[(Int, Int)], fmt: Option[Format]): Board =
      new Board(cells.groupBy(_._1).map { case (y, p) => (y, p.map(_._2)) }, fmt)

    def parse(game: String): Board = {
      val lines = game.split("\n")
      Board(lines.zipWithIndex.flatMap { case (row, y) =>
        row.zipWithIndex.flatMap { case (c, x) =>
          if (c == 'X') Some(y - 1, x - 1) else None
        }
      }, Some(Format(0, 0, lines.length - 2, lines.headOption.map(_.length - 2).getOrElse(0))))
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
