package org.dojo.gameOfLife

import org.scalatest.{FunSpec, Matchers}

import scala.util.Random

class GameOfLiveInfiniteSpec extends FunSpec with Matchers {

  sealed trait Cell

  case object Dead extends Cell

  case object Live extends Cell

  case class Board(cells: Map[Int, Seq[Int]]) {
    def cell(y: Int, x: Int): Cell =
      if (cells.getOrElse(y, List()).contains(x)) Live else Dead
  }
  object Board {
    def apply(cells: Seq[(Int, Int)]): Board = new Board(cells.groupBy(_._1).map { case (y, p) => (y, p.map(_._2)) })
    def apply(): Board = new Board(Map())
  }

  def evolve(board: Board): Board = {
    def countNeighbors(cell: Cell, aliveCount: Int): Int =
      if (cell == Dead) aliveCount else aliveCount - 1

    def alive(aliveCount: Int, cell: Cell): Cell = countNeighbors(cell, aliveCount) match {
      case 2 => cell
      case 3 => Live
      case _ => Dead
    }

    def countAlive(board: Board, y: Int, x: Int): Int =
      (y - 1 to y + 1).flatMap(board.cells.getOrElse(_, List())).count((x - 1 to x + 1).contains(_))

    if (board.cells.isEmpty) board
    else {
      val yMin = board.cells.keys.min - 1
      val yMax = board.cells.keys.max + 1
      val xMin = board.cells.flatMap(_._2).min - 1
      val xMax = board.cells.flatMap(_._2).max + 1
      Board(
        (yMin to yMax).flatMap { y =>
          (xMin to xMax).flatMap { x =>
            if (alive(countAlive(board, y, x), board.cell(y, x)) == Live)
              Some(y -> x)
            else
              None
          }
        }
      )
    }
  }

  describe("Center cell") {
    describe("when Live") {
      it("should die when alone") {
        evolve(Board(Map(1 -> List(1)))).cell(1, 1) shouldBe Dead
      }
      it("should die when surrounded by 1 cell") {
        evolve(Board(Map(1 -> List(0, 1)))).cell(1, 1) shouldBe Dead
      }
      it("should survive when surrounded by 2 cells") {
        evolve(Board(Map(1 -> List(0, 1, 2)))).cell(1, 1) shouldBe Live
      }
      it("should survive when surrounded by 3 cells") {
        evolve(Board(Map(
          0 -> List(0),
          1 -> List(0, 1, 2)))).cell(1, 1) shouldBe Live
      }
      it("should die when surrounded by 4 cells") {
        evolve(Board(Map(
          0 -> List(0, 1),
          1 -> List(0, 1, 2)))).cell(1, 1) shouldBe Dead
      }
    }
    describe("when Dead") {
      it("should stay Dead when alone") {
        evolve(Board()).cell(1, 1) shouldBe Dead
      }
      it("should stay Dead when surrounded by 1 cell") {
        evolve(Board(Map(1 -> List(0)))).cell(1, 1) shouldBe Dead
      }
      it("should stay Dead when surrounded by 2 cells") {
        evolve(Board(Map(1 -> List(0, 2)))).cell(1, 1) shouldBe Dead
      }
      it("should born when surrounded by 3 cells") {
        evolve(Board(Map(
          0 -> List(0),
          1 -> List(0, 2)))).cell(1, 1) shouldBe Live
      }
      it("should stay Dead when surrounded by 4 cells") {
        evolve(Board(Map(
          0 -> List(0, 1),
          1 -> List(0, 2)))).cell(1, 1) shouldBe Dead
      }
    }
  }
  describe("Board") {
    it("should evolve") {
      val start = Board(Map(
        0 -> List(0, 4),
        1 -> List(1, 2),
        2 -> List(2, 3)
      ))
      val step1 = Board(Map(
        0 -> List(1),
        1 -> List(1, 2),
        2 -> List(1, 2, 3)
      ))
      val end = Board(Map(
        0 -> List(1, 2),
        1 -> List(0, 3),
        2 -> List(1, 3),
        3 -> List(2)
      ))
      evolve(start) shouldBe step1
      evolve(step1) shouldBe end
      evolve(end) shouldBe end
    }

    it("should play") {
      def generate(width: Int, heigh: Int, aliveRatio: Double): Board = {
        Board(
          (0 until heigh).flatMap { y =>
            (0 until width).flatMap { x =>
              if (Random.nextDouble() < aliveRatio) Some(y -> x) else None
            }
          }
        )
      }

      def format(board: Board): String = {
        def formatCell(cell: Cell): String = if (cell == Dead) " " else "X"

        if(board.cells.isEmpty)
          """+-------+
            || Empty |
            |+-------+
          """.stripMargin
        else {
          val yMin = board.cells.keys.min
          val yMax = board.cells.keys.max
          val xMin = board.cells.flatMap(_._2).min
          val xMax = board.cells.flatMap(_._2).max
          val header = "+" + (xMin to xMax).map(_ => "-").mkString + "+\n"
          header + (yMin to yMax).map { y =>
            "|" + (xMin to xMax).map(x => formatCell(board.cell(y, x))).mkString + "|\n"
          }.mkString + header
        }
      }

      val maxGeneration = 200
      var history = List(generate(50, 10, 0.2))
      while (history.size == history.distinct.size && history.size < maxGeneration) {
        println(history.size + ":")
        println(format(history.head))
        history = evolve(history.head) :: history
        Thread.sleep(300)
      }
    }
  }
}
