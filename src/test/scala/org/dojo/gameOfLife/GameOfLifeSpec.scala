package org.dojo.gameOfLife

import org.scalatest.{FunSpec, Matchers}

import scala.util.Random

/**
  * Rules :
  *   - 2D board with cells
  *   - a cell stay alive if it has 2 or 3 cells around, otherwise it dies
  *   - a cell is created (from nothing) if there is 3 cells around
  */
// May 3rd 2017 at Arolla coding dojo
class GameOfLifeSpec extends FunSpec with Matchers {

  sealed trait Cell
  case object Dead extends Cell
  case object Live extends Cell

  type Board = List[List[Cell]]
  type Neighborhood = List[List[Cell]]

  def evolve(board: Board): Board = {
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
      board.slice(y - 1, y + 2).map(_.slice(x - 1, x + 2))

    board.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        alive(countAlive(neighborhood(board, y, x)), cell)
      }
    }
  }

  describe("Center cell") {
    describe("when Live") {
      it("should die when alone") {
        evolve(List(
          List(Dead, Dead, Dead),
          List(Dead, Live, Dead),
          List(Dead, Dead, Dead)))(1)(1) shouldBe Dead
      }
      it("should die when surrounded by 1 cell") {
        evolve(List(
          List(Dead, Dead, Dead),
          List(Live, Live, Dead),
          List(Dead, Dead, Dead)))(1)(1) shouldBe Dead
      }
      it("should survive when surrounded by 2 cells") {
        evolve(List(
          List(Dead, Dead, Dead),
          List(Live, Live, Live),
          List(Dead, Dead, Dead)))(1)(1) shouldBe Live
      }
      it("should survive when surrounded by 3 cells") {
        evolve(List(
          List(Dead, Live, Dead),
          List(Live, Live, Live),
          List(Dead, Dead, Dead)))(1)(1) shouldBe Live
      }
      it("should die when surrounded by 4 cells") {
        evolve(List(
          List(Dead, Live, Dead),
          List(Live, Live, Live),
          List(Dead, Live, Dead)))(1)(1) shouldBe Dead
      }
    }
    describe("when Dead") {
      it("should stay Dead when alone") {
        evolve(List(
          List(Dead, Dead, Dead),
          List(Dead, Dead, Dead),
          List(Dead, Dead, Dead)))(1)(1) shouldBe Dead
      }
      it("should stay Dead when surrounded by 1 cell") {
        evolve(List(
          List(Dead, Dead, Dead),
          List(Live, Dead, Dead),
          List(Dead, Dead, Dead)))(1)(1) shouldBe Dead
      }
      it("should stay Dead when surrounded by 2 cells") {
        evolve(List(
          List(Dead, Dead, Dead),
          List(Live, Dead, Live),
          List(Dead, Dead, Dead)))(1)(1) shouldBe Dead
      }
      it("should born when surrounded by 3 cells") {
        evolve(List(
          List(Dead, Dead, Dead),
          List(Live, Dead, Live),
          List(Dead, Live, Dead)))(1)(1) shouldBe Live
      }
      it("should stay Dead when surrounded by 4 cells") {
        evolve(List(
          List(Dead, Live, Dead),
          List(Live, Dead, Live),
          List(Dead, Live, Dead)))(1)(1) shouldBe Dead
      }
    }
  }
  describe("Border cell") {
    describe("when Live") {
      it("should survive when surrounded by 2 cells") {
        evolve(List(
          List(Live, Live, Live),
          List(Dead, Dead, Dead),
          List(Dead, Dead, Dead)))(0)(1) shouldBe Live
      }
    }
  }
  describe("Board") {
    it("should evolve") {
      val start = List(
        List(Live, Dead, Dead, Dead, Live),
        List(Dead, Live, Live, Dead, Dead),
        List(Dead, Dead, Live, Live, Dead),
        List(Dead, Dead, Dead, Dead, Dead))
      val step1 = List(
        List(Dead, Live, Dead, Dead, Dead),
        List(Dead, Live, Live, Dead, Dead),
        List(Dead, Live, Live, Live, Dead),
        List(Dead, Dead, Dead, Dead, Dead))
      val end = List(
        List(Dead, Live, Live, Dead, Dead),
        List(Live, Dead, Dead, Live, Dead),
        List(Dead, Live, Dead, Live, Dead),
        List(Dead, Dead, Live, Dead, Dead))
      evolve(start) shouldBe step1
      evolve(step1) shouldBe end
      evolve(end) shouldBe end
    }

    ignore("should play") {
      def generate(width: Int, heigh: Int, aliveRatio: Double): Board = {
        (0 until heigh).map { _ =>
          (0 until width).map { _ =>
            if (Random.nextDouble() < aliveRatio) Live else Dead
          }.toList
        }.toList
      }

      def format(board: Board): String = {
        def formatCell(cell: Cell): String = if (cell == Dead) " " else "X"
        def formatRow(row: List[Cell]): String = "|" + row.map(formatCell).mkString + "|\n"

        val header = "+" + board.head.map(_ => "-").mkString + "+\n"
        header + board.map(formatRow).mkString + header
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
