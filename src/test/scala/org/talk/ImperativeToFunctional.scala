package org.talk

import java.io.{FileNotFoundException, PrintWriter}
import java.text.{ParseException, SimpleDateFormat}
import java.util.Date

import org.scalatest.{FunSpec, Matchers}

import scala.io.Source
import scala.util.{Failure, Success, Try}

class ImperativeToFunctional extends FunSpec with Matchers {
  describe("average age") {
    /**
      * Spec :
      *   - calculate average age for emplyees
      *   - exclude too young employees
      *   - optionally limit to a team
      */

    case class Employee(name: String, age: Int)

    case class Team(employees: Seq[Employee]) {
      def has(employee: Employee): Boolean =
        employees.contains(employee)
    }

    val employees = Seq(
      Employee("Jim", 28),
      Employee("John", 50),
      Employee("Liz", 35),
      Employee("Penny", 40))

    val sales = Team(employees.take(2))

    it("is imperative code") {
      def averageAge(employees: Seq[Employee],
                     min: Int,
                     team: Team
                    ): Int = {
        var total = 0
        var count = 0
        for (e <- employees) {
          if (e.age >= min && (team == null || team.has(e))) {
            total += e.age
            count += 1
          }
        }
        if (count == 0) throw new Exception("...")
        else total / count
      }

      averageAge(employees, 30, sales)
      averageAge(employees, 30, null)
    }

    it("should have an explicit signature") {
      def averageAge(employees: Seq[Employee],
                     min: Int,
                     team: Option[Team] = None
                    ): Try[Int] = {
        var total = 0
        var count = 0
        for (e <- employees) {
          if (e.age >= min && team.forall(_.has(e))) {
            total += e.age
            count += 1
          }
        }
        if (count == 0) Failure(new Exception("..."))
        else Success(total / count)
      }

      averageAge(employees, 30, Some(sales))
      averageAge(employees, 30)
    }

    it("can use collection API") {
      def averageAge(employees: Seq[Employee],
                     min: Int,
                     team: Option[Team] = None
                    ): Try[Int] = {
        val ages = employees
          .filter(e => e.age >= min && team.forall(_.has(e)))
          .map(_.age)

        if (ages.isEmpty) Failure(new Exception("..."))
        else Success(ages.sum / ages.length)
      }

      averageAge(employees, 30, Some(sales))
      averageAge(employees, 30)
    }

    it("can extract generic code") {
      def averageAge(employees: Seq[Employee],
                     min: Int,
                     team: Option[Team] = None
                    ): Try[Int] = {
        val ages = employees
          .filter(e => e.age >= min && team.forall(_.has(e)))
          .map(_.age)
        average(ages)
      }

      def average(nums: Seq[Int]): Try[Int] =
        if (nums.isEmpty) Failure(new Exception("..."))
        else Success(nums.sum / nums.length)

      averageAge(employees, 30, Some(sales))
      averageAge(employees, 30)
    }

    it("can extract specific code") {
      def averageAge(employees: Seq[Employee],
                     predicate: Employee => Boolean
                    ): Try[Int] =
        average(employees.filter(predicate).map(_.age))

      def average(nums: Seq[Int]): Try[Int] =
        if (nums.isEmpty) Failure(new Exception("..."))
        else Success(nums.sum / nums.length)

      averageAge(employees, e => e.age >= 30 && sales.has(e))
      averageAge(employees, _.age >= 30)
    }

    type Predicate[T] = T => Boolean

    it("can build a DSL") {
      def averageAge(employees: Seq[Employee],
                     predicate: Predicate[Employee]
                    ): Try[Int] =
        average(employees.filter(predicate).map(_.age))

      def average(nums: Seq[Int]): Try[Int] =
        if (nums.isEmpty) Failure(new Exception("..."))
        else Success(nums.sum / nums.length)

      def gt(v: Int): Predicate[Employee] = (e: Employee) => e.age >= v

      def in(team: Team): Predicate[Employee] = (e: Employee) => team.has(e)

      def and[T](ps: Predicate[T]*) = (e: T) => ps.forall(_ (e))

      averageAge(employees, and(gt(30), in(sales)))
      averageAge(employees, gt(30))

      // can be extended easily

      def or[T](ps: Predicate[T]*) = (e: T) => ps.exists(_ (e))

      def not[T](pred: Predicate[T]) = (e: T) => !pred(e)

      def between(min: Int, max: Int): Predicate[Employee] =
        and(gt(min), not(gt(max)))

      averageAge(employees, or(between(30, 50), not(in(sales))))

      // and fallback to previous signature

      def averageAgeOld(employees: Seq[Employee],
                        min: Int,
                        team: Team
                       ): Int =
        averageAge(employees, and(gt(min), in(team))).get
    }
  }

  describe("read and parse a file") {
    /**
      * Spec :
      *   - read a file containing a date
      */

    it("is imperative code") {
      def lastExec(path: String): Date = {
        val text: String = Source.fromFile(path).mkString
        new SimpleDateFormat("yyyy-MM-dd").parse(text)
      }

      val date: Date = lastExec(".lastExec")
    }

    it("is imperative code with some error handling") {
      def lastExec(path: String): Date = {
        var text: String = null
        try {
          text = Source.fromFile(path).mkString
        } catch {
          case _: FileNotFoundException => return null
        }
        try {
          new SimpleDateFormat("yyyy-MM-dd").parse(text)
        } catch {
          case _: ParseException => null
        }
      }

      val date: Date = lastExec(".lastExec")
    }

    it("should have a correct signature") {
      def lastExec(path: String): Try[Date] = Try {
        val text: String = Source.fromFile(path).mkString
        new SimpleDateFormat("yyyy-MM-dd").parse(text)
      }

      val date: Date = lastExec(".lastExec").get
    }

    it("can be split in smaller and reusable functions") {
      def read(path: String): Try[String] =
        Try(Source.fromFile(path).mkString)

      def parse(fmt: String, d: String): Try[Date] =
        Try(new SimpleDateFormat(fmt).parse(d))

      def lastExec(path: String): Try[Date] =
        read(path).flatMap(parse("yyyy-MM-dd", _))

      val date: Date = lastExec(".lastExec").get
    }

    class Lazy[A](block: => A) {
      private lazy val underlying: A = block

      def get: A = underlying

      def map[B](f: A => B): Lazy[B] =
        Lazy(f(underlying))

      def flatMap[B](f: A => Lazy[B]): Lazy[B] =
        Lazy(f(underlying).get)
    }

    object Lazy {
      def apply[A](block: => A): Lazy[A] =
        new Lazy(block)
    }

    it("can be pure") {
      def read(path: String): Lazy[String] =
        Lazy(Source.fromFile(path).mkString)

      def parse(fmt: String, d: String): Lazy[Date] =
        Lazy(new SimpleDateFormat(fmt).parse(d))

      def lastExec(path: String): Lazy[Date] =
        read(path).flatMap(parse("yyyy-MM-dd", _))

      val date: Date = lastExec(".lastExec").get
    }
  }

  describe("read/write file") {
    /**
      * Spec :
      *   - read & print a file
      *   - write it
      *   - read & print it again
      */

    val path = "src/main/resources/file.txt"

    def readFile(path: String): String =
      Source.fromFile(path).mkString

    def writeFile(path: String, text: String): Unit =
      new PrintWriter(path) {
        write(text)
        close()
      }

    it("has side effects everywhere") {
      println("start")
      var content = readFile(path)
      println(content)
      writeFile(path, "new content !")
      content = readFile(path)
      println(content)

      // > start
      // > file content
      // > new content !
    }

    class IO[A](effect: () => A) {
      def run(): A =
        effect()

      def map[B](f: A => B): IO[B] =
        IO(() => f(run()))

      def flatMap[B](f: A => IO[B]): IO[B] =
        IO(() => f(run()).run())
    }

    object IO {
      def apply[A](effect: () => A): IO[A] =
        new IO(effect)
    }

    def readFileIO(path: String): IO[String] =
      IO(() => readFile(path))

    def writeFileIO(path: String, text: String): IO[Unit] =
      IO(() => writeFile(path, text))

    def printlnIO(content: String): IO[Unit] =
      IO(() => println(content))

    it("isolate side effects") {
      val program: IO[Unit] = for {
        content <- readFileIO(path)
        _ <- printlnIO(content)
        _ <- writeFileIO(path, "new content !")
        newContent <- readFileIO(path)
        _ <- printlnIO(newContent)
      } yield ()

      println("start")
      program.run()

      // > start
      // > file content
      // > new content !
    }

    it("can be refactored") {
      val readAndPrintFile: IO[Unit] =
        readFileIO(path).flatMap(printlnIO)

      val program: IO[Unit] = for {
        _ <- readAndPrintFile
        _ <- writeFileIO(path, "new content !")
        _ <- readAndPrintFile
      } yield ()

      println("start")
      program.run()

      // > start
      // > file content
      // > new content !
    }

    it("can be refactored again") {
      val readAndPrintFile: IO[Unit] =
        readFileIO(path).flatMap(printlnIO)

      def wrap[A, B](a: IO[A], b: IO[B]): IO[A] =
        a.flatMap(_ => b).flatMap(_ => a)

      val program: IO[Unit] = wrap(
        readAndPrintFile,
        writeFileIO(path, "new content !"))

      println("start")
      program.run()

      // > start
      // > file content
      // > new content !
    }

    it("does not work with side effect") {
      def readFileIO2(path: String): IO[String] = {
        println(s"readFileIO($path)")
        IO(() => readFile(path))
      }

      def wrap[A, B](a: IO[A], b: IO[B]): IO[A] =
        a.flatMap(_ => b).flatMap(_ => a)

      val readAndPrintFile: IO[Unit] =
        readFileIO2(path).flatMap(printlnIO)

      val program: IO[Unit] = wrap(
        readAndPrintFile,
        writeFileIO(path, "new content !"))

      println("start")
      program.run()

      // > readFileIO(src/main/resources/file.txt)
      // > start
      // > file content
      // > new content !
    }
  }
}
