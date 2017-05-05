package org.sample

import io.circe.Json
import io.circe.optics.JsonPath._
import lol.http._
import lol.json._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FunSpec, Matchers}
import org.scalatest.time.{Millis, Seconds, Span}

import scala.concurrent.ExecutionContext.Implicits.global

class HelloWorld extends FunSpec with Matchers with ScalaFutures with BeforeAndAfterAll {
  implicit val defaultPatience = PatienceConfig(timeout = Span(5, Seconds), interval = Span(500, Millis))
  val typicodeClient = Client("jsonplaceholder.typicode.com", 443, "https")

  override def afterAll(): Unit = {
    typicodeClient.stop()
  }

  describe("Hello World") {
    it("should say Hello") {
      def hello(name: String) = s"Hello $name"
      hello("World") shouldBe "Hello World"
    }

    it("should perform requests") {
      val res = typicodeClient.run(Get("/users")) { response =>
        response.readAs[Json].map(root.each.username.string.getAll)
      }
      res.futureValue shouldBe List()
    }
  }
}
