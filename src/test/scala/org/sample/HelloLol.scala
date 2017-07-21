package org.sample

import io.circe.Json
import io.circe.optics.JsonPath._
import lol.http._
import lol.json._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.{BeforeAndAfterAll, FunSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global

class HelloLol extends FunSpec with Matchers with ScalaFutures with BeforeAndAfterAll {
  implicit val defaultPatience = PatienceConfig(timeout = Span(5, Seconds), interval = Span(500, Millis))
  val typicodeClient = Client("jsonplaceholder.typicode.com")

  override def afterAll(): Unit = {
    typicodeClient.stop()
  }

  describe("LOL") {
    it("should perform requests") {
      val res = typicodeClient.run(Get("/users")) { response =>
        response.readAs[Json].map(root.each.username.string.getAll)
      }.futureValue
      res.length shouldBe 10
    }
  }
}
