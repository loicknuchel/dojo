package org.sample

import lol.http._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FunSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class HelloWorldTest extends FunSpec with Matchers with ScalaFutures {

  describe("Hello World") {
    it("should say Hello") {
      def hello(name: String) = s"Hello $name"
      hello("World") shouldBe "Hello World"
    }

    // GET https://jsonplaceholder.typicode.com/posts/1
    it("should perform requests") {
      case class Post(userId: Int, id: Int, title: String, body: String)
      Client("jsonplaceholder.typicode.com", 443, maxConnections = 1).runAndStop { client =>
        for {
          r <- client.run(Get("/posts/1"))(Future.successful)
          _ = r.status shouldBe 200
        } yield ()
      }
      //sampleClient(Get("/posts/1")).futureValue.status shouldBe 200
    }
  }

}
