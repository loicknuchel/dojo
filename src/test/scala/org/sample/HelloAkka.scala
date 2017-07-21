package org.sample

import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.optics.JsonPath
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpMethods, HttpRequest}
import akka.stream.ActorMaterializer
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.{FunSpec, Matchers}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class HelloAkka extends FunSpec with Matchers with ScalaFutures {
  implicit val defaultPatience = PatienceConfig(timeout = Span(5, Seconds), interval = Span(500, Millis))
  private implicit val system = ActorSystem()
  private implicit val materializer = ActorMaterializer()

  describe("Akka") {
    case class Coords(lat: String, lng: String)
    case class Address(street: String, suite: String, city: String, zipcode: String, geo: Coords)
    case class Comany(name: String, catchPhrase: String, bs: String)
    case class User(id: Int, name: String, username: String, email: String, address: Address, phone: String, website: String, company: Comany)

    it("should perform a GET request") {
      def httpGet(url: String): Future[String] = {
        println("GET " + url)
        Http().singleRequest(HttpRequest(uri = url)).flatMap(_.entity.toStrict(300.millis).map(_.data.utf8String))
      }

      val response = httpGet("https://jsonplaceholder.typicode.com/users").futureValue
      response.length shouldBe 5645

      val users = decode[List[User]](response)
      users.getOrElse(List()).length shouldBe 10
    }

    it("should perform a POST request") {
      def httpPost(url: String, data: String): Future[String] = {
        println("POST " + url + " " + data)
        Http().singleRequest(HttpRequest(
          method = HttpMethods.POST,
          uri = url,
          entity = data
        )).flatMap(_.entity.toStrict(300.millis).map(_.data.utf8String))
      }

      val coords = Coords("-37.3159", "81.1496")
      val address = Address("Kulas Light", "Apt. 556", "Gwenborough", "92998-3874", coords)
      val company = Comany("test", "try it", "bs")
      val user = User(25, "test", "test", "test@mail.com", address, "010-692-6593 x09125", "test.com", company)
      case class Id(id: Int)
      val response = httpPost("https://jsonplaceholder.typicode.com/users", user.asJson.spaces2).futureValue
      response.length shouldBe 14
      val json = response.asJson
      println("response: "+response)
      println("json: "+json)
      val id1 = json.hcursor.downField("id").as[Int]
      println("id1: "+id1)
      val id2 = json.hcursor.get[Int]("id")
      println("id2: "+id2)
      val id3 = JsonPath.root.id.int.getOption(json)
      println("id3: "+id3)
      val id4 = decode[Id](response)
      println("id4: "+id4)
    }
  }
}
