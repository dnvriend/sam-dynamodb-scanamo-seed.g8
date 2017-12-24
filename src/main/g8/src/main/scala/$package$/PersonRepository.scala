package $package$

import java.util.UUID

import com.amazonaws.services.dynamodbv2.{AmazonDynamoDB, AmazonDynamoDBClientBuilder}
import com.github.dnvriend.lambda.annotation.HttpHandler
import com.github.dnvriend.lambda.{ApiGatewayHandler, HttpRequest, HttpResponse, SamContext}
import com.gu.scanamo._
import com.gu.scanamo.syntax._
import play.api.libs.json._

object Person {
  implicit val format: Format[Person] = Json.format[Person]
}
final case class Person(id: Option[String], name: String, age: Int, lucky_number: Int = 0)

final case class PersonDynamoDbRecord(id: String, json: String)
class PersonRepository(tableName: String, ctx: SamContext) {
    val client: AmazonDynamoDB = AmazonDynamoDBClientBuilder.defaultClient()
    val table: Table[PersonDynamoDbRecord] = Table[PersonDynamoDbRecord](ctx.dynamoDbTableName(tableName))

  def id: String = UUID.randomUUID.toString

  def put(id: String, person: Person): Unit = {
    val putOperation = table.put(PersonDynamoDbRecord(id, Json.toJson(person).toString))
    Scanamo.exec(client)(putOperation)
  }

  def get(id: String): Option[Person] = {
    val getOperation = table.get('id -> id)
    for {
      record <- Scanamo.exec(client)(getOperation)
      json <- record.toOption.map(_.json)
      person <- Json.parse(json).asOpt[Person]
    } yield person
  }

    def list: List[Person] = {
      val scanOperation = table.scan()
      for {
        record <- Scanamo.exec(client)(scanOperation)
        json <- record.toOption.map(_.json)
        person <- Json.parse(json).asOpt[Person]
      } yield person
    }
}

@HttpHandler(path = "/person", method = "post")
class PostPerson extends ApiGatewayHandler {
  override def handle(request: HttpRequest, ctx: SamContext): HttpResponse = {
    val repo = new PersonRepository("people", ctx)
    val id: String = repo.id
    val person = request.bodyOpt[Person].get
    repo.put(id, person)
    HttpResponse.ok.withBody(Json.toJson(person.copy(id = Option(id))))
  }
}

@HttpHandler(path = "/person", method = "get")
class GetListOfPerson extends ApiGatewayHandler {
  override def handle(request: HttpRequest, ctx: SamContext): HttpResponse = {
    val repo = new PersonRepository("people", ctx)
    HttpResponse.ok.withBody(Json.toJson(repo.list))
  }
}

@HttpHandler(path = "/person/{id}", method = "get")
class GetPerson extends ApiGatewayHandler {
  override def handle(request: HttpRequest, ctx: SamContext): HttpResponse = {
    val repo = new PersonRepository("people", ctx)
    request.pathParamsOpt[Map[String, String]].getOrElse(Map.empty).get("id")
      .fold(HttpResponse.notFound.withBody(Json.toJson("Person not found")))(id => {
        HttpResponse.ok.withBody(Json.toJson(repo.get(id)))
      })
  }
}