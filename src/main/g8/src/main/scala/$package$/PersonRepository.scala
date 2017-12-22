package $package$

import java.util.UUID

import com.amazonaws.services.dynamodbv2.{AmazonDynamoDB, AmazonDynamoDBClientBuilder}
import com.amazonaws.services.lambda.runtime.Context
import com.github.dnvriend.lambda.annotation.HttpHandler
import com.github.dnvriend.lambda.{ApiGatewayHandler, HttpRequest, HttpResponse}
import com.gu.scanamo._
import com.gu.scanamo.syntax._
import play.api.libs.json._

object Person {
  implicit val format: Format[Person] = Json.format[Person]
}
final case class Person(id: Option[String], name: String, age: Int, lucky_number: Int = 0)

final case class PersonDynamoDbRecord(id: String, json: String)
object PersonRepository {
  final val TableName = "sam-seed-dnvriend-people"
  val client: AmazonDynamoDB = AmazonDynamoDBClientBuilder.defaultClient()
  val table: Table[PersonDynamoDbRecord] = Table[PersonDynamoDbRecord](TableName)

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
}

@HttpHandler(path = "/person", method = "post")
class PostPerson extends ApiGatewayHandler {
  override def handle(request: HttpRequest, ctx: Context): HttpResponse = {
    val person = request.bodyOpt[Person].get
    val id = UUID.randomUUID.toString
    PersonRepository.put(id, person)
    HttpResponse(200, Json.toJson(person.copy(id = Option(id))), Map.empty)
  }
}

object PersonId {
  implicit val format: Format[PersonId] = Json.format
}
final case class PersonId(id: String)
object PersonNotFound {
  implicit val format: Format[PersonNotFound] = Json.format
}
final case class PersonNotFound(msg: String)
@HttpHandler(path = "/person/{id}", method = "get")
class GetPerson extends ApiGatewayHandler {
  override def handle(request: HttpRequest, ctx: Context): HttpResponse = {
    val personIdPathParam: Option[PersonId] = request.pathParamsOpt[PersonId]
    val personId: String = personIdPathParam.map(_.id).getOrElse("no id in path param")
    val person = PersonRepository.get(personId)
    person.fold(
      HttpResponse(404, Json.toJson(PersonNotFound("person with id '" + personId + "' not found")), Map.empty)
    )(person => HttpResponse(200, Json.toJson(person), Map.empty))
  }
}