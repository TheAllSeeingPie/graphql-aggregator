package Server

import com.twitter.finagle.http.{Method, Request, Response, Status}
import com.twitter.finagle.{Http, Service}
import com.twitter.util.{Await, Future, FuturePool}
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods.{compact, pretty, render}
import sangria.ast.Value
import sangria.introspection.{IntrospectionField, IntrospectionScalarType, IntrospectionType}
import sangria.marshalling.MarshallerCapability
import sangria.marshalling.json4s.Json4sJacksonSupportLowPrioImplicits
import sangria.marshalling.json4s.jackson.Json4sJacksonInputParser._
import sangria.marshalling.json4s.jackson._
import sangria.parser.QueryParser
import sangria.renderer.QueryRenderer
import sangria.schema.{Action, Context, DefaultIntrospectionSchemaBuilder, Field, ObjectType, Schema, StringType}
import sangria.validation.Violation

import scala.collection.immutable
import scala.io.Source
import scala.util.{Failure, Success}

class Materialiser(host: String, ssl: Boolean = false, path: String = "/graphql") {
  val hello: Field[Any, Any] = Field("hello", StringType, resolve = _ => "world!")
  val QueryType = ObjectType("Query", "The root of all queries", List(hello))
  var schema: Schema[Any, Any] = Schema(QueryType)
  val client = {
    val client = Http.client
    if (ssl) client.withTls(host).newService(s"$host:443")
    else client.newService(s"$host:80")
  }

  def materialise(rawQuery: String) = {
    val request = Request(Method.Post, path)
    request.contentType = "application/json"
    request.contentString = compact(render(("operationName" -> "IntrospectionQuery") ~
      ("query" -> rawQuery) ~
      ("variables" -> JObject(List()))))
    val response = Await.result(client(request))
    parse(response.contentString) match {
      case Success(json) => schema = Schema.buildFromIntrospection(json)
    }
    response.contentString
  }
}

class GraphQLService extends Service[Request, Response] with Json4sJacksonSupportLowPrioImplicits {
  private val pool = FuturePool.interruptibleUnboundedPool
  val materialiser = new Materialiser("api.hollowverse.com", ssl = true)

  override def apply(request: Request) = {
    if (request.path == "/graphql") executeQuery(request)
    else pool(graphiql)
  }

  private def createResponse(content: String, status: Status = Status.Ok) = {
    val response = Response(status)
    response.contentString = content
    response
  }

  private def createResponseFuture(content: Future[String], status: Status = Status.Ok) = {
    content.map(c => {
      val response = Response(status)
      response.contentString = c
      response
    })
  }

  private val graphiql = createResponse(Source.fromInputStream(getClass().getClassLoader().getResourceAsStream("graphiql.html")).mkString)

  private def executeQuery(request: Request) = {
    implicit val formats = org.json4s.DefaultFormats

    parse(request.contentString) match {
      case Success(input) => {
        val JString(query) = input \ "query"
        val vars = (input \ "variables").toOption
        val operation = (input \ "operationName").extractOpt[String]

        QueryParser.parse(query) match {
          case Success(parsedQuery) => {
            if (operation == Some("IntrospectionQuery") || parsedQuery.operations.contains(Some("IntrospectionQuery"))) {
              Future.value(createResponse(materialiser.materialise(query)))
            } else {
              val mapped = parsedQuery.operations.map(_._2).map(d => {
                d.selections.map(s => {
                  val rendered = QueryRenderer.render(s)
                  s match {
                    case sangria.ast.Field(_, name, args, _, _, _, _, _) => {
                      val replaced = vars match {
                        case None => rendered
                        case Some(vars) => {
                          vars match {
                            case JObject(vars) => vars.map(v => (v._1, compact(render(v._2))))
                              .foldRight(rendered)((arg, r) => {
                                r.replace("$" + arg._1, arg._2)
                              })
                          }
                        }
                      }
                      materialiser.schema.query.fields.collectFirst {
                        case Field(`name`, _, _, _, _, _, _, _, _) => {
                          implicit val formats = org.json4s.DefaultFormats
                          val resolveRequest = Request(Method.Post, "/graphql")
                          resolveRequest.contentType = "application/json"
                          resolveRequest.contentString = pretty(render(("operationName" -> operation) ~ ("query" -> s"{ $replaced } ") ~ ("variables" -> vars)))
                          println(resolveRequest.contentString)
                          materialiser.client(resolveRequest)
                        }
                      }
                    }
                  }
                })
              })
              val vals: immutable.Iterable[Vector[Future[Response]]] = mapped.map(_.map(f => f.get))
              val vals2: immutable.Iterable[Future[Seq[Response]]] = vals.map(f => Future.collect(f))
              val vals3: Future[Seq[Seq[Response]]] = Future.collect(vals2.toList)
              val vals4: Future[Seq[JValue]] = vals3.map(f => f.flatten).map(f => f.map(r => parse(r.contentString) match {
                case Success(json) => json \\ "data"
              }))
              val vals5 = vals4.map(s => {
                s.fold(JNothing)((a, b) => a merge b)
              })
              createResponseFuture(vals5.map(f => compact(render(("data" -> f)))))
            }
          }
          case Failure(failure) => pool(createResponse(compact(render("error" -> failure.getMessage)), Status.BadRequest))
        }
      }
      case Failure(failure) => pool(createResponse(failure.toString, Status.BadRequest))
    }
  }
}
