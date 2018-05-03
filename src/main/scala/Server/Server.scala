package Server

import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.{Http, Service}
import com.twitter.util.{Await, Future}

import scala.io.Source

object Server extends App {
  override def main(args: Array[String]) = {
    val testApi = Http.serve(":8000", new Service[Request, Response] {
      override def apply(request: Request) = if (request.path == "/swagger") Future.value({
        val r = Response()
        r.contentString = Source.fromInputStream(getClass().getClassLoader().getResourceAsStream("HelloWorld.swagger")).mkString
        r
      }) else Future.value({
        val r = Response()
        r.contentString = Source.fromInputStream(getClass().getClassLoader().getResourceAsStream("HelloWorld.json")).mkString
        r
      })
    })
    val dynamicExample = Http.serve(":3000", new GraphQLService)
    Await.all(testApi, dynamicExample)
  }
}


