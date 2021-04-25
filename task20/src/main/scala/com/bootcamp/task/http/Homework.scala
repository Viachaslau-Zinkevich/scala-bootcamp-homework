package com.bootcamp.task.http

import cats.effect._
import cats.implicits._
import cats.kernel.Comparison.{EqualTo, GreaterThan, LessThan}
import com.bootcamp.task.http.Cache
import com.bootcamp.task.http.GuessResult.Greater
import com.bootcamp.task.http.GuessResult.Less
import com.bootcamp.task.http.GuessResult.Equals
import com.bootcamp.task.http.Utils._
import org.http4s._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.dsl.io.{Created, _}
import org.http4s.headers._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Random


// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// The exact protocol and message format to use is not specified and should be designed while working on the task.

sealed trait GuessResult

object GuessResult {
  object Equals extends GuessResult
  object Less extends GuessResult
  object Greater extends GuessResult
}

final case class GuessResponse(result: GuessResult)

object Utils {
  def printLine(string: String = ""): IO[Unit] = IO(println(string))
}

object GuessServer extends IOApp {

  private val cache = Cache.of[IO, String, Long](30.minutes, 5.seconds)

  private implicit val R: Random = Random

  object FromMatcher extends ValidatingQueryParamDecoderMatcher[Long](name = "from")

  object ToMatcher extends ValidatingQueryParamDecoderMatcher[Long](name = "to")

  object ValueMatcher extends ValidatingQueryParamDecoderMatcher[Long](name = "value")


  private def routes(cache: Cache[IO, String, Long])(implicit R: Random) = HttpRoutes.of[IO] {

    case req @ POST -> Root / "game" / "start" :? FromMatcher(from) +& ToMatcher(to) => from.product(to).fold(
      errors => BadRequest(errors.toString), { case (from, to) =>
        for {
          oldSessionID <- IO(req.cookies.find(_.name == "SessionID").map(_.content))
          sessionID = oldSessionID.getOrElse(UUID.randomUUID().toString)
          rand <- IO(R.between(from, to))
          _ <- cache.put(sessionID, rand)
          r <- cache.get(sessionID)
          _ <- printLine(s"$sessionID: $rand")
          rs <- Created(r.toString).map(rs => rs.addCookie("SessionID", sessionID))
        } yield rs
      })

    case req @ GET -> Root / "game" / "guess" :? ValueMatcher(value) =>
      value.product(req.cookies.find(_.name == "SessionID").map(_.content)
        .toValidNel(ParseFailure("No cookie", "No SessionID provided"))).fold(
        errors => BadRequest(errors.toString),
        { case (value, sessionID) =>
          import io.circe.generic.auto._
          import org.http4s.circe.CirceEntityCodec._

          cache.get(sessionID).flatMap {
            case Some(guessedValue) => Ok(GuessResponse(value.comparison(guessedValue) match {
              case GreaterThan => Greater
              case EqualTo => Equals
              case LessThan => Less
            }))
            case None => BadRequest("Game not found")
          }
        }
      )
  }

  override def run(args: List[String]): IO[ExitCode] = cache.use { c =>
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 8080, host = "localhost")
      .withHttpApp(routes(c).orNotFound)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }

}

object GuessClient extends IOApp {

  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._

  private val uri = uri"http://localhost:8080"


  override def run(args: List[String]): IO[ExitCode] = BlazeClientBuilder[IO](ExecutionContext.global)
    .resource.use { implicit client =>
    for {
      from <- IO(Random.nextLong(Long.MaxValue))
      to <- IO(Random.between(from, Long.MaxValue))
      _ <- printLine("Executing simple GET and POST requests:")
      sessionID <- client.run(Request(Method.POST, (uri / "game" / "start")
        .withQueryParam("from", from)
        .withQueryParam("to", to))).use { rs =>
        IO.fromOption(rs.cookies.find(_.name == "SessionID").map(_.content))(new Exception("No Session ID"))
      }
      _ <- printLine(s"Started game with session:$sessionID")

      response <- {
        implicit val id: String = sessionID
        guessValue(from, to)
      }
      _ <- printLine(s"Guessed Value: $response")
    } yield ()
  }.as(ExitCode.Success)

  def guessValue(from: Long, to: Long)(implicit client: Client[IO], sessionID: String): IO[Long] =
      for {
        value <- IO(Random.between(from, to))
        rs <- client.expect[GuessResponse](Request[IO](Method.GET, (uri / "game" / "guess").withQueryParam("value", value),
                  headers = Headers.of(Cookie(RequestCookie("SessionID", sessionID)))))
        _ <- printLine(s"$value: ${rs.result}")
        result <- rs.result match {
          case Greater => guessValue(from, value)
          case Less => guessValue(to min value + 1, to)
          case Equals => IO.pure(value)
        }
      } yield result
}
