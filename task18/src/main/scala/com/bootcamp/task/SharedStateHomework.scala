package com.bootcamp.task

import cats.effect.IOApp
import cats.effect.concurrent.Ref

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 */
object SharedStateHomework extends IOApp {

  import cats._
  import cats.effect._
  import cats.implicits._

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_] : Clock : Monad, K, V](
      state: Ref[F, Map[K, (Long, V)]],
      expiresIn: FiniteDuration
  ) extends Cache[F, K, V] {

    def get(key: K): F[Option[V]] = state.get.map { map =>
      map.get(key).map { case (_, value) => value }
    }

    def put(key: K, value: V): F[Unit] = for {
      currentTime <- Clock[F].realTime(TimeUnit.MILLISECONDS)
      expiresAt = currentTime + expiresIn.toMillis
      _ <- state.update { orig => orig + (key -> (expiresAt, value)) }
    } yield ()

    def evictExpired: F[Unit] = for {
      currentTime <- Clock[F].realTime(TimeUnit.MILLISECONDS)
      _ <- state.update { map =>
        map.filter { case (_, (time, _)) => time > currentTime }
      }
    } yield ()
  }

  object Cache {
    def of[F[_] : Monad : Clock, K, V](
        expiresIn: FiniteDuration,
        checkOnExpirationsEvery: FiniteDuration
    )(implicit T: Timer[F], C: Concurrent[F]): Resource[F, Cache[F, K, V]] = {
      val cache = Ref.of[F, Map[K, (Long, V)]](Map()).map { state => new RefCache(state, expiresIn) }
      def tryEvict(cache: RefCache[F, K, V]): F[Unit] = for {
        _ <- T.sleep(checkOnExpirationsEvery)
        _ <- cache.evictExpired
        _ <- tryEvict(cache)
      } yield ()

      for {
        c <- Resource.eval(cache)
        _ <- Resource.make(C.start(tryEvict(c)))(_.cancel)
      } yield c
    }

  }

  override def run(args: List[String]): IO[ExitCode] =
    Cache.of[IO, Int, String](10.seconds, 4.seconds).use { cache =>
      for {
        _ <- cache.put(1, "Hello")
        _ <- cache.put(2, "World")
        _ <- cache.get(1).flatMap(s => IO {
          println(s"first key $s")
        })
        _ <- cache.get(2).flatMap(s => IO {
          println(s"second key $s")
        })
        _ <- IO.sleep(12.seconds)
        _ <- cache.get(1).flatMap(s => IO {
          println(s"first key $s")
        })
        _ <- cache.get(2).flatMap(s => IO {
          println(s"second key $s")
        })
        _ <- IO.sleep(12.seconds)
        _ <- cache.get(1).flatMap(s => IO {
          println(s"first key $s")
        })
        _ <- cache.get(2).flatMap(s => IO {
          println(s"second key $s")
        })
      } yield ExitCode.Success
  }
}

