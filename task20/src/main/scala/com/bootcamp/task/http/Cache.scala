package com.bootcamp.task.http

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, Resource, Timer}
import cats.implicits._

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

trait Cache[F[_], K, V] {
	def get(key: K): F[Option[V]]

	def put(key: K, value: V): F[Unit]
}

object Cache {
	private final class RefCache[F[_] : Clock : Monad, K, V](
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
