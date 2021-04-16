package com.bootcamp.task.effects

import scala.concurrent.Future
import scala.util.Try

/*
 * Homework 1. Provide your own implementation of a subset of `IO` functionality.
 *
 * Provide also tests for this functionality in EffectsHomework1Spec (which you should create).
 *
 * Refer to:
 *  - https://typelevel.org/cats-effect/datatypes/io.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO$.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO.html
 * about the meaning of each method as needed.
 *
 * There are two main ways how to implement IO:
 * - Executable encoding  - express every constructor and operator for our model in terms of its execution
 * - Declarative encoding - express every constructor and operator for our model as pure data in a recursive
 *                          tree structure
 *
 * While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this
 * task using executable encoding, that is:
 *  - Add a `private val run: () => A` parameter to the class `IO` private constructor
 *  - Have most of the methods return a `new IO(...)`
 *
 * Ask questions in the bootcamp chat if stuck on this task.
 */
object EffectsHomework1 {
  final class IO[+A](private val supplier: () => A) {
    def map[B](f: A => B): IO[B] = IO(f(supplier()))
    def flatMap[B](f: A => IO[B]): IO[B] = IO(f(supplier()).unsafeRunSync())
    def *>[B](another: IO[B]): IO[B] = this.flatMap(_ => another)
    def as[B](newValue: => B): IO[B] = this.map(_ => newValue)
    def void: IO[Unit] = this.map(_ => ())
    def attempt: IO[Either[Throwable, A]] = IO(Try(supplier()).toEither)
    def option: IO[Option[A]] = this.map(Option(_))
    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] =
      this.redeemWith(f, IO(_))
    def redeem[B](recover: Throwable => B, map: A => B): IO[B] =
      this.redeemWith (
        err => IO(recover(err)),
        res => IO(map(res))
      )
    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] =
      this.attempt.flatMap {
        case Right(res) => bind(res)
        case Left(err) => recover(err)
      }
    def unsafeRunSync(): A = supplier()
    def unsafeToFuture(): Future[A] = Future.fromTry(Try(unsafeRunSync()))
  }

  object IO {
    def apply[A](body: => A): IO[A] = new IO(() => body)
  }
}
