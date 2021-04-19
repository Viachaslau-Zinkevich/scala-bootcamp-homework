package com.bootcamp.task.effects

import cats.data.Validated.{Invalid, Valid}
import cats.effect.{Blocker, ContextShift, ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxParallelTraverse
import cats.{Applicative, Monad}
import com.bootcamp.task.effects.Homework._

import java.io.File
import java.nio.file.{Files, Path}
import java.util.concurrent.{ConcurrentHashMap, Executors}
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.io.StdIn
import scala.util.Try

object Homework {

	import cats.data._
	import cats.effect._
	import cats.implicits._

	type Error = String

	final case class FPath(path: Path) extends AnyVal {
		private def copy() = ()
	}

	object FPath {
		def apply(path: String): Validated[Error, FPath] = {
			val file = new File(path)
			file match {
				case f if !f.exists() => s"Path doesn't exists: $f".invalid
				case f if !f.isFile => s"Is not a file: $f".invalid
				case f if !f.canRead => s"Cannot read a file: $f".invalid
				case f => Try(new FPath(f.toPath)).toValidated.leftMap(_.toString)
			}
		}
	}

	final case class Seed(value: Int) extends AnyVal {
		private def copy() = ()
	}

	object Seed {
		def fromString(raw: String): Validated[Error, Seed] =
			Try(raw.toInt).toValidated
				.leftMap(_.toString)
				.map(new Seed(_))
	}

	trait DataSource[F[_], P] {
		def provide(): F[P]
	}

	type Converter[R] = String => Validated[_, R]

	final class ConsoleSource[F[_]: Monad, P](message: String, blocker: Blocker)(
		implicit converter: Converter[P],
		sync: Sync[F], cs: ContextShift[F]) extends DataSource[F, P] {
		override def provide(): F[P] = blocker.delay(println(message)) *> blocker.delay(StdIn.readLine())
			.map(converter)
			.flatMap {
				case Valid(v) => implicitly[Monad[F]].pure(v)
				case Invalid(e) => blocker.delay(println(s"Error: $e")) *> provide()
			}
	}

	object ConsoleSource {
		def of[F[_]: Sync: ContextShift, P: Converter](message: String, blocker: Blocker): DataSource[F, P] =
			new ConsoleSource[F, P](message, blocker)
	}

	object WordsReader {
		def words[F[_]: Sync: ContextShift](blocker: Blocker, p: FPath): F[List[String]] = {
			Resource.fromAutoCloseableBlocking(blocker)(Sync[F].delay(Files.newBufferedReader(p.path)))
				.use { reader =>
					@tailrec
					def lines(list: List[String]): List[String] = reader.readLine() match {
						case null => list
						case line => lines(list :++ line.split("\\s+").toList)
					}
					Sync[F].delay(lines(Nil))
				}
		}
	}

	trait SigStorage[F[_]] {
		def save(path: String, algo: Hash, sig: Int): F[Unit]
	}

	object SigStorage {
		def inMemory[F[_]: Applicative]: SigStorage[F] = new SigStorage[F] {

			private val cache = new ConcurrentHashMap[(String, String), Int]()

			override def save(path: String, algo: Hash, sig: Int): F[Unit] =
				Applicative[F].pure(cache.put((path, algo.name), sig)).as(())

		}
	}

	sealed trait Hash {
		def apply(word: String, seed: Int): Int

		val name: String
	}

	object Hash {
		val javaHash: Hash = new Hash {
			def apply(word: String, seed: Int = 0): Int = {
				var hash = 0

				for (ch <- word.toCharArray)
					hash = 31 * hash + ch.toInt

				hash = hash ^ (hash >> 20) ^ (hash >> 12)
				hash ^ (hash >> 7) ^ (hash >> 4)
			}

			override val name: String = "java"
		}

		val knuthHash: Hash = new Hash {
			def apply(word: String, seed: Int = 0): Int = {
				var hash = 0
				for (ch <- word.toCharArray)
					hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
				hash % seed
			}

			override val name: String = "knuth"
		}
	}

}

object App extends IOApp {

	override implicit def contextShift: ContextShift[IO] =
		IO.contextShift(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))

	override def run(args: List[String]): IO[ExitCode] = {
		implicit val pathConverter: Converter[FPath] = FPath.apply
		implicit val seedConverter: Converter[Seed] = Seed.fromString

		val algos: List[Hash] = List(Hash.javaHash, Hash.knuthHash)

		val blocker = Blocker.liftExecutorService(Executors.newSingleThreadExecutor())
		val fReader: DataSource[IO, FPath] = ConsoleSource.of[IO, FPath]("Enter file path:", blocker)
		val seedReader: DataSource[IO, Seed] = ConsoleSource.of[IO, Seed]("Enter seed:", blocker)
		val sigStorage: SigStorage[IO] = SigStorage.inMemory[IO]

		val res = for {
			path <- fReader.provide()
			seed <- seedReader.provide()
			words <- WordsReader.words[IO](blocker, path)
			_ <- algos.parTraverse[IO, Unit] { hash =>
				words.parTraverse[IO, Int] { w =>
					IO(hash(w, seed.value))
				}
					.map(_.min)
					.flatMap( sig => sigStorage.save(path.path.toString, hash, sig) <* IO(println(s"Saved: ${path.path};${hash.name};$sig")))
			}
		} yield ExitCode.Success

		res.flatMap(m => IO(println(m)) as m)
	}
}
