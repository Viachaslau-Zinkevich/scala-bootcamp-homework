package com.bootcamp.task

object Homework {
	object Task1 {
		final case class Money(amount: BigDecimal)

		implicit val moneyOrdering: Ordering[Money] = (a,b) => a.amount compareTo b.amount
	}

	object Task2 {
		trait Show[T] { // fancy toString
			def show(entity: T): String
		}

		final case class User(id: String, name: String)

		object ShowSyntax {
			implicit class ShowOps[A](value: A) {
				def show(implicit s: Show[A]): String = s.show(value)
			}
		}

		import ShowSyntax._
		implicit val showUser: Show[User] = user => s"My name is ${user.name} with ID ${user.id}"

		User("1", "Oleg").show
	}

	object Task3 {
		type Error = String
		trait Parse[T] { // invent any format you want or it can be csv string
			def parse(entity: String): Either[Error, T]
		}

		object CsvParseSyntax {
			implicit class CsvParseOps (value: String) {
				def parse[T](implicit parser: Parse[T]): Either[Error,T] = parser.parse(value)
			}
		}

		import CsvParseSyntax._

		implicit val listParse: Parse[List[String]] = v => Right(v.split(",\\s*").toList)

		abstract class BaseCsvParser[T] extends Parse[T] {
			override def parse(entity: String): Either[Error, T] = entity.parse[List[String]]
				.flatMap(parse)

			abstract def parse(row: List[String]): Either[Error, T]
		}

		implicit val userParse: BaseCsvParser[User] = {
			case Nil => Left("Empty list")
			case id :: name :: Nil => Right(User(id, name))
			case _ => Left("Invalid args")
		}

		final case class User(id: String, name: String)

		val user = "1, User".parse[User]

		val invalid = "lalala".parse[User]

	}

	object Task4 {

		trait Equals[T] {
			def equals(a: T, b: T): Boolean
		}

		object EqualsSyntax {
			implicit class EqualsOps[T](a: T) {
				def === (b: T)(implicit e: Equals[T]): Boolean = e.equals(a, b)
			}
		}

		import EqualsSyntax._
		implicit val stringEquals: Equals[String] = _.equals(_)
		"a" === "b"
		// "a" === 1 doesn't compile
	}

	object AdvancedHomework {

		trait FlatMapped[F[_]] {
			def flatMap[A, B](f: A => F[B])(fa: F[A]): F[B]
		}

		object FlatMapped {
			def apply[A](implicit instance: FlatMapped[A]): FlatMapped[A] = instance
		}

		object FlatMappedSyntax {
			implicit class FlatMappedOps[A, F[_]: FlatMapped](fa: F[A]) {
				def fMap[B](f: A => F[B]): F[B] = FlatMapped[F].flatMap(f)(fa)
			}
		}

		import FlatMappedSyntax._

		implicit val v: FlatMapped[Option] = new FlatMapped[Option] {
			override def flatMap[A, B](f: A => Option[B])(fa: Option[A]): Option[B] = fa.flatMap(f)
		}

		Option[Int](1).fMap(v => Some(v.toString))
	}
}
