package com.bootcamp.task

import cats.Functor

object Excercises {
	trait Applicative[F[_]] extends Functor[F] {
		def map[A,B](fa: F[A])(f: A => B): F[B]

		def unit[A](a: => A): F[A]

		// implement methods using each other
		def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
			val fbc = map(fa)(f.curried)
			apply(fbc)(fb)
		}

		def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
			map2(fa, fab)((a, f) => f(a))

		def sequence[A](fas: List[F[A]]): F[List[A]] =
			traverse(fas)(f => f)


		def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
			as.map(a => f(a))
				.foldRight(unit(List.empty[B]))((fb, fl) => map2(fb, fl)
				((b, l) => b :: l))
	}

	trait Monad[M[_]] extends Functor[M] {
		def unit[A](a: => A): M[A]

		// implement methods using each other
		def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

		def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

		def map[A,B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))

		def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = {
			val fa = map(ma)(a => f.curried(a))
			flatMap(mb)(b => map(fa)(_(b)))
		}
	}
}


object Monads {
	import scala.language.higherKinds

	trait Monad[M[_]] {
		def unit[A](a: A): M[A]
		def bind[A, B](ma: M[A])(amb: A => M[B]): M[B]
	}

	trait Monoid[A] {
		def mempty: A
		def mappend(x: A)(y: A): A
	}


	case class Identity[A](a: A)
	object Identity {
		implicit val identityMonad = new Monad[Identity] {
			def unit[A](a: A): Identity[A] = Identity(a)
			def bind[A, B](ma: Identity[A])(amb: A => Identity[B]): Identity[B] =
				amb(ma.a)
		}
	}

	sealed trait Maybe[+A]
	case class Just[A](a: A) extends Maybe[A]
	case object None extends Maybe[Nothing]
	object Maybe {
		implicit val maybeMonad = new Monad[Maybe] {
			def unit[A](a: A): Maybe[A] = Just(a)
			def bind[A, B](ma: Maybe[A])(amb: A => Maybe[B]): Maybe[B] = ma match {
				case None => None
				case Just(a) => amb(a)
			}
		}
	}

	case class State[S, A](run: S => (S, A))
	object State {
		// inspired by https://stackoverflow.com/a/6248296
		implicit def stateMonad[S] = new Monad[({type x[a]=State[S, a]})#x] {
			def unit[A](a: A): State[S, A] = State((_, a))
			def bind[A, B](ma: State[S, A])(amb: A => State[S, B]): State[S, B] = {
				State(s => {
					val (ss, a) = ma.run(s)
					amb(a).run(ss)
				})
			}
		}
	}

	case class Reader[R, A](run: R => A)
	object Reader {
		implicit def readerMonad[R] = new Monad[({type x[a]=Reader[R, a]})#x] {
			def unit[A](a: A): Reader[R, A] = Reader(_ => a)
			def bind[A, B](ma: Reader[R, A])(amb: A => Reader[R, B]): Reader[R, B] =
				Reader(r => amb(ma.run(r)).run(r))
		}
	}

	case class Writer[W, A](run: (W, A))
	object Writer {
		implicit def readerMonad[W](implicit m: Monoid[W]) = new Monad[({type x[a]=Writer[W, a]})#x] {
			def unit[A](a: A): Writer[W, A] = Writer((m.mempty, a))
			def bind[A, B](ma: Writer[W, A])(amb: A => Writer[W, B]): Writer[W, B] = {
				val (w, a) = ma.run
				val (ww, aa) = amb(a).run
				Writer(m.mappend(w)(ww), aa)
			}
		}
	}
}
