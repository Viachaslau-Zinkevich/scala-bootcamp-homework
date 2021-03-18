package com.evolutiongaming.bootcamp.typeclass

import com.evolutiongaming.bootcamp.typeclass.ImplicitsHomework.SuperVipCollections4s.GetSizeScore

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

//fill in implementation gaps here making the ImplicitsHomeworkSpec pass!
object ImplicitsHomework {

	/**
	 * Lo and behold! Brand new super-useful collection library for Scala!
	 *
	 * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
	 * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
	 * of the data stored.
	 *
	 * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
	 * a thing called size score. Its calculation rules:
	 * - size score of a Byte is 1
	 * - Int - 4 (as primitive JVM int consists of 4 bytes)
	 * - Long - 8
	 * - Char - 2 (one UTF-16 symbol is 2 bytes)
	 * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
	 * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
	 * the fields
	 * - score for any sequence (Array[T], List[T], Vector[T]) is
	 * 12 (our old friend object header) + sum of scores of all elements
	 * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
	 */
	object SuperVipCollections4s {
		type SizeScore = Int

		trait GetSizeScore[T] {
			def apply(value: T): SizeScore
		}

		trait GetSizeScore2[T[_]] {
			def apply[D: GetSizeScore](value: T[D]): SizeScore
		}

		object syntax {

			implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
				def sizeScore: SizeScore = implicitly[GetSizeScore[T]].apply(inner)
			}

			implicit class GetSizeScoreOps2[T[_] : GetSizeScore2, D: GetSizeScore](inner: T[D]) {
				def sizeScore: SizeScore = implicitly[GetSizeScore2[T]].apply(inner)
			}

		}

		/**
		 * Mutable key-value cache which limits the size score of the data scored.
		 *
		 * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
		 * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
		 * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
		 * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
		 *
		 * @param maxSizeScore max size score for the stored data
		 * @tparam K key type
		 * @tparam V value type
		 */
		final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
			//with this you can use .sizeScore syntax on keys and values

			import syntax._

			implicit val tupleSizeScore: GetSizeScore[(K, V)] = {
				case (k, v) => k.sizeScore + v.sizeScore
			}
			/*
			mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
			 */
			private val map = mutable.LinkedHashMap.empty[K, V]

			private var totalScore: SizeScore = 0

			def put(key: K, value: V): Unit = {
				val score = key.sizeScore + value.sizeScore
				val existingObjectScore = get(key).map(_.sizeScore).getOrElse(0)

				evictIfNecessary(totalScore - existingObjectScore + score)
				totalScore += score
				map.put(key, value)
			}

			private def evictIfNecessary(lastScore: SizeScore): Unit = {
				@tailrec
				def evict(entries: List[(K, V)], lastScore: SizeScore): Unit =
					entries match {
						case (k, v) :: tail if lastScore > maxSizeScore =>
							val size = (k, v).sizeScore

							totalScore -= size
							map.remove(k)

							evict(tail, lastScore - size)
						case _ =>
					}

				evict(map.toList, lastScore)
			}

			def get(key: K): Option[V] = map.get(key)
		}

		/**
		 * Cool custom immutable multi-map collection - does not extend the standard library collection types
		 * (yes, this is a feature)
		 */
		final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])

		object PackedMultiMap {
			def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()

			def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
		}

		/**
		 * Type-class allowing us to iterate over different "collection-like" types with one type arg
		 */
		trait Iterate[-F[_]] {
			def iterator[T](f: F[T]): Iterator[T]
		}

		/**
		 * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
		 */
		trait Iterate2[-F[_, _]] {
			def iterator1[T, S](f: F[T, S]): Iterator[T]

			def iterator2[T, S](f: F[T, S]): Iterator[S]
		}

		object instances {

			implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
				override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
			}
			//Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
			implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
				override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
			}

			implicit val mapIterate: Iterate2[Map] = new Iterate2[Map] {
				override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keys.iterator

				override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.values.iterator
			}

			implicit val multiIterate: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
				override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.iterator.map({ case (a, _) => a })

				override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.iterator.map({ case (_, b) => b })
			}

			def fixed[T](size: SizeScore): GetSizeScore[T] = _ => size

			abstract class ReferencedSizeScore[T] extends GetSizeScore[T] {
				override def apply(value: T): SizeScore = 12 + fieldsSize(value)

				def fieldsSize(value: T): SizeScore
			}

			import syntax._

			implicit val byteGetSizeScore: GetSizeScore[Byte] = fixed(1)
			implicit val charGetSizeScore: GetSizeScore[Char] = fixed(2)
			implicit val intGetSizeScore: GetSizeScore[Int] = fixed(4)
			implicit val longGetSizeScore: GetSizeScore[Long] = fixed(8)
			implicit val stringGetSizeScore: ReferencedSizeScore[String] = s => s.length * 'c'.sizeScore
			implicit val anySizeScore: GetSizeScore[Any] = fixed(12)

			implicit def iteratorGetSizeScore[T: GetSizeScore]: GetSizeScore[Iterator[T]] = {
				s => s.map(_.sizeScore).sum
			}

			implicit def iterableGetSizeScore[F[_]: Iterate, T: GetSizeScore]: GetSizeScore[F[T]] =
				s => (s : Any).sizeScore + implicitly[Iterate[F]].iterator(s).sizeScore

			implicit def iterableGetSizeScore2[F[_, _]: Iterate2, K: GetSizeScore, V: GetSizeScore]: GetSizeScore[F[K, V]] = {
				value =>
					val iterate2 = implicitly[Iterate2[F]]
					(value : Any).sizeScore + iterate2.iterator1(value).sizeScore + iterate2.iterator2(value).sizeScore
			}


			/*implicit val iteratorGetSizeScore: GetSizeScore2[Iterator] = new GetSizeScore2[Iterator] {
				override def apply[D: GetSizeScore](value: Iterator[D]): SizeScore = value.map(_.sizeScore).sum
			}

			implicit class GetSizeScoreIterate[T[_] : Iterate, K: GetSizeScore](inner: T[K])(implicit val imp: Iterate[T]) {
				def sizeScore: SizeScore = (inner: Any).sizeScore + imp.iterator(inner).sizeScore
			}*/

			/*implicit class GetSizeScoreIterate2[T[_, _] : Iterate2, K: GetSizeScore, V: GetSizeScore](inner: T[K, V])(implicit val imp: Iterate2[T]) {
				def sizeScore: SizeScore = (inner: Any).sizeScore + imp.iterator1(inner).sizeScore + imp.iterator2(inner).sizeScore
			}*/

		}

	}

	/*
	Time to bring some business value!
	#GoodVibes #ThrowbackThursday #NoFilter #squadgoals
	 */
	object MyTwitter {

		import SuperVipCollections4s._

		final case class Twit(
								 id: Long,
								 userId: Int,
								 hashTags: Vector[String],
								 attributes: PackedMultiMap[String, String],
								 fbiNotes: List[FbiNote],
							 )

		final case class FbiNote(
									month: String,
									favouriteChar: Char,
									watchedPewDiePieTimes: Long,
								)

		trait TwitCache {
			def put(twit: Twit): Unit

			def get(id: Long): Option[Twit]
		}

		/*
		Return an implementation based on MutableBoundedCache[Long, Twit]
		 */
		def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {

			import instances._
			import syntax._

			private implicit val noteGetSizeScore: ReferencedSizeScore[FbiNote] = note =>
				note.favouriteChar.sizeScore + note.month.sizeScore + note.watchedPewDiePieTimes.sizeScore

			private implicit val twitGetSizeScore: ReferencedSizeScore[Twit] = twit =>
				twit.id.sizeScore + twit.userId.sizeScore + twit.attributes.sizeScore +
					twit.fbiNotes.sizeScore + twit.hashTags.sizeScore

			private val cache = new MutableBoundedCache[Long, Twit](maxSizeScore)

			override def put(twit: Twit): Unit = cache.put(twit.id, twit)

			override def get(id: Long): Option[Twit] = cache.get(id)
		}
	}

}
