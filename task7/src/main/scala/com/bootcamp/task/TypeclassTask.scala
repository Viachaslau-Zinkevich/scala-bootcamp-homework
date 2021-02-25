package com.bootcamp.task

object TypeclassTask {

	trait HashCode[A] {
		def hash(entity: A): Int
	}

	object DefaultHashCode extends HashCode[Any] {
		override def hash(entity: Any): Int = entity.hashCode()
	}

	object HashCode {
		def apply[A](implicit instance: HashCode[A]): HashCode[A] = instance
	}

	implicit class HashCodeOps[A](x: A) {
		def hash(implicit h: HashCode[A]): Int = h.hash(x)
	}

	implicit val defHash: HashCode[String] = DefaultHashCode.hash

	"asd".hash
}
