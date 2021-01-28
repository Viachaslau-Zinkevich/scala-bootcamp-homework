package com.bootcamp.task

import scala.math.{hypot, max, min, sqrt}

sealed trait Located {
	def x: Double
	def y: Double
}

sealed trait Bounded {
	def minX: Double
	def maxX: Double
	def minY: Double
	def maxY: Double
}

sealed trait Shape extends Located with Bounded {
	def area: Double
}

final case class Point(x: Double, y: Double) extends Shape {
	override val minX: Double = x
	override val maxX: Double = x
	override val minY: Double = y
	override val maxY: Double = y
	override val area: Double = 0

	def destination(d: Point): Double = hypot(d.x - x, d.y - y)
}

final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
	override val x: Double = centerX
	override val y: Double = centerY
	override val minX: Double = centerX - radius
	override val maxX: Double = centerX + radius
	override val minY: Double = centerY - radius
	override val maxY: Double = centerY + radius
	override val area: Double = Math.PI * radius * radius
}

sealed trait Tetragon extends Shape {
	def w: Double
	def h: Double
	def angle: Double
}

sealed case class Rectangle(x: Double, y: Double, w: Double, h: Double) extends Tetragon {
	override val minX: Double = x
	override val maxX: Double = x + w
	override val minY: Double = y
	override val maxY: Double = y + h
	override val area: Double = w * h
	override val angle: Double = 90
}

sealed case class Square(x: Double, y: Double, w: Double) extends Tetragon {

	private[this] val delegate = Rectangle(x, y, w, w)

	override val area: Double = delegate.area
	override val minX: Double = delegate.minX
	override val maxX: Double = delegate.maxX
	override val minY: Double = delegate.minY
	override val maxY: Double = delegate.maxY
	override val angle:Double = delegate.angle
	override val h: Double = w
}

final class Triangle(n1: Point, n2: Point, n3: Point) extends Shape {
	private val nodes = Vector(n1, n2, n3)
	private val leftPoint = nodes.minBy(_.x)

	override val minX: Double = nodes.map(_.x).min
	override val maxX: Double = nodes.map(_.x).max
	override val minY: Double = nodes.map(_.y).min
	override val maxY: Double = nodes.map(_.y).max
	override val x: Double = leftPoint.x
	override val y: Double = leftPoint.y
	override val area: Double = {
		val a = n1 destination n2
		val b = n2 destination n3
		val c = n1 destination n3
		val p = (a + b + c) / 2
		sqrt(p * (p - a) * (p - b) * (p - c))
	}
}

// 3D shapes
sealed trait Projectable[+S <: Shape] {
	def projection: S
}

sealed trait Located3D extends Located {
	def z: Double
}

sealed trait Bounded3D extends Bounded {
	def minZ: Double
	def maxZ: Double
}

sealed trait Shape3D extends Shape with Located3D with Bounded3D {
	def volume: Double
}


sealed abstract class Shape3DBase[P <: Shape](override val projection: P) extends Shape3D with Projectable[P] {
	override val x: Double = projection.x
	override val y: Double = projection.y
	override val minX: Double = projection.minX
	override val maxX: Double = projection.maxX
	override val minY: Double = projection.minY
	override val maxY: Double = projection.maxY
}

final case class Sphere(override val projection: Circle, z: Double) extends Shape3DBase[Circle](projection) {

	override val minZ: Double = z + projection.radius
	override val maxZ: Double = z - projection.radius
	override val area: Double = 4 * projection.area
	override val volume: Double = (area / 3) * projection.radius
}

final case class Point3D(override val projection: Point, z: Double) extends Shape3DBase[Point](projection) {
	override val minZ: Double = z
	override val maxZ: Double = z
	override val area: Double = 0
	override val volume: Double = 0
}

sealed case class Cuboid(override val projection: Rectangle, z: Double, d: Double) extends Shape3DBase[Rectangle](projection) {
	override val minZ: Double = z + d
	override val maxZ: Double = z - d
	override val area: Double = 2 * (projection.area + projection.w * d + projection.h * d)
	override val volume: Double = projection.area * d
}

sealed case class Cube(override val projection: Square, z: Double) extends Shape3DBase[Square](projection) {

	private[this] val delegate = Cuboid(Rectangle(projection.x, projection.y, projection.w, projection.w),
		z, projection.w)

	override def volume: Double = delegate.volume
	override def area: Double = delegate.area
	override def minZ: Double = delegate.minZ
	override def maxZ: Double = delegate.maxZ
}

sealed case class Pyramid(override val projection: Triangle, baseZ: Double, top: Point3D)
	extends Shape3DBase[Triangle](projection) {

	override val x: Double = projection.x
	override val y: Double = projection.y
	override val z: Double = baseZ
	override val minZ: Double = min(top.z, baseZ)
	override val maxZ: Double = max(top.z, baseZ)
	override val minX: Double = min(top.x, projection.minX)
	override val maxX: Double = max(top.x, projection.maxX)
	override val minY: Double = min(top.y, projection.minY)
	override val maxY: Double = max(top.y, projection.maxY)
	override val area: Double = ???
	override val volume: Double = ???
}

sealed case class Triangle3D(n1: Point3D, n2: Point3D, n3: Point3D) extends Shape3D with Projectable[Triangle] {

	private val nodes = Seq(n1, n2, n3)

	override val projection: Triangle = new Triangle(n1.projection, n2.projection, n3.projection)
	override def x: Double = n1.x
	override def y: Double = n1.y
	override def z: Double = n1.z
	override def minX: Double = projection.minX
	override def maxX: Double = projection.maxX
	override def minY: Double = projection.minY
	override def maxY: Double = projection.maxY
	override def minZ: Double = nodes.map(_.z).min
	override def maxZ: Double = nodes.map(_.z).max
	override def area: Double = ???
	override def volume: Double = 0
}
