package edu.luc.cs.laufer.cs371.shapes

import org.scalatest.funsuite.AnyFunSuite

import TestFixtures.*
import Shape.*

class TestBoundingBox extends AnyFunSuite:

  def testBoundingBox(description: String, s: Shape, x: Int, y: Int, width: Int, height: Int): Unit =
    test(description):
      val Location(u, v, Rectangle(w, h)) = boundingBox(s)
      assert(x == u)
      assert(y == v)
      assert(width == w)
      assert(height == h)

  // TODO comment these tests back in

  testBoundingBox("simple ellipse", simpleEllipse, -50, -30, 100, 60)
  testBoundingBox("simple rectangle", simpleRectangle, 0, 0, 80, 120)
  testBoundingBox("simple location", simpleLocation, 70, 30, 80, 120)
  testBoundingBox("basic group", basicGroup, -50, -30, 100, 70)
  testBoundingBox("simple group", simpleGroup, 150, 70, 350, 280)
  testBoundingBox("complex group", complexGroup, 30, 60, 470, 320)

end TestBoundingBox

//test constructors and if requirements pass
class TestConstructors extends AnyFunSuite:

  test("rectangle must have pos dimensions"):
    assertThrows[IllegalArgumentException](Rectangle(0, 5))
    assertThrows[IllegalArgumentException](Rectangle(5, 0))
    assertThrows[IllegalArgumentException](Rectangle(-5, 5))
    assertThrows[IllegalArgumentException](Rectangle(5, -5))

  test("ellipse radius must be pos"):
    assertThrows[IllegalArgumentException](Ellipse(0, 5))
    assertThrows[IllegalArgumentException](Ellipse(5, 0))
    assertThrows[IllegalArgumentException](Ellipse(-5, 5))
    assertThrows[IllegalArgumentException](Ellipse(5, -5))

  test("groups should be non-empty"):
    assertThrows[IllegalArgumentException](Group())

end TestConstructors

class TestSize extends AnyFunSuite: //test size according to bounding box tests
  
  def testSize(description: String, s: Shape, expected: Int): Unit =
    test(description):
      assert(size(s) == expected)

  testSize("simple ellipse", simpleEllipse, 1)
  testSize("simple rectangle", simpleRectangle, 1)
  testSize("simple location", simpleLocation, 1)
  testSize("basic group", basicGroup, 2)
  testSize("simple group", simpleGroup, 2)
  testSize("complex group", complexGroup, 5)

end TestSize


class TestHeight extends AnyFunSuite: //test height according to bounding box tests

  def testHeight(description: String, s: Shape, expected: Int): Unit =
    test(description):
      assert(height(s) == expected)

  testHeight("simple ellipse", simpleEllipse, 1)
  testHeight("simple rectangle", simpleRectangle, 1)
  testHeight("simple location", simpleLocation, 2)
  testHeight("basic group", basicGroup, 2)
  testHeight("simple group", simpleGroup, 3)
  testHeight("complex group", complexGroup, 6)

end TestHeight


class TestScale extends AnyFunSuite: //test scale according to bounding box tests
  def testScale(description: String, s: Shape, f: Int, x: Int, y: Int, width: Int, height: Int): Unit =
    test(description):
      val Location(u, v, Rectangle(w, h)) = (boundingBox(scale(s, f)): @unchecked)
      assert(x == u)
      assert(y == v)
      assert(width == w)
      assert(height == h)

  testScale("scale simple ellipse by 2", simpleEllipse, 2, -100, -60, 200, 120)
  testScale("scale simple rectangle by 3", simpleRectangle, 3, 0, 0, 240, 360)
  testScale("scale simple location by 2", simpleLocation, 2, 140, 60, 160, 240)
  testScale("scale basic group by 2", basicGroup, 2, -100, -60, 200, 140)
  testScale("scale simple group by 2", simpleGroup, 2, 300, 140, 700, 560)
  testScale("scale complex group by 2", complexGroup, 2, 60, 120, 940, 640)

end TestScale
