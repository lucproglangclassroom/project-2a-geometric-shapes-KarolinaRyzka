package edu.luc.cs.laufer.cs371.shapes

/** data Shape = Rectangle(w, h) | Location(x, y, Shape) */
enum Shape derives CanEqual:
  case Rectangle(width: Int, height: Int)
    require(width > 0, s"rectangle width must be pos")
    require(height > 0, s"rectangle height must be pos")
  case Location(x: Int, y: Int, shape: Shape)
  
  // TODO add missing cases (see test fixtures) //done
  case Group(shapes: Shape*)
    require(shapes.nonEmpty, "group must have at least one shape")
  case Ellipse(width: Int, height: Int)
    require(width > 0, s"ellipse width radius must be pos")
    require(height > 0, s"ellipse height radius must be pos")

