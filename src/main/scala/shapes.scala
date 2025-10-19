package edu.luc.cs.laufer.cs371.shapes

/** data Shape = Rectangle(w, h) | Location(x, y, Shape) */
enum Shape derives CanEqual:
  case Rectangle(width: Int, height: Int)
  case Location(x: Int, y: Int, shape: Shape)
  
  // TODO add missing cases (see test fixtures) //done
  case Group(shapes: Shape*)
  case Ellipse(width: Int, height: Int)

//for shape constructor validity, rectangles and ellipses positive, groups non empty
object Shape:
  
  object Rectangle:
    def apply(width: Int, height: Int): Shape.Rectangle =
      require(width > 0, s"rectangle width must be positive")
      require(height > 0, s"rectangle height must be positive")
      new Shape.Rectangle(width, height)

  object Group:
    def apply(shapes: Shape*): Shape.Group =
      require(shapes.nonEmpty, "group must have at least one shape")
      new Shape.Group(shapes*)


  object Ellipse:
    def apply(width: Int, height: Int): Shape.Ellipse =
      require(width > 0, s"ellipse rx must be positive")
      require(height > 0, s"ellipse ry must be positive")
      new Shape.Ellipse(width, height)

