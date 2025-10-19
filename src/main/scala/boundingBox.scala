package edu.luc.cs.laufer.cs371.shapes


// TODO: implement this behavior

import Shape.*
import com.typesafe.scalalogging.Logger //check from proj 1b

object boundingBox:
  private val logger = Logger(getClass)

  def apply(s: Shape): Location = s match
    case Rectangle(w, h) => Location(0, 0, Rectangle(w, h)) // not yet implemented //done has bounding box around origin
    
    case Ellipse(radiusx, radiusy) => Location(-radiusx, -radiusy, Rectangle(2 * radiusx, 2 * radiusy)) //ellipse has bbox of rectangle that is 2 * radius of x and 2 * radius of y,

    case Location (locx, locy, inner) => //find child's bbox and translate by location of x and location of y
      val Location(x, y, Rectangle(w, h)) = apply(inner)
      Location (x + locx, y + locy, Rectangle(w, h))

    case Group(shapes*) => //group's bbox is min/max of all child boxes
      val boxes = 
        shapes
          .map(apply)
          .map { case Location(x, y, Rectangle(w, h)) => (x, y, w, h) }
      val initialize =  //get first element as option
        boxes.headOption
          .map { case (x, y, w, h) => (x, y, w + x, h + y) } //(minX, minY, maxX, maxY)
          .getOrElse((0,0,0,0))
      val (minX, minY, maxX, maxY) =
        boxes.tail.foldLeft(initialize):
          case ((mnx, mny, mxx, mxy), (x, y, w, h)) =>
            (math.min(mnx, x), math.min(mny, y), math.max(mxx, w + x), math.max(mxy, h + y))  
      Location(minX, minY, Rectangle(maxX - minX, maxY - minY))
     
end boundingBox

object size:
  private val logger = Logger(getClass)

  def apply(s: Shape): Int = // count nodes, rectangle and ellipse each 1, location 1 inner shape, group is sum of sizes of children
    val n = s match
      case Rectangle(_,_) => 1
      case Ellipse(_,_) => 1
      case Location(_,_, inner) => apply(inner)
      case Group(shapes*) => shapes.map(apply).sum 
    logger.debug(s"size($s) = $n")
    n

object height:
  private val logger = Logger(getClass)

  def apply(s: Shape): Int = //root to leaf: rectangle and ellipse each a leaf of 1, location is 1 + inner child, group is 1 + the max height of child
    val h = s match
      case Rectangle(_,_) => 1
      case Ellipse(_,_) => 1
      case Location(_,_, inner) => 1 + apply(inner)
      case Group(shapes*) => 1 + shapes.map(apply).max
    logger.debug(s"height($s) = $h")
    h

object scale:
  private val logger = Logger(getClass)
  //recursively by factor of f

  def apply(s: Shape, f: Int): Shape = 
    val scale = s match
      case Rectangle(w, h) => Rectangle(w * f, h * f)
      case Ellipse(radiusx, radiusy) => Ellipse(radiusx * f, radiusy * f)
      case Location(locx, locy, inner) => Location(locx * f, locy * f, apply(inner, f))
      case Group(shapes*) => Group(shapes.map(apply(_, f))*)
    logger.debug(s"scale($s, $f) => $scale")
    scale

    
    
