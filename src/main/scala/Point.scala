package shalene

import javafx.scene.chart.{Axis, NumberAxis, XYChart}

/** Cartesian Point case class. Overriden toString (x, y)
 * @param x - Value for x location
 * @param y - Value for y location  
*/
case class CartesianPoint(x: Double, y: Double) {
  /** @return A new `XYChart.Data[Number, Number]` = x, y */
  def getXYData = new XYChart.Data[Number, Number](x,y)
  override def toString : String = x.toString + "\t" + y.toString
}

/** Polar Point case class. Overriden toString (ang, rad)
 * @param ang - Value for angle (in radians)
 * @param rad - Value for radius (any units with double precision)  
*/
case class PolarPoint(ang: Double, rad: Double) {
  /** @return A new `XYChart.Data[Number, Number]` = x, y */
  def getXYData(dev:Double) = new XYChart.Data[Number, Number](ang,rad - dev)
  override def toString : String = ang.toString + "\t" + rad.toString
}

/** Full featured cartesian and polar representations of contour points
 *
 * ===Instantiation===
 * {{{
 *  val newPointFromCartesianCoords = pointFactory.mkCartesianPoint(5.0, 4.2) // Using x and y values
 *  val newPointFromPolarCoords = pointFactory.mkPolarPoint(0.4, 3.54) // Using angle(radians) and radius values
 *  }}}
 *  This will return a fully initialized `Point` object.
 *
 * @constructor '''Must be instantiated with either cartesian or polar coordinates, or both! An absolute requirement!'''
 *
 * @author James R. Thompson, D.Phil
 * @since 6/11/12
 * @param cartesian - A `CartesianPoint` field, used to instantiate
 * @param polar - A `PolarPoint` field, used to instantiate
 */
class Point(var cartesian:CartesianPoint, var polar:PolarPoint) extends Serializable {
  require(cartesian != null || polar != null)
  def this(cart: CartesianPoint) = this(cart, PolarPoint(math.atan2(cart.y,cart.x), math.sqrt(pointUtil.sqr(cart.x) + pointUtil.sqr(cart.y))))
  def this(pol:PolarPoint) = this(CartesianPoint(math.cos(pol.ang) * pol.rad, math.sin(pol.ang) * pol.rad), pol) 
  def polarToCart(pol:PolarPoint) = CartesianPoint(math.cos(pol.ang) * pol.rad, math.sin(pol.ang) * pol.rad)
  def cartToPolar(cart:CartesianPoint) = PolarPoint(math.atan2(cart.y,cart.x), math.sqrt(pointUtil.sqr(cart.x) + pointUtil.sqr(cart.y)))
  /**  @return Euclidean distance from this point to the input Cartesian point */
  def euclidDistance(input:CartesianPoint) : Double = math.sqrt((cartesian.x + input.x) - (cartesian.y + input.y))
  /**  @return Euclidean distance from this point to the input Polar point */
  def euclidDistance(input:PolarPoint) : Double = euclidDistance(polarToCart(input))
  /**  @return Radius difference between this point and the input PolarPoint*/
  def radiusDiff(input:PolarPoint) = polar.rad - input.rad
  /**  @return Absolute radius difference between this point and the input PolarPoint*/
  def radiusDiffAbs(input:PolarPoint) = math.abs(radiusDiff(input))
}

/** Point static builder object - used to create a [[com.malmstadt.data.Point]] object
 * @author James R. Thompson, D.Phil
 * @since 6/11/12
 */
object pointFactory {
  /**
    * @return A `Point` object using cartesian inputs.
    * @param x - Value for x location
    * @param y - Value for y location  
  */
  def mkCartesianPoint(x: Double, y: Double) = new Point(CartesianPoint(x, y))
  /**
    * @return A `Point` object using polar inputs.
    * @param ang - Value for angle (in radians)
    * @param rad - Value for radius (any units with double precision)  
  */
  def mkPolarPoint(ang: Double, rad: Double) = new Point(PolarPoint(ang, rad))
}

/** Utility object for the squaring of `Double` values */
object pointUtil {
  def sqr(x:Double) = x * x
}