package shalene

import scala.math._

case class PolarLocation(val x:Double, val y:Double) {
  def getPoint(numAngles: Double) : Point = {
    val angle = y * (2*Pi/numAngles)
    val xloc = x * cos(angle) 
    val yloc = x * sin(angle)
    pointFactory.mkCartesianPoint(xloc, yloc)
  }
}

class EdgeFinder(img: Array[Double], width: Int, height: Int) {

  def findCentre : (Double, Double) = {
    implicit def conv2DTo1D(loc:(Int,Int)) : Int = loc._2 * width + loc._1
    def moment(in:IndexedSeq[((Int,Int), Double)]) = {
      val d = in.map(_._2).sum
      (in.map(v => v._1._1 * v._2).sum / d, in.map(v => v._1._2 * v._2).sum / d)
    }
    val pixelMap = for(x <- 0 until width; y <- 0 until height) yield ((x,y), img.apply((x,y)))
    moment(pixelMap)
  }

 	def convImgToPolar(angleLines:Int, thresholdPercent: Double, radiusThreshold: Int) : List[(List[Double], PolarLocation)] = {
 		val widthInitial = width
 		val heightInitial = height
    val centreLoc = findCentre
 		val centerX = centreLoc._1
 		val centerY = centreLoc._2
 		def getRad = {
 			var tempRad = sqrt((centerX * centerX) + (centerY * centerY))
 			var checkRad = sqrt((centerX - widthInitial) * (centerX - widthInitial) + (centerY * centerY))
 			if(checkRad > tempRad) tempRad = checkRad
 			checkRad = sqrt((centerX * centerX) + (centerY - heightInitial) * (centerY - heightInitial))
 			if(checkRad > tempRad) tempRad = checkRad
			checkRad = sqrt((centerX - widthInitial) * (centerX - widthInitial) + (centerY - heightInitial) * (centerY - heightInitial))
 			if(checkRad > tempRad) tempRad = checkRad
 			tempRad
 		}
 		val radius = getRad.toInt
    def getInterpolatedPixel(x:Double, y:Double) = {
      var xtemp = x
      var ytemp = y
      if(xtemp < 0.0) xtemp = 0.0
      if(xtemp >= width - 1.0) xtemp = width - 1.001
      if(ytemp < 0.0) ytemp = 0.0
      if(ytemp >= height - 1.0) ytemp = height - 1.001 
      val xbase = xtemp.toInt
      val ybase = ytemp.toInt
      val xFraction : Double = xtemp - xbase
      val yFraction : Double = ytemp - ybase
      val offset = ybase * width + xbase
      val lowerLeft = img(offset)
      val lowerRight = img(offset + 1)
      val upperRight = img(offset + width + 1)
      val upperLeft = img(offset + width)
      val upperAverage = upperLeft + xFraction * (upperRight - upperLeft)
      val lowerAverage = lowerLeft + xFraction * (lowerRight - lowerLeft)
      lowerAverage + yFraction * (upperAverage - lowerAverage)
    }
    var movingAvg = 0.0
 		val out = for(yy <- 0 until angleLines) yield {
 			val arr = for(xx <- 0 until radius - radiusThreshold) yield { // Make radius thresholded here <- ie. (radius - guess GUV size + x)
 				val r = xx
 				val angle = (yy / angleLines.toDouble) * Pi * 2
 				val x = r * cos(angle) + centerX
 				val y = r * sin(angle) + centerY
 				getInterpolatedPixel(x, y)
 			}
      val output = arr.toList
      val diff1 = {
        for(i <- 1 until output.length - 1) yield (output(i+1) - output(i)) - (output(i-1) - output(i))
      }
      val diff2 = { // Take the second differential, should give a minimum equal to the position of the 
        for(i <- 1 until diff1.length - 1) yield (diff1(i+1) - diff1(i)) - (diff1(i-1) - diff1(i))
      }
      val value = diff2.indexOf(diff2.min)
      val maxRange = value * (thresholdPercent / 100.0)
      if(yy < 1) {
        movingAvg = value
        (arr.toList, PolarLocation(value, yy.toDouble))
      } else if(value > movingAvg + maxRange || value < movingAvg - maxRange) {
        (arr.toList, PolarLocation(movingAvg, yy.toDouble))
      } else {
        movingAvg += value
        movingAvg = movingAvg / 2
        (arr.toList, PolarLocation(movingAvg, yy.toDouble))
      }
 		}
    out.toList
  }

  def getPoints(in:List[(List[Double], PolarLocation)]) : IndexedSeq[Point] = {
    val angleLines = in.length
    in.map(_._2).map(_.getPoint(angleLines.toDouble)).toIndexedSeq
 	}

}