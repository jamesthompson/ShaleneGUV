package shalene

import javafx.scene.chart.XYChart

/**
 * Contour class - container for Points[Cartesian or Polar] describing an instance of an arbitrary(0 -> infinitely sized) polygon's edge in 2D space
 * Author: James R. Thompson, D.Phil
 * Date: 6/19/13
 */

class Contour(var points : IndexedSeq[Point], val avgIntensity : Double = 0.0) extends Serializable {
 
  def this() = this(IndexedSeq[Point]())   
  def addPoint(p:Point) = points = points.+:(p)
  def getPoint(index:Int) = points.apply(index)
  def numPoints = points.size
  def getRadii = points.map(_.polar.rad).toArray

	def getMaxRadius = points.map(_.polar.rad).max

  def sortPoints = points = {
    val xavg = points.map(_.cartesian.x).sum / points.length
    val yavg = points.map(_.cartesian.y).sum / points.length
    val newPoints = for(p <- points) yield pointFactory.mkCartesianPoint(p.cartesian.x - xavg, p.cartesian.y - yavg)
    newPoints.sortBy(_.polar.ang)
  }

  def avgRadius = getRadii.sum / numPoints
  def stDev = {
    val avg = avgRadius
    math.sqrt(points.map(_.polar.rad).map((r:Double) => contourMath.sqr(r - avg)).sum / numPoints)
  }
  def filter(multiplesOfStDev:Double) = {
    val avg = avgRadius
    val sdev = stDev
    points = points.filter((p:Point) => p.polar.rad <= avg + sdev*multiplesOfStDev && p.polar.rad >= avg - sdev*multiplesOfStDev)
  }
  def killPoint(index:Int) = (points take index) ++ (points drop (index + 1))
  def getSeries(scaleSize:Double) = {
    val series = new XYChart.Series[Number, Number]
    points.map(_.cartesian.getXYData).map((np:XYChart.Data[Number,Number]) => series.getData.add(np))
    series
  }
  def getDeviationSeries = {
    val devSeries = new XYChart.Series[Number, Number]
    val a = avgRadius
    points.map(_.polar.getXYData(a)).map((np:XYChart.Data[Number,Number]) => devSeries.getData.add(np))
    devSeries
  }

  override def toString = "~~~~Contour Polar Points Print Out~~~~\n\nAvg radius = " + avgRadius.toString + "\n\nSt. Deviation = " + stDev.toString + "\n\nAngle(radians)\t\tRadii\n\n" + points.toStream.map(_.polar.toString + "\n").mkString
  def toPolarString = points.toStream.map(_.polar.toString + "\n").mkString
  def toCartString = "~~~~Contour Cartesian Points Print Out~~~~\n\nAvg radius = " + avgRadius.toString + "\n\nSt. Deviation = " + stDev.toString + "\n\nx\t\t\ty\n\n" + points.toStream.map(_.cartesian.toString + "\n").mkString
}

object contourMath {
  def sqr(input: Double) = input * input
}