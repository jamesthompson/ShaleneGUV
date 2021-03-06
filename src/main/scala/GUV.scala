package shalene

import scala.Serializable
import javafx.scene.chart.XYChart
import scala.math._
import collection.mutable.ArrayBuffer
import javafx.scene.input.{Clipboard, ClipboardContent}
import java.io.PrintWriter

/**
 * GUV data class
 * Author: James R. Thompson, D.Phil
 * Date: 6/15/12
 */

class GUV(var contours:IndexedSeq[Contour], val name:String) extends Serializable {

  def this(name:String) = this(IndexedSeq[Contour](), name)
  def addContour(cont:Contour) = contours = contours.+:(cont)
  def getContour(index:Int) = contours(index)
  def killContour(index:Int) = contours = (contours take index) ++ (contours drop (index + 1))
  def avgRadius : Double = contours.map(_.avgRadius).sum / contours.length
  def getSize = contours.length
  def getFrameChart(index:Int) = contours(index).getSeries(avgRadius)
  def getFrameDeviationChart(index:Int) = contours(index).getDeviationSeries
  def checkContoursOk {
    contours = contours.sortWith(_.avgRadius < _.avgRadius)
    println(contours.map(_.avgRadius).mkString("\n"))
  }
  def saveAllContours {
    System.out.println("Contours")
    val clipboard : Clipboard = Clipboard.getSystemClipboard
    val content : ClipboardContent = new ClipboardContent
    val angles = for(i <- 0 until 360) yield {
      val lineang = for (c <- contours) yield {
          c.points(i).polar.ang.toString
        }
      lineang.mkString("\t")
    }
    val radii = for(i <- 0 until 360) yield {
      val linerad = for (c <- contours) yield {
          c.points(i).polar.rad.toString
        }
      linerad.mkString("\t")
    }
    val outAng = angles.mkString("\n")
    val outRad = radii.mkString("\n")
    content.putString(outAng + "\n\n\n\n\n" + outRad)
    clipboard.setContent(content)
  }
  def calcStDev(in: IndexedSeq[Double], average: Double) = {
    def squaredDifference(v1:Double, v2:Double) = pow(v1 - v2,2.0)
    val squared = in.foldLeft(0.0)(_ + squaredDifference(_, average))
    sqrt(squared / in.length.toDouble)
  }
  def sqr(in:Double) : Double = in * in

	def calcScale : Double = contours.map(c => c.getMaxRadius).max

  def getAvgIntensity : IndexedSeq[Double] = contours.map(_.avgIntensity).reverse

  def getStDevAvgIntensities : IndexedSeq[Double] = contours.map(_.stDevIntensity).reverse

  def getAvgRadius : IndexedSeq[Double] = contours.map(_.avgRadius).reverse

  def getStDevRadius: IndexedSeq[Double] = contours.map(_.stDev).reverse

  case class GUVPrinter(avgInt: Double, stDevInt: Double, avgRadius: Double, stDevRadius: Double) {
    override def toString = s"$avgInt\t$stDevInt\t$avgRadius\t$stDevRadius"
  }

  def saveAvgIntensity : Unit = {
    val avgInts = getAvgIntensity
    val stDevInts = getStDevAvgIntensities
    val avgRadii = getAvgRadius
    val stDevRadii = getStDevRadius
    val nums = for(i <- 0 until getSize) yield GUVPrinter(avgInts(i), stDevInts(i), avgRadii(i), stDevRadii(i))
    val s = nums.mkString("\n")
    val out = new PrintWriter(name + ".Analysis.csv")
    out.print("Avg-Intensity\tStDev-Intensity\tAvg-Radius (px)\tStDev-Radius(px)\n")
    out.print(s)
    out.close
  }

  def getAvgIntensitySeries : XYChart.Series[Number, Number] = {
    val series = new XYChart.Series[Number, Number]
    val data = getAvgIntensity
    for {i <- 0 until data.length} {
      val np = new XYChart.Data[Number, Number](i, data(i))
      series.getData.add(np)
    }
    series
  }

  override def toString = name
}
