package util

import scala.collection.mutable

/**
  * Created by yitao on 1/30/17.
  */
object Util {
//  def isEqual(e1: Double, e2: Double): Boolean = math.abs(e1-e2)<0.00000000000001


  def isEqual(e1: Double, e2: Double): Boolean = {
    if (math.abs(e1-e2)<0.0000001) true
    else math.abs(if (e1>e2) (e1-e2)/e2 else (e2-e1)/e1) < 0.000000000001
  }

  def readLines(filePath: String): (Array[Array[Int]],Array[Double]) ={
    val input = io.Source.fromFile(filePath)
    val inputLines= input.mkString.split("\n")
    input.close()
    val samplesToWeights = mutable.Map[String,Double]()
    inputLines.filter(_.nonEmpty).foreach{line=>
      samplesToWeights.put(line,1.0+samplesToWeights.getOrElse(line,0.0))
    }
    val (samples,weights) = samplesToWeights.toArray.unzip
    (samples.map(s=>s.split(",").map(_.toInt)),weights)
  }

  def manhattanDistance(a:Array[Int],b:Array[Double]):Double = {
    (a,b).zipped.map(_-_).map(x=>math.abs(x)).sum
  }

  def euclideanDistance(a:Array[Int],b:Array[Double]):Double = {
    (a,b).zipped.map(_-_).map(x=>x*x).sum
  }

  def convertToAssignment(sample:Array[Int]):Map[Int,Boolean]= {
    sample.zipWithIndex.map{case (v,i) => i+1 -> (v==1)}.toMap
  }

  def convertToAssignment(sample:String):Map[Int,Boolean] = {
    sample.split(",").zipWithIndex.map{case (v,i) => i+1 -> !v.contains("0")}.toMap
  }

}
