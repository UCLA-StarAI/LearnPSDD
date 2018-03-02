/**
  * Created by MikeLyt on 1/30/17.
  */

package util

import algo.{ParameterCalculator,ParameterCalculatorWithoutSmoothing,
             MEstimateParameterCalculator,LaplaceParameterCalculator}
import algo.{OperationScorer,DllScorer,DllPerDsizeScorer}
import operations.{OperationCompletionType,Complete,Minimal,MaxDepth}
import main.{SaveFrequency,Best,All}

object Parser{
  def parseOperationType (op:String): Seq[String] = {
    return Seq(op.split(",")(0),op.split(",")(1))
  }

  def parseParameterCalculator (pc:String): ParameterCalculator = {
    val no = "no"
    val mEstimatorPrefix = "m"
    val laplacePrefix = "l"
    val modelCountPrefix = "mc"
    val float = "[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?"
    val mEstimator = (mEstimatorPrefix+"-(" + float + ")").r
    val laplace = (laplacePrefix+"-(" + float + ")").r
    val mc = (modelCountPrefix+"-(" + float + ")").r
    val mcMEstimator = (modelCountPrefix+"-"+mEstimatorPrefix+"-(" + float + ")").r
    val mcLaplace = (modelCountPrefix+"-"+laplacePrefix+"-(" + float + ")").r
    pc match {
      case `no` => ParameterCalculatorWithoutSmoothing
      case mEstimator(m) => new MEstimateParameterCalculator(m.toDouble)
      case laplace(m) => new LaplaceParameterCalculator(m.toDouble)
    }
  }

  def parseScorer (sc:String): OperationScorer ={
    sc match {
      case "dll" => DllScorer
      case "dll/ds" =>DllPerDsizeScorer
    }
  }

  def parseOperationCompletionType (oc:String): OperationCompletionType ={
    val complete = "complete"
    val minimal = "minimal"
    val maxDepthPrefix = "maxDepth"
    val maxDepth = (maxDepthPrefix+"-(\\d+)").r

    oc match {
      case `complete` => Complete
      case `minimal` => Minimal
      case maxDepth(k) => MaxDepth(k.toInt)
    }
  }

  def parseSavingFrequency (sf:String): SaveFrequency = {
    val allPrefix = "all"
    val bestPrefix = "best"

    val all = (allPrefix+"-(\\d+)").r
    val best = (bestPrefix+"-(\\d+)").r

    sf match {
      case all(k) => All(k.toInt)
      case best(k) => Best(k.toInt)
    }
  }
}