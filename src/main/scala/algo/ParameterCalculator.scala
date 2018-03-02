package algo

import structure.Data
import util.{Util, Log}

/**
  * This class calculates the parameter of an element in log space.
  *
  * Created by jessa on 7/28/16.
  *
 **/
abstract class ParameterCalculator {
  def calculate(elementData: Data, nodeData: Data, nbOfElements: Int): Double
}

object ParameterCalculatorWithoutSmoothing extends ParameterCalculator {

  def calculate(elementData: Data, nodeData: Data, nbOfElements: Int): Double = if (nbOfElements == 1) 0.0 else math.log(elementData.total/nodeData.total)

  override def toString: String = "no"
}

class LaplaceParameterCalculator(m: Double) extends ParameterCalculator {
  def calculate(elementData: Data, nodeData: Data, nbOfElements: Int): Double = if (nbOfElements == 1) 0.0 else math.log((elementData.total + m)/(nodeData.total + m*nbOfElements))

  override def toString: String = "l-"+m
}

class MEstimateParameterCalculator(m: Double) extends ParameterCalculator {
  def calculate(elementData: Data, nodeData: Data, nbOfElements: Int): Double = if (nbOfElements == 1) 0.0 else math.log((elementData.total + m/nbOfElements)/(nodeData.total + m))

  override def toString: String = "m-"+m
}
