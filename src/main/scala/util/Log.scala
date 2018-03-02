package util

/**
  * Helper class for logarithmic operatinos
  *
  * Created by jessa on 3/28/16.
  */
object Log {

  def multiply(numbers: Double*) = numbers.sum
  def divide(logA: Double, logB: Double) = logA-logB

  def add(logA: Double, logB: Double) = {
    if (logA==Double.NegativeInfinity)
      logB
    else if (logB==Double.NegativeInfinity)
      logA
    else
      logA+Math.log(1+Math.exp(logB-logA))
  }

  def oneMinus(logA: Double): Double = Math.log(1-Math.exp(logA))

  val one = 0.0
  val zero = Double.NegativeInfinity


}
