package structure

import sdd.Sdd


/**
  * This class stores a PSDD element which is  a triple of the prime, sub and parameter. Additionally it stores
  * the data that passes through this edge and the formula of the element.
  *
  * @param prime
  * @param sub
  * @param data
  * @param formula
  * @param theta
  */
case class PsddElement(val index: Int, var prime: PsddNode, var sub: PsddNode, var data: DataSets, var formula: Sdd, var theta: Double, var constraints:Constraint = NoConstraint) {

  override def hashCode = index.hashCode()

  def instanceEqual(other: Any): Boolean = other.isInstanceOf[PsddElement]

  override def equals(other: Any) = other match {
    case other: PsddElement => other.instanceEqual(this) && {
      this.prime == other.prime &&
      this.sub == other.sub &&
      this.data == other.data
    }
    case _ => false
  }

  def equal(that: PsddElement): Boolean = this.index==that.index

  override def toString: String = (prime.index, sub.index, data.train.size, data.valid.size, data.test.size, formula, theta, index).toString()
}
