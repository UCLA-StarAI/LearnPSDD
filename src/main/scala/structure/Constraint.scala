package structure

import sdd.{SddManager, WmcManager, Sdd}
import util.Log
import scala.collection.JavaConverters._

/**
  *
  * A constraint is a formula that you can apply to a PSDD. A set of constraints is used to split a node on. (e.g. X and not X)
  *
 * Created by jessa on 8/2/16.
 *
 */
abstract class Constraint {
  def isDeterministic(formula: Sdd, vars: Array[Int]): Boolean

  def vars: Iterable[Int]

  def restrict(formula: Sdd): Sdd


  def project(vars: Array[Int]): Constraint

  def isSatisfiedBy(assignment: Map[Int,Boolean]): Boolean

  def isSatisfiableIn(formula: Sdd): Boolean
  def isImpliedBy(formula: Sdd): Boolean

  def MC(formula: Sdd): Double

  var f: Sdd = null
  protected def calculateSdd(mgr: SddManager): Sdd
  def sdd(mgr: SddManager): Sdd = {
    if (f == null) f = calculateSdd(mgr)
    f
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Constraint]

  override def equals(other: Any) = other match{
    case other: Constraint => other.canEqual(this)
    case _ => false
  }

  def getConstraint:Array[(Int,Boolean)]

}

case class ConjunctionConstraint(constraint: Map[Int,Boolean]) extends Constraint {
  def restrict(formula: Sdd): Sdd = constraint.foldLeft(formula){case (alpha,(v,a)) => alpha.conjoin(new Sdd(if (a) v.toLong else -v.toLong, alpha.getManager))}

  def vars = constraint.keys

  override def project(vars: Array[Int]): Constraint = {
    val newConstraints = constraint.filterKeys(vars.contains)
    if (newConstraints.isEmpty) NoConstraint
    else new ConjunctionConstraint(newConstraints)
  }

  override def isSatisfiedBy(assignment: Map[Int, Boolean]): Boolean = constraint.forall{case (v,a) => assignment(v)==a}

  override protected def calculateSdd(mgr: SddManager): Sdd = constraint.map{case (v,a) => new Sdd(if (a) v else -v, mgr)}.reduce{_.conjoin(_)}

  def isSatisfiableIn(formula: Sdd): Boolean = {
    MC(formula)>Log.zero
  }

  def isImpliedBy(formula: Sdd): Boolean = {
    MC(formula) == NoConstraint.MC(formula)
  }

  def isDeterministic(formula: Sdd, vars: Array[Int]): Boolean = {
    val constraintVars = this.vars.toSet
    val neededVars =  vars.filterNot(constraintVars.contains)
    val fVars = formula.getUsedVariables
    formula.ModelCount()==1 && neededVars.forall(v=>fVars.contains(v.toLong))
  }

  def MC(formula: Sdd): Double = {

    val wmc = new WmcManager(formula, true)
    constraint.foreach { case (v, a) => wmc.setLiteralWeight(if (a) -v else v, wmc.getZeroWeight) }
    val mc = wmc.propagate()
    wmc.free()

    mc
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[ConjunctionConstraint]

  override def equals(other: Any): Boolean = other match {
    case other: ConjunctionConstraint => other.canEqual(this) && this.constraint == other.constraint
    case _ => false
  }

  override def getConstraint:Array[(Int,Boolean)] = this.constraint.toArray

  override def hashCode: Int = constraint.hashCode()

  override def toString: String = constraint.map{case (v,a) => if (a) v else -v}.mkString("C(",",",")")
}

case class SddConstraint(protected val constraint: Sdd) extends Constraint {
  def restrict(formula: Sdd): Sdd = formula.conjoin(constraint)

  def vars = constraint.getUsedVariables.asScala.map(_.toInt)

  override def project(vars: Array[Int]): SddConstraint = new SddConstraint(constraint.project(vars.map(_.toLong)))

  override def isSatisfiedBy(assignment: Map[Int, Boolean]): Boolean = {
    val wmc = new WmcManager(constraint, true)
    assignment.foreach{case (v,a)=> wmc.setLiteralWeight(if (a) -v else v, wmc.getZeroWeight)}

    val result = wmc.propagate() > wmc.getZeroWeight
    wmc.free()
    result
  }


  def isSatisfiableIn(formula: Sdd): Boolean = {
    !formula.conjoin(constraint).isFalse
  }

  def isImpliedBy(formula: Sdd): Boolean = {
    formula.conjoin(constraint) == formula
  }

  def isDeterministic(formula: Sdd, vars: Array[Int]): Boolean = {
    val f = formula.conjoin(constraint)
    val fVars = f.getUsedVariables
    f.ModelCount()==1 && vars.forall(v=>fVars.contains(v.toLong))
  }

  def MC(formula: Sdd): Double = {
    val sdd = formula.conjoin(constraint)
    if (sdd.isTrue) math.log(2)*sdd.getManager.getVarCount
    else {
      val wmc = new WmcManager(sdd, true)
      val mc = wmc.propagate()
      wmc.free()
      mc
    }
  }


  override protected def calculateSdd(mgr: SddManager): Sdd = constraint

  override def canEqual(other: Any): Boolean = other.isInstanceOf[SddConstraint]

  override def equals(other: Any): Boolean = other match {
    case other: SddConstraint => other.canEqual(this) && this.constraint == other.constraint
    case _ => false
  }

  override def getConstraint:Array[(Int,Boolean)] = Map[Int,Boolean]().toArray

  override def hashCode: Int = constraint.getId.hashCode()

  override def toString: String = "(C"+constraint.getId+")"
}


case object NoConstraint extends Constraint {
  def restrict(formula: Sdd): Sdd = formula

  def vars = Iterable.empty[Int]

  override def project(vars: Array[Int]): Constraint = NoConstraint

  def isDeterministic(formula: Sdd, vars: Array[Int]): Boolean = {
    val fVars = formula.getUsedVariables
    formula.ModelCount()==1 && vars.forall(v=>fVars.contains(v.toLong))
  }

  override def MC(formula: Sdd): Double = {
    if (formula.isTrue) math.log(2)* formula.getManager.getVarCount
    else {
      val wmc = new WmcManager(formula, true)
      val mc = wmc.propagate()
      wmc.free()
      mc
    }
  }
  override def isSatisfiableIn(formula: Sdd): Boolean = true

  override def isImpliedBy(formula: Sdd): Boolean = true

  override protected def calculateSdd(mgr: SddManager) = new Sdd(true, mgr)

  override def isSatisfiedBy(assignment: Map[Int, Boolean]): Boolean = true

  override def getConstraint:Array[(Int,Boolean)] = Map[Int,Boolean]().toArray

  override def toString: String = "C(/)"
}