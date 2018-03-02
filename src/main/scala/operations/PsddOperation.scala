package operations

import algo.ParameterCalculator
import structure.{Constraint, PsddElement, PsddDecision}
import scala.collection.mutable.ArrayBuffer

/**
  * This class represents an operation that can be done on a PSDD (split, clone, maybe merge in the future).
  *
  * The operation can be executed or simulated
  * The class stores the log likelihood gain, increase in number of edges and the set of nodes that are changed by this
  * operation.
  *
  * Created by jessa on 9/7/16.
  * Modified by Yitao on 2/9/17
  */
abstract class PsddOperation(parameterCalculator: ParameterCalculator, completionType: OperationCompletionType, mgr: PsddManager, root:PsddDecision) {

  def execute(): Boolean

  protected def simulate(): SimulationResult

  def update(): SimulationResult = {
    val res = simulate()
    dll = res.dll
    dSize = res.dSize
    res
  }

  var dll: Double = 0.0
  var dSize: Int = 0
  var changedNodes: Set[Int] = Set.empty[Int]

}

/**
  * The result of simulating or executing an operation
  */
trait Result{
  def dll: Double
  def dSize: Int
}

/**
  *
  * @param dll
  * @param dSize
  * @param changedNodes
  */
case class SimulationResult(dll: Double, dSize: Int) extends Result

/**
  *
  * @param dll
  * @param validDll
  * @param testDll
  * @param dSize
  * @param changedNodes
  * @param newNodes
  */

case class Split(splitNode: PsddDecision, splitElement: PsddElement, splitFormulas: Array[Constraint], parameterCalculator: ParameterCalculator, completionType: OperationCompletionType, root: PsddDecision, mgr: PsddManager) extends PsddOperation(parameterCalculator, completionType, mgr, root) {
  val subVars = splitNode.vtree.right.vars
  assert(splitFormulas.forall(!_.vars.exists(subVars.contains)),"split on sub")

  override def simulate(): SimulationResult = mgr.simulateSplit(splitNode, splitElement, parameterCalculator, splitFormulas, completionType, root)

  override def execute(): Boolean  = {
    val (result,indices) = mgr.executeSplit(splitNode, splitElement, parameterCalculator, splitFormulas, completionType, root)
    afterSplitElementsIndices++=indices
    result
  }

  override def toString: String = "Split("+splitNode.index+","+splitNode.vtree.index+","+splitElement.index+","+splitFormulas.mkString("[",",","]")+","+completionType+","+afterSplitElementsIndices.mkString(",")+")"

  override def equals(other: Any): Boolean = other match{
    case other: Split =>
      this.splitNode == other.splitNode &&
        this.splitElement.prime == other.splitElement.prime &&
        this.splitFormulas.toSet==other.splitFormulas.toSet

    case _ => false
  }

  val afterSplitElementsIndices = ArrayBuffer[Int]()

  override def hashCode: Int = (splitNode.index, splitElement.index, splitFormulas).hashCode()
}

case class Clone(node: PsddDecision, originalParents:Array[PsddElement], parentsToRedirect: Set[PsddElement], parameterCalculator: ParameterCalculator, completionType: OperationCompletionType, root: PsddDecision, mgr: PsddManager) extends PsddOperation(parameterCalculator, completionType, mgr, root) {

  override def simulate(): SimulationResult = mgr.simulateClone(node, parentsToRedirect, parameterCalculator, completionType, root)

  override def execute(): Boolean = {
    val (result,index) = mgr.executeClone(node,originalParents,parentsToRedirect, parameterCalculator, completionType, root)
    afterClonedNodeIndex = index
    result
  }

  override def toString: String = "Clone("+node.index+","+node.vtree.index+","+parentsToRedirect.map(_.index).mkString("{",",","}")+","+completionType+","+afterClonedNodeIndex+")"

  override def equals(other: Any): Boolean = other match{
    case other: Clone =>
      this.node == other.node &&
      this.originalParents.toSet == other.originalParents.toSet &&
      this.parentsToRedirect == other.parentsToRedirect
    case _ => false
  }

  var afterClonedNodeIndex = -1

  override def hashCode: Int = (node.index, parentsToRedirect.map(_.index)).hashCode()
}
