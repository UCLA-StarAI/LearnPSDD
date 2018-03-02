package algo

import operations._
import structure.{ConjunctionConstraint, PsddDecision, PsddElement}
import util.Util
import scala.collection.mutable

/**
  * Created by yitao on 2/9/2017
  */

abstract class OperationFinder[O <: PsddOperation, I <: Object](mgr: PsddManager, operationCompletionType: OperationCompletionType, scorer: OperationScorer, parameterCalculator: ParameterCalculator) {
  def find(node:PsddDecision, typeSpecificInput: I, root:PsddDecision): (O, Double,String)

}

class SplitOperationFinder(mgr: PsddManager, operationCompletionType: OperationCompletionType, scorer: OperationScorer, parameterCalculator: ParameterCalculator)
  extends OperationFinder[Split,PsddElement](mgr,operationCompletionType,scorer,parameterCalculator){

  def find(node:PsddDecision, splitElement:PsddElement, root: PsddDecision): (Split, Double,String) = {
    if (splitElement.data.train.isEmpty) return (null, Double.NegativeInfinity,"")

    val vars = node.vtree.left.vars.toSet
    if (vars.isEmpty) return (null, Double.NegativeInfinity,"")
    var split = ConjunctionConstraint(Map())
    var bestSplitOp = (null: Split, Double.NegativeInfinity,"")

    vars.foreach { v =>
      // split an existing split on another variable to get two splits
      val newSplits = Set(ConjunctionConstraint(split.constraint + (v->true)), ConjunctionConstraint(split.constraint + (v->false)))

      // only add this splits if it doesn't add a "False" node
      if (newSplits.forall(splitFormula =>
        !splitFormula.isImpliedBy(splitElement.formula)
          && splitFormula.isSatisfiableIn(splitElement.formula))){
        // calculate the score of adding set of splits
        val op = new Split(node, splitElement, newSplits.toArray, parameterCalculator, operationCompletionType, root, mgr)
        val res = op.update()
        val score = if (res.dSize == 0) Double.NegativeInfinity else scorer.score(res)
        if (!Util.isEqual(score, bestSplitOp._2) && score>=bestSplitOp._2){
          bestSplitOp = (op, score,"")
        }
      }
    }

    bestSplitOp
  }

  private def splitsToString(splits: Set[ConjunctionConstraint]): String = {
    splits.map(_.constraint.map{case (v,a)=>if (a) v else -v}.toArray.sorted.mkString("[",",","]")).toArray.sorted.mkString("Split(",",",")")
  }
}



class CloneOperationFinder(mgr: PsddManager, operationCompletionType: OperationCompletionType, scorer: OperationScorer, parameterCalculator: ParameterCalculator, maxNumberOfCloneParents:Int)
  extends OperationFinder[Clone, Array[PsddElement]](mgr,operationCompletionType,scorer,parameterCalculator){

  def find(node:PsddDecision, parents: Array[PsddElement], root: PsddDecision): (Clone, Double,String) = {
    if (parents.length<=1 || node.deterministic || node.elements.forall(_.data.train.isEmpty)) {
      (null, Double.NegativeInfinity,"")
    }else {
      val maxSetSize = if (parents.size>maxNumberOfCloneParents) maxNumberOfCloneParents else parents.length-1
      val cloneOps = (1 to maxSetSize).flatMap(setSize => parents.toSet.subsets(setSize)).map(parentsToRedirect =>
        Clone(node, parents, parentsToRedirect, parameterCalculator, operationCompletionType, root, mgr)).map { op =>
        val res = op.update()
        val score = if (res.dSize==0) Double.NegativeInfinity else scorer.score(res)
        (op, score,parentsToString(op.parentsToRedirect))
      }
      val bestClone = if (!cloneOps.isEmpty) cloneOps.maxBy(x=>(x._2,x._3)) else (null, Double.NegativeInfinity,"")
      bestClone
    }
  }

  private def parentsToString(parents: Set[PsddElement]): String = {
    parents.map(_.index).toArray.sorted.mkString("Clone(",",",")")
  }
}

