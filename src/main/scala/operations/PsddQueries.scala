package operations

import java.io.{File, PrintWriter}

import sdd.Sdd
import structure._
import util.{Util, Log}

import scala.collection.mutable
import BigDecimal._
import spire.math._

/**
 * Created by jessa on 8/9/16.
 * Modified by yitao on 1/29/17.
 */
object PsddQueries {


  def parentsBeforeChildren(root: PsddNode): mutable.ArrayBuffer[PsddNode]= {
    val queue = new mutable.ArrayBuffer[PsddNode]
    queue += root
    root.flag=true
    var i = 0
    var truePsdd: PsddNode = null
    while (i < queue.size){
      val node = queue(i)
      node.elements.foreach { el =>
        if (!el.prime.flag) {
          queue += el.prime
          el.prime.flag = true
        }
        if (!el.sub.flag) {
          if (el.sub.isInstanceOf[PsddTrue]) {
            truePsdd = el.sub
          }else {
            queue += el.sub
            el.sub.flag = true
          }
        }
      }
      i+=1
    }
    queue.foreach(_.flag=false)
    queue += truePsdd
    queue
  }

  def decisionParentsBeforeChildren(root: PsddNode, maxLevel: Int = Int.MaxValue): mutable.ArrayBuffer[PsddDecision]= {
    val queue = mutable.ArrayBuffer[PsddDecision]()
    queue += root.asInstanceOf[PsddDecision]
    root.flag = true
    var i = 0
    while (i < queue.size){
      val node = queue(i)
      if (node.vtree.level < maxLevel) {
        node.elements.foreach { el =>
          el.prime match {
            case child: PsddDecision =>
              if (!child.flag) {
                queue += child
                child.flag = true
              }
            case _ =>
          }
          el.sub match {
            case child: PsddDecision =>
              if (!child.flag) {
                queue += child
                child.flag = true
              }
            case _ =>
          }
        }
      }
      i+=1
    }

    queue.foreach(_.flag=false)
    queue
  }

  def decisionNodesWithParents(root: PsddNode, selectCrit: PsddDecision => Boolean = _=>true): mutable.Map[PsddDecision,Array[PsddElement]] = {
    val nodes = mutable.Map[PsddDecision, Array[PsddElement]]()
    val queue = mutable.ArrayBuffer[PsddDecision]()
    root match {
      case root:PsddDecision =>
        if (selectCrit(root)) {nodes += root->Array.empty}
        queue += root
        root.flag=true
        var i = 0
        while (i < queue.size){
          val node = queue(i)
          node.elements.foreach { el =>
            el.prime match {
              case child: PsddDecision =>
                if (!child.flag) {
                  queue += child
                  child.flag = true
                }
                if (selectCrit(child)) {nodes(child) = nodes.getOrElse(child, Array.empty) :+ el}
              case _ =>
            }
            el.sub match {
              case child: PsddDecision =>
                if (!child.flag) {
                  queue += child
                  child.flag = true
                }
                if (selectCrit(child)) {nodes(child) = nodes.getOrElse(child, Array.empty) :+ el}
              case _ =>
            }
          }
          i+=1
        }
    }
    queue.foreach(_.flag=false)
    nodes
  }

  def nodes(root: PsddNode) = parentsBeforeChildren(root)
  def decisionNodes(root: PsddNode) = decisionParentsBeforeChildren(root)

  def childrenBeforeParents(root: PsddNode) = parentsBeforeChildren(root).reverse
  def decisionChildrenBeforeParents(root: PsddNode) = decisionParentsBeforeChildren(root).reverse

  def elements(root: PsddNode) =  {
    val decisionNodes = decisionParentsBeforeChildren(root)
    decisionNodes.flatMap(_.elements)
  }

  def size(psdd: PsddDecision) = elements(psdd).size

  def logLikelihood(psdd: PsddNode, dataSet: String): Double = dataSet match {
    case "train" => elements(psdd).map(el => el.theta*el.data.train.total).sum
    case "test" => elements(psdd).map(el => el.theta*el.data.test.total).sum
    case "valid" => elements(psdd).map(el => el.theta*el.data.valid.total).sum
  }

  def entropy(root: PsddNode): Double = {
    val childrenBeforeParentsNodes =childrenBeforeParents(root)
    childrenBeforeParentsNodes.foreach(node => node.baggage = node.elements.map{el=> el.theta - el.prime.baggage.asInstanceOf[Double] - el.sub.baggage.asInstanceOf[Double]})
    val ent = root.baggage.asInstanceOf[Double]
    childrenBeforeParentsNodes.foreach(_.baggage=null)
    ent
  }

  def logProb(root: PsddNode, assignment: Map[Int,Boolean]): Double = {
    val childrenBeforeParentsNodes = childrenBeforeParents(root)
    childrenBeforeParentsNodes.foreach{node =>
      node.baggage = node match {
        case node: PsddLiteral => if (assignment.getOrElse(node.v,node.pos)==node.pos) Log.one else Log.zero
        case node: PsddTrue => Log.one
        case node: PsddDecision => node.elements.map(el=>Log.multiply(el.prime.baggage.asInstanceOf[Double],el.sub.baggage.asInstanceOf[Double],el.theta)).reduce(Log.add)
      }
    }
    val prob = root.baggage.asInstanceOf[Double]
    childrenBeforeParentsNodes.foreach(_.baggage=null)
    prob
  }

  def bigDecimalProb(root: PsddNode, assignment: Map[Int,Boolean]): BigDecimal = {
    val logProbability = logProb(root,assignment)
    spire.math.pow(BigDecimal.decimal(math.E),BigDecimal.decimal(logProbability))
  }

  /*def isValidDecisionNode(node: PsddDecision): Boolean = {
    // needs elements
    if (node.elements.isEmpty){
      println("Decision node "+node.index+" has no elements!")
      false
    }
    //data
    //toDo: how to check whether data is distributed correctly
    else if (!node.elements.forall{el =>
      /*val primeOk = el.prime match {
        case child: PsddDecision =>
          val ok = el.data.checkOperation(child.data) && el.data.diff(child.data).isEmpty
          if (!ok) {
            println(el.data.train.size, child.data.train.size)
            println(el.data.intersect(child.data), el.data)
            println("Child "+child.index+" of element with prime "+el.prime.index+" of node "+node.index+" does not contain all the data of the element")
          }
          ok
        case _ => true
      }
      val subOk = el.sub match {
        case child: PsddDecision =>
          val ok = el.data.checkOperation(child.data) && el.data.diff(child.data).isEmpty
          if (!ok) {
            println(el.data.train.size, child.data.train.size)
            println(el.data.intersect(child.data), el.data)
            println("Child "+child.index+" of element with prime "+el.prime.index+" of node "+node.index+" does not contain all the data of the element")
          }
          ok
        case _ => true
      }*/
      val primeOk = true
      val subOk = true
      primeOk & subOk
    }){
      false
    }
    // parameters
    else if(!Util.isEqual(node.elements.toList.map(_.theta).reduce(Log.add), Log.one)){
      println("Parameters of node "+node.index+" do not sum up to one")
      println(node.elements.toList.map(_.theta).mkString("{",",","}"))
      false
    }

    //formulas
    else if (node.elements.map(_.formula).reduce(_.disjoin(_))!=node.formula){
      println("The formula of node "+node.index+" is not equal to the disjunction of the formulas of its elements.")
      println("Node formula: "+node.formula.getVisualization)
      println("Element formulas:")
      node.elements.foreach(el => println("\t"+el.formula.getVisualization))
      false
    }
    else if (!node.elements.forall{el=>
      el.prime match {
        case p: PsddDecision=> val ok = p.formula.conjoin (el.sub.formula) == el.formula
          if (! ok) {
            println ("The formula of element with prime " + el.prime.index + " of node " + node.index + " is not the conjunction of the formula of the prime and sub.")
            println ("Prime formula: " + el.prime.formula.getVisualization)
            println ("Sub formula: " + el.sub.formula.getVisualization)
            println ("Element formula: " + el.formula.getVisualization)
            println ("Element projected on prime: " + el.formula.project (el.prime.vtree.vars.map (_.toLong) ).getVisualization)
            println ("Element projected on sub: " + el.formula.project (el.sub.vtree.vars.map (_.toLong) ).getVisualization)
          }
          ok
        case p: PsddLiteral => true
        case p: PsddTrue => true
      }
    }){
      false
    }
    //mutually exclusive
    else if(!node.elements.toArray.toSet.subsets(2).map(_.toArray).forall(els => els(0).formula.conjoin(els(1).formula).isFalse)){
      println("Elements in node "+node.index+" are not mutually exclusive.")
      node.elements.foreach(el =>println(el.formula.getVisualization))
      false
    }
    else true

  }

  def isValid(root: PsddNode):Boolean = {
    setContextsAsBaggage(root)
    val psddNodes = nodes(root)
    val valid = psddNodes.forall{
      case node: PsddDecision =>

        isValidDecisionNode(node) && {

        // context

          val nodeContext = node.baggage.asInstanceOf[Sdd]
          node.elements.forall{el =>
            val selectionFormula = el.formula.conjoin(nodeContext)
            val selectedData = root.asInstanceOf[PsddDecision].data.filter(assignment => ConjunctionConstraint(assignment).MC(selectionFormula)>Log.zero)
            val ok = el.data == selectedData
            if (!ok){
              println("Data in element with prime "+el.prime.index+" of node "+node.index+" is not correct. "+el.data+" <=> "+selectedData)
              println("Selection formula: "+selectionFormula.getVisualization)
              println("Root data "+root.asInstanceOf[PsddDecision].data)
            }
            ok
          }
        }


      case _ => true
    }

    psddNodes.foreach(_.baggage=null)
    valid
  }*/

}
