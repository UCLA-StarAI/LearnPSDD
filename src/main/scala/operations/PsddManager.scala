package operations

import java.io.File

import algo.ParameterCalculator
import main.{Main, Output}
import sdd.{Sdd, SddManager}
import structure._
import util.{Util, Log, UniqueMap}

import scala.collection.mutable
import scala.io.Source

/**
  * Created by Yitao on 2/17/17

  * This class is responsible for low level transformations on PSDDs.
  * PSDDs can be read in from file (PSDD or SDD format) or a simple PSDD that represents independent variables can be generated for any vtree
  *
  * When build initial the initial psdd, we only keep unique psdds, considering two PSDDs equal that have the same structure and data.
  *
  */
class PsddManager(sddManager: SddManager) {

  val trueSdd = new Sdd(true, sddManager)
  val trueNode = new PsddTrue(0, trueSdd)

  val roots = mutable.Set[PsddNode]()

  var nodeCount = 0 // 0 is true node
  var elementCount = 0

  private def nextNodeIndex = {
    nodeCount += 1
    nodeCount
  }

  private def nextElementIndex = {
    elementCount +=1
    elementCount
  }


  ////////////////
  // OPERATIONS //
  ////////////////

  val minDll = 0.0000001

  ///////////////////////
  /// Minimal Split /////
  ///////////////////////

  /**
    * Execute a split
    * @param splitNode
    * @param splitElement
    * @param parameterCalculator
    * @param splitFormulas  //splitFormulas have two constraints: one constraint to specify some f == true, the other to specify the same f == false
    * @param operationCompletionType
    * @param root
    * @return
    */
  def executeSplit(splitNode: PsddDecision, splitElement: PsddElement, parameterCalculator: ParameterCalculator, splitFormulas: Array[Constraint], operationCompletionType: OperationCompletionType, root:PsddDecision): (Boolean,mutable.ArrayBuffer[Int]) = {
    if (!splitElement.prime.isInstanceOf[PsddDecision]) return (false,mutable.ArrayBuffer[Int]())
    if (!splitFormulas.forall(f=>f.isSatisfiableIn(splitElement.formula))) return (false,mutable.ArrayBuffer[Int]())
    assert (splitNode.elements.contains(splitElement),"execution: element not in node\n"+splitElement+"\n"+splitNode.elements.mkString("\n"))

    ///////////////////////////
    // execute minimal split //
    ///////////////////////////
    val nodeTrainData = splitNode.trainData

    // remove old element
    splitNode.elements.remove(splitNode.elements.indexWhere(x=>x.index==splitElement.index))
    val newNbElements = splitNode.elements.size + splitFormulas.length

    /////////////////////////
    //// execute clones /////
    // following the split //
    /////////////////////////

    val afterSplitElementsIndices = mutable.ArrayBuffer[Int]()
    val newElements = splitFormulas.map{f =>
      operationCompletionType match {
        case Minimal => cloneElementAfterSplit (splitElement, f, splitElement.data.filter (f.isSatisfiedBy), parameterCalculator, newNbElements, nodeTrainData, 0)
        case Complete => cloneElementAfterSplit (splitElement, f, splitElement.data.filter (f.isSatisfiedBy), parameterCalculator, newNbElements, nodeTrainData, Int.MaxValue)
        case MaxDepth(k) => cloneElementAfterSplit (splitElement, f, splitElement.data.filter (f.isSatisfiedBy), parameterCalculator, newNbElements, nodeTrainData, k)
      }
    }
    (newElements,splitFormulas).zipped.map{case(el,f) => el.constraints = addConstraint(el.constraints,f)}
    newElements.foreach(splitNode.elements+=_)
    afterSplitElementsIndices++=newElements.map(_.index)
    //require(PsddQueries.isValidDecisionNode(splitNode), "Somehow the split is invalid\n")
    (true,afterSplitElementsIndices)
  }

  private def cloneElementAfterSplit(elementToClone:PsddElement, constraint:Constraint, data:DataSets, parameterCalculator:ParameterCalculator, nbElements:Int, nodeTrainData:Data,operationDepth:Int):PsddElement = {
    if (operationDepth<=0) {
      minCloneElementAfterSplit(elementToClone, constraint, data, parameterCalculator, nbElements, nodeTrainData,operationDepth)
    }else{
      completeCloneElementAfterSplit(elementToClone,constraint,data,parameterCalculator,nbElements,nodeTrainData,operationDepth-1)
    }
  }

  private def minCloneElementAfterSplit(elementToClone:PsddElement, constraint:Constraint, data:DataSets, parameterCalculator:ParameterCalculator, nbElements:Int, nodeTrainData:Data,operationDepth:Int):PsddElement = {
    val splitVar = constraint.vars.head
    if (elementToClone.prime.vtree.vars.toSet.contains(splitVar)) {
      val newPrime = cloneDecisionNodeAfterSplit(elementToClone.prime,constraint,data,parameterCalculator,operationDepth)
      PsddElement(nextElementIndex,newPrime,elementToClone.sub,data,constraint.restrict(elementToClone.formula),parameterCalculator.calculate(data.train,nodeTrainData,nbElements),elementToClone.constraints)
    }else{
      val newSub = cloneDecisionNodeAfterSplit(elementToClone.sub,constraint,data,parameterCalculator,operationDepth)
      PsddElement(nextElementIndex,elementToClone.prime,newSub,data,constraint.restrict(elementToClone.formula),parameterCalculator.calculate(data.train,nodeTrainData,nbElements),elementToClone.constraints)
    }
  }

  private def completeCloneElementAfterSplit(elementToClone:PsddElement, constraint:Constraint, data:DataSets, parameterCalculator:ParameterCalculator, nbElements:Int, nodeTrainData:Data,operationDepth:Int):PsddElement = {
    val newPrime = cloneDecisionNodeAfterSplit(elementToClone.prime,constraint,data,parameterCalculator,operationDepth)
    val newSub = cloneDecisionNodeAfterSplit(elementToClone.sub,constraint,data,parameterCalculator,operationDepth)
    PsddElement(nextElementIndex,newPrime,newSub,data,constraint.restrict(elementToClone.formula),parameterCalculator.calculate(data.train,nodeTrainData,nbElements),elementToClone.constraints)
  }


  private def cloneDecisionNodeAfterSplit(nodeToClone:PsddNode, constraint:Constraint, data:DataSets, parameterCalculator:ParameterCalculator,operationDepth:Int): PsddNode = {
    val clonedNode:PsddNode = nodeToClone match {
      case node: PsddDecision =>
        val elementsNeedToBeCloned = node.elements.filter(el=>constraint.isSatisfiableIn(el.formula))
        val nbElements = elementsNeedToBeCloned.size
        val newElements = elementsNeedToBeCloned.map(el=>cloneElementAfterSplit(el,constraint,data.filter(el.constraints.isSatisfiedBy),parameterCalculator,nbElements,data.train,operationDepth))
        PsddDecision(nextNodeIndex,node.vtree,newElements,constraint.restrict(nodeToClone.formula))
      case node: PsddLiteral => node
      case node: PsddTrue => node
    }
    clonedNode
  }

  private def addConstraint(a:Constraint,b:Constraint): Constraint = {
    (a,b) match {
      case (aa:ConjunctionConstraint,bb:ConjunctionConstraint) => ConjunctionConstraint((aa.getConstraint++bb.getConstraint).toMap)
      case (aa:ConjunctionConstraint,_) => ConjunctionConstraint(aa.getConstraint.toMap)
      case (_,bb:ConjunctionConstraint) => ConjunctionConstraint(bb.getConstraint.toMap)
      case _ => NoConstraint
    }
  }

  ///////////////
  /// Clone /////
  ///////////////
  /**
    * execute a clone
    * @param cloneNode
    * @param parentsToRedirect
    * @param parameterCalculator
    * @param operationCompletionType
    * @param root
    * @return
    */
  def executeClone(nodeToClone:PsddDecision,originalParents:Array[PsddElement], parentsToRedirect:Set[PsddElement], parameterCalculator:ParameterCalculator, operationCompletionType: OperationCompletionType, root:PsddDecision):(Boolean,Int) = {
    operationCompletionType match{
      case Minimal => executeMinClone(nodeToClone,originalParents,parentsToRedirect,parameterCalculator,root)
      case Complete => executeMinClone(nodeToClone,originalParents,parentsToRedirect,parameterCalculator,root)
    }
  }

  private def executeMinClone(nodeToClone:PsddDecision,originalParents:Array[PsddElement], parentsToRedirect:Set[PsddElement], parameterCalculator:ParameterCalculator,  root:PsddDecision):(Boolean,Int) = {
    //calculate remaining parents for nodeToClone
    val remainingParents = originalParents.filter(p=>parentsToRedirect.forall(_.index!=p.index))

    //execute minimal clones to nodeToClone
    val formulaForClonedNode = parentsToRedirect.map(p=>p.formula).reduce(_.disjoin(_))
    val clonedElements = nodeToClone.elements.map{el=>PsddElement(nextElementIndex,el.prime, el.sub, el.data, el.constraints.restrict(formulaForClonedNode), el.theta, el.constraints)}
    val clonedNode = PsddDecision(nextNodeIndex,nodeToClone.vtree,clonedElements,formulaForClonedNode)

    //redirect parents
    parentsToRedirect.foreach{el=>
      require(el.prime.index==nodeToClone.index || el.sub.index==nodeToClone.index)
      if (el.prime.index==nodeToClone.index) {
        el.prime = clonedNode
      }else {
        el.sub = clonedNode
      }
      //require(PsddQueries.isValidDecisionNode(node),"Redirected parent nodes somehow are not valid\n")
    }

    //recalculate formula for nodeToClone
    nodeToClone.formula = remainingParents.map(p=>p.formula).reduce(_.disjoin(_))
    nodeToClone.elements.foreach{el=>el.formula=el.constraints.restrict(nodeToClone.formula)}

    //recalculate parameters
    val dataForNodeToClone = remainingParents.map(p=>p.data).reduce(_.union(_))
    val dataForClonedNode = parentsToRedirect.map(p=>p.data).reduce(_.union(_))
    nodeToClone.elements.foreach{el=>
      el.data =  dataForNodeToClone.filter(el.constraints.isSatisfiedBy)
      el.theta = parameterCalculator.calculate(el.data.train,dataForNodeToClone.train,nodeToClone.elements.size)
    }
    clonedNode.elements.foreach{el=>
      el.data =  dataForClonedNode.filter(el.constraints.isSatisfiedBy)
      el.theta = parameterCalculator.calculate(el.data.train,dataForClonedNode.train,clonedNode.elements.size)
    }

    (true,clonedNode.index)
  }


  ///////////////////
  // Simulations ///
  //////////////////


  ////////////////////////
  //// Split Simulation //
  ////////////////////////

  /**
    * Simulate a split
    *
    * Precondition: valid split! Every splitFormula should be satisfiable and not implied
    * @param splitNode
    * @param splitElement
    * @param parameterCalculator
    * @param splitFormulas
    * @param operationCompletionType
    * @return
    */
  def simulateSplit(splitNode: PsddDecision, splitElement: PsddElement, parameterCalculator: ParameterCalculator, splitFormulas: Array[Constraint], operationCompletionType: OperationCompletionType, root:PsddDecision): SimulationResult = {
    require (splitFormulas.forall(constraint => constraint.isSatisfiableIn(splitElement.formula)),{
      "All the split formulas need to be satisfiable.\n"+
        "node: "+splitNode.index+", element: "+(splitElement.prime.index, splitElement.sub.index)+"\n"+
        splitFormulas.filterNot(constraint=>constraint.isSatisfiableIn(splitElement.formula)).map("constraint: "+_.toString).mkString("\n")
    })

    if (!splitElement.prime.isInstanceOf[PsddDecision]) return SimulationResult(0.0,0)

    assert (splitNode.elements.contains(splitElement),"simulation: element not in node")
    val dataFilters = splitFormulas.map(f => splitElement.data.train.filter(f.isSatisfiedBy))

    ///////////////////
    //simulate split///
    ///////////////////

    //val oldElements = splitNode.elements
    val oldDllSplit = splitNode.elements.toArray.map(el => el.data.train.total*el.theta).sum
    val newNbElements = splitNode.elements.size + splitFormulas.length -1
    val newDllSplitNewEls = splitFormulas.zip(dataFilters).map{case (c,data) =>
      data.total*parameterCalculator.calculate(data,splitNode.trainData, newNbElements)
    }.sum
    splitNode.elements.remove(splitNode.elements.indexWhere(x=>x.index==splitElement.index))
    val newDllSplitOtherEls = splitNode.elements.map(el => el.data.train.total* parameterCalculator.calculate(el.data.train, splitNode.data.train, newNbElements)).sum

    val splitDll = newDllSplitNewEls-oldDllSplit+newDllSplitOtherEls


    //simulate clone
    val cloneResult = simulateConstrainedClone(Array(splitElement.prime.asInstanceOf[PsddDecision],splitElement.sub.asInstanceOf[PsddDecision]), parameterCalculator, dataFilters, splitFormulas,
      updateCompletionTypeAfterSplit(splitFormulas, operationCompletionType), -splitDll+minDll,root)
    splitNode.elements += splitElement

    SimulationResult(cloneResult.dll + splitDll, cloneResult.dSize + splitFormulas.length - 1)
  }

  private def updateCompletionTypeAfterSplit(splitFormulas: Array[Constraint], operationCompletionType: OperationCompletionType): OperationCompletionType = {
    val updatedCompletionType = operationCompletionType match {
      case Complete => Complete
      case Minimal => Minimal
      case MaxDepth(k) => MaxDepth(k - 1)
    }
    updatedCompletionType
  }


  /**
    * This method sets the clone specifications for a (multi) clone with maximum depth.
    * A clone specification is an object on a node that specifies how it should be cloned. This specifies for which
    * clones this specific node will be cloned. Because of the clone formula, not all nodes may need to be cloned.
    *
    * @param roots
    * @param dataFilters
    * @param cloneFormulas
    * @param maxDepth
    * @param withParents if this variable is true, the clone specification also keeps the parents of this node for each clone
    */
  private def setMaxDepthCloneSpecifications(roots: Array[PsddDecision], dataFilters: Array[Data], cloneFormulas: Array[Constraint], maxDepth: Int, withParents: Boolean): Unit = {
    assert(roots.forall(root => PsddQueries.decisionNodes(root).forall(_.cloneSpec==null)))
    val nbClones = dataFilters.length

    assert(nbClones == cloneFormulas.length)
    assert(roots.forall(_.vtree.level==roots.head.vtree.level))

    val maxLevel = if (roots.head.vtree.level + maxDepth <0) Int.MaxValue else roots.head.vtree.level + maxDepth

    val queue = mutable.Queue[PsddDecision]()
    roots.foreach{
      root=>
        root.cloneSpec = CloneSpecification(cloneAll = root.vtree.level <= maxLevel, cloneFormulas.map(c => c.isSatisfiableIn(root.formula)), if (withParents) Array.empty else null)
        queue.enqueue(root)

    }

    while(queue.nonEmpty) {
      val node = queue.dequeue()
      val localCloneFormulas = cloneFormulas.map(c => c.project(node.vtree.vars))

      val clones = node.cloneSpec.clones.zipWithIndex.filter(_._1)
      node.elements.foreach{ element => element match {
        case PsddElement(_,prime,sub,_,formula,_,_) =>

          clones.foreach{case (_,index) =>
            val cloneFormula = localCloneFormulas(index)

            if (cloneFormula.isSatisfiableIn(formula) &&
              (node.vtree.level < maxLevel)) // not at maximum depth yet, so formula needs to be possible
            {
              Array(prime,sub).foreach{
                case child: PsddDecision =>
                  if (node.vtree.level < maxLevel || !cloneFormula.project(child.vtree.vars).isImpliedBy(child.formula)) {
                    if (child.cloneSpec == null) {
                      queue.enqueue(child)
                      child.cloneSpec = CloneSpecification(cloneAll = node.vtree.level < maxLevel, Array.fill(nbClones)(false), if (withParents) Array.fill(nbClones)(mutable.Set.empty[(PsddDecision, PsddElement)]) else null)
                    }
                    assert(cloneFormula.project(child.vtree.vars).isSatisfiableIn(child.formula), "child not satisfiable")
                    child.cloneSpec.clones(index) = true
                    if (withParents) child.cloneSpec.parents(index) += ((node, element))
                  }
                case _ =>
              }
            }
          }
      }
      }
    }
  }


  /**
    * This method returns all the nodes that have a clone specification that are decendents of one of the given roots.
    * A node appears in the list before before its children
    * @param roots
    * @return
    */
  private def clonableParentsBeforeChildren(roots: Array[_ <: PsddNode]): mutable.ArrayBuffer[PsddDecision]= {
    val queue = mutable.ArrayBuffer[PsddDecision]()
    roots.foreach{
      case root: PsddDecision =>
        if (root.cloneSpec!=null) {
          queue += root
          root.flag = true
        }
      case _ =>
    }
    if (queue.nonEmpty) {
      var i = 0
      while (i < queue.size) {
        val node = queue(i)
        node.elements.foreach { el =>
          el.prime match {
            case child: PsddDecision =>
              if (!child.flag && child.cloneSpec != null) {
                queue += child
                child.flag = true
              }
            case _ =>
          }
          el.sub match {
            case child: PsddDecision =>
              if (!child.flag && child.cloneSpec != null) {
                queue += child
                child.flag = true
              }
            case _ =>
          }
        }
        i += 1
      }
    }
    queue.foreach(_.flag=false)
    queue
  }

  private def clonableChildrenBeforeParents(roots: Array[_ <: PsddNode]) = clonableParentsBeforeChildren(roots).reverse


  /**
    * Simulate a clone of one node according to the clone specifications set.
    * @param node
    * @param parameterCalculator
    * @param cloneFormulas
    * @param dataFilters
    * @return
    */
  private def simulateClone(node: PsddDecision, parameterCalculator: ParameterCalculator, cloneFormulas: Array[Constraint], dataFilters: Array[Data]): (Double,Int) = {

    // new elements

    val (dllNewElements,dSize, trainStayData) = if (node.cloneSpec.cloneAll || cloneFormulas.length <2) {
      var trainStayData = node.trainData
      var dll = 0.0
      var dSize = 0

      node.cloneSpec.clones.zipWithIndex.foreach {
        case (false, _) => Array.empty[Double]
        case (true, index) =>
          val newNodeTrainData = node.trainData.intersect(dataFilters(index))
          trainStayData = trainStayData.diff(newNodeTrainData)
          val cloneFormula = cloneFormulas(index).project(node.vtree.vars)
          val clonedElements =node.elements.filter{ el => cloneFormula.isSatisfiableIn(el.formula) }

          val deterministic = node.deterministic || cloneFormula.isDeterministic(node.formula,node.vtree.vars)
          val freebee = deterministic || newNodeTrainData.isEmpty || trainStayData.isEmpty

          clonedElements.foreach { el =>
            val newElTrainData = el.data.train.intersect(newNodeTrainData)
            val locDll = newElTrainData.total * parameterCalculator.calculate(newElTrainData, newNodeTrainData, clonedElements.size)
            dll +=locDll

            if (!freebee) dSize += 1
          }
      }
      (dll, dSize, trainStayData)

    } else {
      val cloneMap = mutable.Map.empty[Sdd,(Data, Constraint)]
      node.cloneSpec.clones.zipWithIndex.foreach{
        case (false,_) =>
        case (true, index) =>
          val c = cloneFormulas(index).project(node.vtree.vars)
          val f = c.restrict(node.formula)
          val v = cloneMap.get(f)
          val data = if (v.isDefined) dataFilters(index).union(v.get._1) else dataFilters(index)
          cloneMap(f) = (data, c)
      }
      var trainStayData = node.trainData
      var dll = 0.0
      var dSize = 0
      cloneMap.toArray.foreach{ case(f,(data, cloneFormula)) =>
        val newNodeTrainData = node.trainData.intersect(data)
        trainStayData = trainStayData.diff(newNodeTrainData)

        val deterministic = node.deterministic || cloneFormula.isDeterministic(node.formula,node.vtree.vars)
        val freebee = deterministic || newNodeTrainData.isEmpty || trainStayData.isEmpty

        val clonedElements = node.elements.filter(el => cloneFormula.isSatisfiableIn(el.formula))
        clonedElements.foreach{ el =>
          val newElTrainData = el.data.train.intersect(newNodeTrainData)
          dll += newElTrainData.total*parameterCalculator.calculate(newElTrainData, newNodeTrainData, clonedElements.size)

          if (!freebee) dSize += 1
        }
      }
      (dll, dSize, trainStayData)
    }

    // Staying element

    val oldLl = node.elements.toArray.map(el=> el.data.train.total*el.theta).sum

    val newNodeTrainData = node.trainData.intersect(trainStayData)
    val newLlOldNode = node.elements.toArray.map{el =>
      val newElTrainData = el.data.train.intersect(newNodeTrainData)
      newElTrainData.total*parameterCalculator.calculate(newElTrainData, newNodeTrainData, node.elements.size)
    }.sum


    (dllNewElements + newLlOldNode - oldLl, dSize)
  }


  /**
    * Simulate a max depth clone according to the clone specifications set.
    * @param cloneRoots
    * @param parameterCalculator
    * @param dataFilters
    * @param cloneFormulas
    * @param maxDepth
    * @return
    */
  private def simulateCloneMaxDepth(cloneRoots: Array[PsddDecision], parameterCalculator: ParameterCalculator, dataFilters: Array[Data], cloneFormulas: Array[Constraint], maxDepth: Int, root:PsddDecision): SimulationResult = {
    setMaxDepthCloneSpecifications(cloneRoots, dataFilters, cloneFormulas, maxDepth, withParents = false)

    val clonables = clonableParentsBeforeChildren(cloneRoots).toArray
    val (dll: Double, dSize: Int) = {
      if (clonables.isEmpty) (0.0,0)
      else clonables.map(node => simulateClone(node, parameterCalculator, cloneFormulas, dataFilters)).reduce( (a,b) => (a._1+b._1,a._2+b._2))
    }

    clonables.foreach(_.cloneSpec = null)

    val changedNodes = clonables.map(_.index).toSet

    SimulationResult(dll,dSize)
  }


  /**
    * Simulate a constrained multi clone (used by split)
    * @param roots
    * @param parameterCalculator
    * @param dataFilters
    * @param cloneFormulas
    * @param operationCompletionType
    * @param minDll
    * @return
    */
  private def simulateConstrainedClone(cloneRoots: Array[PsddDecision], parameterCalculator: ParameterCalculator, dataFilters: Array[Data], cloneFormulas: Array[Constraint], operationCompletionType: OperationCompletionType, minDll: Double,root:PsddDecision): SimulationResult = {
    operationCompletionType match {
      case Complete => simulateCloneMaxDepth(cloneRoots, parameterCalculator, dataFilters, cloneFormulas, Int.MaxValue,root)
      case Minimal=> simulateCloneMaxDepth(cloneRoots, parameterCalculator, dataFilters, cloneFormulas, 0,root)
      case MaxDepth(k) => simulateCloneMaxDepth(cloneRoots, parameterCalculator, dataFilters, cloneFormulas,k, root)
    }
  }


  ////////////////
  // PARAMETERS //
  ////////////////

  def calculateParameters(psdd: PsddNode, parameterCalculator: ParameterCalculator,root:PsddDecision): Unit = PsddQueries.decisionNodes(psdd).foreach{ node =>
    val nodeTrainData = node.trainData
    val nbElements = node.elements.size
    node.elements.foreach{el =>
      el.theta = parameterCalculator.calculate(el.data.train, nodeTrainData, nbElements)
    }
    if(!Util.isEqual(node.elements.toArray.map(_.theta).reduceLeft(Log.add), Log.one)){
      println(node.elements.toArray.map(el=>math.pow(math.E,el.theta)).mkString(";")+"Sum"+node.elements.map(_.theta).reduce(Log.add).toString+"log"+node.elements.map(_.theta).mkString(";"))
      println(node.index)

      findNode(root,node.index)

      require(true==false, "elements parameters do not sum up to one\n")
    }
  }

  //findNode is used to debug occured in parameter calculations
  def findNode(current:PsddNode,target:Int):Boolean = {
    if (current.index == target){
      println(current.elements.size)
      current.elements.foreach{el=>
        println(el.constraints.toString)
      }
      true
    }else{
      current match {
        case n:PsddDecision =>
          var found = false
          n.elements.foreach{el=>
            if (!found) {
              found = findNode(el.prime,target)
            }
            if (!found) {
              found = findNode(el.sub,target)
            }
          }
          found
        case _ => false
      }
    }
  }

  /**
    * simulate a clone
    * @param root
    * @param parameterCalculator
    * @param parents
    * @param operationCompletionType
    * @return
    */
  def simulateClone(cloneNode: PsddDecision,  parents: Set[PsddElement], parameterCalculator: ParameterCalculator, operationCompletionType: OperationCompletionType, root:PsddDecision): SimulationResult = {
    val res = simulateConstrainedClone(Array(cloneNode), parameterCalculator, Array(parents.map(_.data.train).reduce(_.union(_))), Array(NoConstraint), operationCompletionType,minDll,root)
    SimulationResult(res.dll, res.dSize)
  }


  /////////////////////////////
  // Distribute Data to PSDD //
  /////////////////////////////

  /**
    * Distribute data over a sub-PSDD
    * @param psdd
    */

  def distributeData(node:PsddNode,data:DataSets): Unit = {
    require(roots.contains(node),"The provide node is not a psdd root.\n")
    val elements = PsddQueries.elements(node)
    val emptyData = data.empty
    elements.foreach{el => el.data = emptyData}
    passDataToElement(node,data)
  }

  private def passDataToElement(node:PsddNode,data:DataSets): Unit = {
    node.elements.foreach{el=>
      val filteredData = data.filter(el.constraints.isSatisfiedBy)
      el.data = el.data.union(filteredData)
      passDataToDecisionNode(el,filteredData)
    }
  }

  private def passDataToDecisionNode(element:PsddElement,data:DataSets): Unit = {
    element.prime match{
      case n: PsddDecision =>
        passDataToElement(n,data)
      case _ =>
    }
    element.sub match{
      case n: PsddDecision =>
        passDataToElement(n,data)
      case _ =>
    }
  }

  ////////////////////////
  // Build Initial PSDD //
  ////////////////////////

  var totalVtreeLevels = 0
  /**
    * Make a PSDD for the given vtree that represents independent variables (the product of marginals)
    * @param vtree
    * @param data
    * @param parameterCalculator
    * @return
    */
  def newPsdd(vtree: VtreeInternal, data: DataSets, parameterCalculator: ParameterCalculator): PsddDecision = {

    val vtree2psdd = mutable.Map[VtreeNode, PsddDecision]()

    for (vtree <- vtree.parentsBeforeChildren().reverse) vtree2psdd(vtree) = vtree match {
      case vtree: VtreeInternalVar =>
        if (vtree.level>totalVtreeLevels) totalVtreeLevels=vtree.level
        val pos = litPsdd(vtree.left, true)
        val neg = litPsdd(vtree.left, false)
        PsddDecision(nextNodeIndex,vtree, mutable.ArrayBuffer(
          PsddElement(nextElementIndex,pos,trueNode, data, pos.formula, math.log(0.5), ConjunctionConstraint(Map(vtree.v->true))),
          PsddElement(nextElementIndex,neg,trueNode, data, neg.formula, math.log(0.5), ConjunctionConstraint(Map(vtree.v->false)))),trueSdd)


      case vtree: VtreeInternal =>
        val prime = vtree2psdd(vtree.left)
        val sub = vtree2psdd(vtree.right)
        val element = PsddElement(nextElementIndex, prime, sub, data, trueSdd, Log.one)
        val elements = mutable.ArrayBuffer(element)
        PsddDecision(nextNodeIndex, vtree, elements, trueSdd)
    }
    val root = vtree2psdd(vtree)
    roots += root
    distributeData(root,data)
    calculateParameters(root, parameterCalculator,root)
    root
  }

  // make a literal node
  private def litPsdd(vtree: VtreeNode, pos:Boolean): PsddLiteral = vtree match {
    case vtree: VtreeVar =>
      val v = if (pos) vtree.v else -vtree.v
      PsddLiteral(nextNodeIndex, vtree, v, new Sdd(v, sddManager))
    case _ => throw new IllegalArgumentException("vtree must be a leaf node")
  }

}
