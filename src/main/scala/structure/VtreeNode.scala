package structure

import java.io.{File, PrintWriter}

import scala.collection.mutable
import scala.io.Source

/**
  * A vtree node
  *
 * Created by jessa on 3/27/16.
 *
 */
abstract class VtreeNode {
  def get(position: Int): VtreeInternal
  def vars: Array[Int]

  def visualization: String

  def size: Int
  def setLevels(level: Int = 0): Unit = {}
  var index = -1

  def indexLeftMost: Int
  def indexRightMost: Int
}


/**
  * A vtree internal node. This is any vtree node that has an index
  *
  * @param i_index
  * @param i_parent
  * @param left
  * @param right
  */
class VtreeInternal(i_index: Int, i_parent: VtreeInternal, var left: VtreeNode, var right: VtreeNode) extends VtreeNode {

  index = i_index

  var level: Int = -1
  var parent = i_parent

  def root: VtreeNode = if (parent == null) this else parent.root

  override def vars: Array[Int] = left.vars ++ right.vars

  override def size: Int = right.size

  override def indexLeftMost = left.indexLeftMost
  override def indexRightMost = right.indexRightMost

  def nbVars = (indexRightMost-indexLeftMost)/2+1

  def maxLevel: Int = {
    var m = this.level
    left match{
      case left: VtreeInternal => m = left.asInstanceOf[VtreeInternal].maxLevel
      case _ =>
    }
    right match{
    case right: VtreeInternal =>
        val mr = right.maxLevel
        if (mr>m) m = mr
      case _ =>
    }
    m
  }

  def get(position: Int): VtreeInternal ={
    if (position==index) this
    else if (position < index) left.get(position)
    else right.get(position)
  }

  def setParents(): Unit = {
    left match {
      case child : VtreeInternal =>
        child.parent = this
        child.setParents()
      case _ =>
    }
    right match {
      case child : VtreeInternal =>
        child.parent = this
        child.setParents()
      case _ =>
    }
  }

  override def setLevels(level: Int = 0): Unit = {
    this.level=level
    left.setLevels(level+1)
    right.setLevels(level+1)
  }

  override def visualization: String =  "("+index+": "+left.visualization+" , "+right.visualization + " )"


  def save(file: File): Unit = {
    val pw = new PrintWriter(file)
    pw.write("c ids of vtree nodes start at 0\nc ids of variables start at 1\nc vtree nodes appear bottom-up, children before parents\nc\nc file syntax:\nc vtree number-of-nodes-in-vtree\nc L id-of-leaf-vtree-node id-of-variable\nc I id-of-internal-vtree-node id-of-left-child id-of-right-child\nc\n")
    pw.write("vtree "+size)
    for (node <- parentsBeforeChildren().reverseIterator) node match {
      case node: VtreeInternalVar => pw.write("\nL "+node.index+" "+node.v)
      case node: VtreeInternal => pw.write("\nI "+node.index+" "+node.left.index+" "+node.right.index)
    }
    pw.flush()
    pw.close()
  }

  def parentsBeforeChildren(): Seq[VtreeInternal] ={
    var i = 0
    val childrenAfterParents = mutable.MutableList(this)

    while (i<childrenAfterParents.size) {
      childrenAfterParents(i).left match {
        case node: VtreeInternal => childrenAfterParents += node
        case _ =>
      }
      childrenAfterParents(i).right match {
        case node: VtreeInternal => childrenAfterParents += node
        case _ =>
      }
      i+=1
    }
    childrenAfterParents
  }

  def saveAsDot(file: File): Unit = {

    val nodes = mutable.MutableList[String]()
    val edges = mutable.MutableList[String]()
    val queue = new mutable.Queue[VtreeNode]
    queue.enqueue(this)

    while(queue.nonEmpty) {
      queue.dequeue() match {
        case node: VtreeInternalVar=> // makeNodeExpansionList dot leaf node
          nodes += "n"+node.index+" [label=\""+node.v+"\",fontname=\"Times-Italic\",fontsize=14,shape=\"plaintext\",fixedsize=true,width=.25,height=.25];"

        case node: VtreeInternal => // makeNodeExpansionList dot decision node
          nodes += "n"+node.index+" [label=\""+node.index+"\",fontname=\"Times\",shape=\"plaintext\",fontsize=12,fixedsize=true,width=.2,height=.18];"
          // makeNodeExpansionList connections
          for(child <- Array(node.left,node.right)){
            queue.enqueue(child)
            child match {
              case child: VtreeInternalVar => edges+= "n"+node.index+"->n"+child.index+" [headclip=true,arrowhead=none,headlabel=\""+child.index+"\",labelfontname=\"Times\",labelfontsize=10];"
              case child: VtreeInternal =>edges+= "n"+node.index+"->n"+child.index+" [arrowhead=none];"
            }
          }
      }
    }


    val pw = new PrintWriter(file)
    pw.write(
      """digraph vtree {

overlap=false

      """)

    pw.write(nodes.mkString("\n"))
    pw.write("\n\n")
    pw.write(edges.mkString("\n"))
    pw.write(
      """

}
      """)
    pw.flush()
    pw.close()
  }


}


/**
  * A Vtree internal variable node. This represents a variable (leaf) in the vtree. But it actually contains children:
  * the "real" variable node on the left and a vtree terminal on the right. This is because in our PSDD representation,
  * a literal is represented as a decision node with one element with the literal as the prime and "true" as the sub. Similarly a
  * distribution over a variable is a decision node with two elements: one with the positive literal as the prime and
  * "true" as the sub and the other with the negative literal as the prime and "true as the sub.
  *
  * @param i_index
  * @param i_parent
  * @param v
  */
class VtreeInternalVar(i_index: Int, i_parent: VtreeInternal, val v: Int) extends VtreeInternal(i_index, i_parent, new VtreeVar(v), VtreeTerminal) {
  index = i_index
  parent = i_parent

  override def size: Int = index+1
  override def setParents(){}
  override def setLevels(level: Int = 0) = this.level=level

  override def visualization: String = "("+index+": "+v+" )"

  override def indexLeftMost = index
  override def indexRightMost = index

}

/**
  * The "real" vtree variable
  * @param v
  */
case class VtreeVar(v: Int) extends VtreeNode{

  override def size: Int = 0
  override def vars: Array[Int] = Array(v)

  def get(position: Int): VtreeInternal = throw new IllegalArgumentException("the vtree has no node with index "+position)

  override def visualization: String = v.toString

  override def indexLeftMost = -1
  override def indexRightMost = -1

}

/**
  * Vtree terminal node
  */
case object VtreeTerminal extends VtreeNode {
  override def get(position: Int): VtreeInternal = throw new IllegalArgumentException("the vtree has no node with index "+position)

  override def size: Int = 0

  override def visualization: String = "_"

  override def vars: Array[Int] = Array()

  override def indexLeftMost = -1
  override def indexRightMost = -1
}


/**
  * This object contains code to construct vtrees
  *
  * Default vtrees are:
  *  - balanced
  *  - right linear
  *  - left linear
  *
  * A vtree can also be read from file
  *
  * Or constructed
  *  - top down providing a split method
  *  - bottom up providing a combination method
  *
  *  These construction methods are used for learning the vtree from data in the vtreeLearner
  *
  */
object  VtreeNode {


  def balanced(varOrder: Array[Int]): VtreeInternal = constructTopDown(varOrder, (vars, _) => vars.splitAt(vars.length/2))
  def rightLinear(varOrder: Array[Int]): VtreeInternal = constructTopDown(varOrder, (vars, _) => vars.splitAt(1))
  def leftLinear(varOrder: Array[Int]): VtreeInternal = constructTopDown(varOrder, (vars, _) => vars.splitAt(vars.length-1))
  def balancedBottomUp(varOrder: Array[Int]): VtreeInternal = constructBottomUp(varOrder, vars => vars.indices.toArray.grouped(2).toArray)

  def constructTopDown(varOrder: Array[Int], splitMethod: (Array[Int], Array[Int]) => (Array[Int], Array[Int])): VtreeInternal = {
    if (varOrder.isEmpty)
      throw new IllegalArgumentException("A vtree needs to have at least 1 variable.")
    if (varOrder.length==1)
      return new VtreeInternalVar(0,null,varOrder.head)

    // #vars is at least two
    val queue = new mutable.Queue[(Array[Int], VtreeInternal, Int, Array[Int])]()

    def makeChild(vars: Array[Int], node: VtreeInternal, contextSize: Int, context: Array[Int]): VtreeNode = {
      if (vars.length == 1)
        new VtreeInternalVar(contextSize, node, vars.head)
      else {
        val res = new VtreeInternal(-1, node, null, null)
        queue.enqueue((vars, res, contextSize, context))
        res
      }
    }

    val root = new VtreeInternal(-1, null, null, null)

    queue.enqueue((varOrder, root, 0, Array()))

    while(queue.nonEmpty){
      val (vars, node, contextSize, context) = queue.dequeue()
      val (varsLeft, varsRight) = splitMethod(vars, context)
      val contextSizeLeft = contextSize
      val contextSizeRight = contextSize + (2* varsLeft.length)
      val contextRight = context ++ varsLeft
      node.index = contextSizeRight -1

      node.left = makeChild(varsLeft, node, contextSizeLeft, context)
      node.right = makeChild(varsRight, node, contextSizeRight, contextRight)

    }

    root.setLevels()
//    setIndexes(root)
    root
  }

  def constructBottomUp(varOrder: Array[Int], combineMethod: (Array[Array[Int]]) => Array[Array[Int]]): VtreeInternal = {
    if (varOrder.isEmpty)
      throw new IllegalArgumentException("A vtree needs to have at least 1 variable.")

    var nodes: Array[VtreeInternal] = varOrder.map(v => new VtreeInternalVar(-1, null, v))
    var vars: Array[Array[Int]] = varOrder.map(v => Array(v))

    while (nodes.length > 1) {

      val combos = combineMethod(vars)

      vars = combos.map(combo => if (combo.length == 1) vars(combo(0)) else vars(combo(0)) ++ vars(combo(1)))
      nodes = combos.map { combo =>
        if (combo.length == 1) nodes(combo(0))
        else {
          val parent = new VtreeInternal(-1, null, nodes(combo(0)), nodes(combo(1)))
          nodes(combo(0)).parent = parent
          nodes(combo(1)).parent = parent
          parent
        }
      }
    }
    setIndexes(nodes(0))
    nodes(0).setLevels()
    nodes(0)
  }

  def setIndexes(vtreeNode: VtreeNode, contextSize: Int = -1): Int ={
    vtreeNode match {
      case vtree: VtreeInternalVar => vtree.index = contextSize+1; vtree.index
      case vtree: VtreeInternal =>
        val newContextSize = setIndexes(vtree.left, contextSize)
        vtree.index = newContextSize+1
        setIndexes(vtree.right, vtree.index)
      case _ => contextSize
    }
  }

  def read(file: File): VtreeInternal ={

    val comment = "[cC].*".r
    val vtree = "vtree (\\d+)".r
    val leaf = "[lL] (\\d+) (\\d+)".r
    val internal = "[iI] (\\d+) (\\d+) (\\d+)".r

    var vtreeNodes: Array[VtreeInternal] = null
    var root: VtreeInternal = null

    for (line <- Source.fromFile(file).getLines()) line match{
      case comment() => //println("comment")
      case vtree(nbNodes) => //println("vtree",nbNodes.toInt)
        vtreeNodes = new Array(nbNodes.toInt)
      case leaf(id, v) => //println("leaf",id,v)
        root = new VtreeInternalVar(id.toInt, null, v.toInt)
        vtreeNodes(id.toInt) = root
      case internal(id, left, right) => //println("internal",id, left,right)
        root = new VtreeInternal(id.toInt, null, vtreeNodes(left.toInt), vtreeNodes(right.toInt))
        vtreeNodes(id.toInt) = root
    }
    root.setParents()
    setIndexes(root)
    root.setLevels()
    root
  }
}