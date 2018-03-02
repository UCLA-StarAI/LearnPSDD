package algo

import java.io.{File, PrintWriter}

import structure.{Data, VtreeNode}
import util.MutualInformation

import scala.collection.mutable
import scala.io.Source
import scala.sys.process._


/**
  * Learns a vtree from data
  */
object VtreeLearner {

  /**
    * Learns a balanced vtree from data by recursively splitting the set variables in two equal pieces
    * so that the pairwise mutual information between the two sets is minimized
    *
    * To find the best split, metis is used.
    *
    * @param data
    * @param ufactor
    * @param out
    * @return
    */
  def learnMetisTopDown(data: Data, ufactor: Int=1, out: String): VtreeNode = {
    val mi = MutualInformation.pairwiseMutualInformation(data)

    // convert pairwiseWeights to int
    val minInt = 1
    val maxInt = 10000
    val intMi = toLongMi(mi, minInt, maxInt)

    val dataVarsIndices = data.vars.indices.map(i=>(data.vars(i),i)).toMap

    def split(vars: Array[Int], context: Array[Int]): (Array[Int],Array[Int]) = {

      // metis needs vertices 1 to n => use index vars +1
      def mv2di(mv: Int) = dataVarsIndices(vars(mv-1))


      // makeNodeExpansionList graph file
      val graphFilePath = out+".metis.graph"
      val graphFile = new File(graphFilePath)
      val pw = new PrintWriter(graphFile)
      pw.write(vars.length+" "+(vars.length*(vars.length-1))/2+" 001\n")
      for (mv1 <- 1 to vars.length){
        pw.write((1 to vars.length).flatMap(mv2 => if (mv1==mv2) None else Some(mv2+" "+intMi(mv2di(mv1))(mv2di(mv2)))).mkString(" ")+"\n")
      }

      pw.flush()
      pw.close()

      // execute metis
      val metisOutput = "gpmetis -ufactor " + ufactor + " " + graphFile.getAbsolutePath + " 2" !!

      // read metis solution
      val solutionFile = new File(graphFilePath+".part.2")
      val (p1,p2) = Source.fromFile(solutionFile).getLines().zipWithIndex.partition(_._1.contains("1"))
      val a1 = p1.map(mv=>vars(mv._2)).toArray
      val a2 = p2.map(mv=>vars(mv._2)).toArray

      solutionFile.delete()
      graphFile.delete()

      if (a1.nonEmpty && a2.nonEmpty)
        (a1,a2)
      else{  // metis can return empty partitions
        println("metis gave emtpy partition for ("+vars.mkString(",")+")")
        if (vars.length<8)
          vars.splitAt(vars.length/2)
        else {
          var minX: Array[Int] = Array()
          var minY: Array[Int] = Array()
          var minMI = Double.PositiveInfinity
          for(x <- vars.combinations(vars.length/2)){
            val y = vars.filterNot(x.contains)
            val mutInfo = MutualInformation.setMi(x,y,mi,dataVarsIndices)
            if (mutInfo<minMI){
              minX = x
              minY = y
              minMI = mutInfo
            }
          }
          (minX,minY)
        }
      }
    }
    VtreeNode.constructTopDown(data.vars, split)
  }


  def toLongMi(mi: Array[Array[Double]], minInt: Int, maxInt: Int): Array[Array[Long]] = {
    val maxMi = mi.flatten.max
    val minMi = mi.flatten.min
    val deltaMi = maxMi-minMi

    val deltaInt = maxInt - minInt

    mi.map { a => a.map { v =>((v/deltaMi*deltaInt) + minInt).round } }
  }

  /**
    * Learns a balanced vtree from data by recursively pairing sets of variables
    * so that the pairwise mutual information of the pairs is maximized.
    *
    * To find the optimal pairs of all sets of variables, blossom5 is used.
    *
    * @param data
    * @param out
    * @return
    */
  def learnBlossomBottomUp(data: Data, out: String): VtreeNode = {

    val mi = MutualInformation.pairwiseMutualInformation(data)

    val dataVarsIndices = data.vars.indices.map(i=>(data.vars(i),i)).toMap

    def vars2mi(cluster1: Array[Int], cluster2: Array[Int]):Double = MutualInformation.setMi(cluster1,cluster2,mi, dataVarsIndices)

    def combine(nodes: Array[Array[Int]]) = blossomCombine(nodes.length, {(i1,i2) => vars2mi(nodes(i1),nodes(i2))}, out)

    VtreeNode.constructBottomUp(data.vars, combine)
  }


  def blossomCombine(nbNodes: Int, pairWeights: (Int,Int)=> Double, out: String): Array[Array[Int]] = {
    assert (nbNodes>1)

    if (nbNodes % 2 == 0) { //even
      blossomCombineEven(nbNodes, pairWeights, out)._1
    }
    else { // odd
    val (bestI, bestPairs,_) = (0 until nbNodes).indices.foldLeft((0,Array[Array[Int]](),Double.PositiveInfinity)){
      case ((bestI, bestPairs, bestScore),i) =>
        val (pairs,score) = blossomCombineEven(nbNodes-1, {(i1,i2)=> pairWeights(if (i1<i) i1 else i1+1,if (i2<i) i2 else i2+1 )} , out)
        if (score<bestScore)
          (i,pairs.map(a=>a.map(j=> if (j>=i)j+1 else j)),score)
        else
          (bestI,bestPairs,bestScore)
    }
      Array(Array(bestI)) ++ bestPairs
    }
  }

  def blossomCombineEven(nbNodes: Int, pairWeights: (Int,Int)=>Double, out: String): (Array[Array[Int]],Double) = {

    // blossom needs vertices 0 to n => use nodes index

    // calculate setMIs:
    val weights = Array.fill(nbNodes)(Array.fill(nbNodes)(0.0))
    (0 until nbNodes).foreach{ i =>
      (i+1 until nbNodes).foreach{ j =>
        weights(i)(j) = pairWeights(i,j)
      }
    }

    val intSetMi = toLongMi(weights, -1, -1000000)



    // makeNodeExpansionList graph file
    val graphFilePath:String = out + ".blossom.graph"
    println (graphFilePath)
    val outPath = graphFilePath+".pairs"

    val graphFile = new File(graphFilePath)
    val outFile = new File(outPath)
    val pw = new PrintWriter(graphFile)

    pw.write(nbNodes+" "+(nbNodes*(nbNodes-1))/2+"\n")

    (0 until nbNodes).foreach(i => (i+1 until nbNodes).foreach(j => pw.write(i+" "+j+" "+intSetMi(i)(j)+"\n")))

    pw.flush()
    pw.close()

    //execute blossom
    val blossomOutput = "blossom5 -e " + graphFilePath + " -w " + outPath !!
    val score = blossomOutput.split("\n").last.split("=").last.toDouble


    // read blossom solution
    val result = Source.fromFile(outFile).getLines().drop(1).map{line =>
      line.trim.split(" ").map(_.toInt)
    }.toArray

    graphFile.delete()
    outFile.delete()


    (result,score)
  }



  /**
    * Learns a vtree from data by recursively greedily pairing two sets of variables.
    * The selected pair is the pair with the greatest mutual information.
    *
    * @param data
    * @return
    */
  def learnGreedyBottomUp(data: Data): VtreeNode = {
    val mi = MutualInformation.pairwiseMutualInformation(data)
    val v2i = data.vars.indices.map(i=> data.vars(i) -> i).toMap

    def combine(nodes: Array[Array[Int]]): Array[Array[Int]] = {

      // calculate all pairs
      val pairs = nodes.indices.combinations(2).map(p => ((p(0),p(1)), MutualInformation.setMi(nodes(p(0)),nodes(p(1)),mi, v2i))).toList.sortBy(_._2)
      val combinations = mutable.MutableList[Array[Int]]()
      var indices = nodes.indices.toArray

      for (((i,j),w) <- pairs) {
        if (indices.contains(i) && indices.contains(j)) {
          combinations += Array(i,j)
          indices = indices.filterNot(v=>v==i || v==j)
        }
      }
      if (indices.nonEmpty){
        combinations += Array(indices(0))
      }
      combinations.toArray
    }

    VtreeNode.constructBottomUp(data.vars, combine)
  }


  /**
    * Learns a balanced vtree from data by recursively splitting the set variables in two equal pieces
    * so that the pairwise mutual information between the two sets is minimized
    *
    * To find the best split, all options are tried.
    *
    * @param data
    * @return
    */
  def learnExhaustiveTopDown(data: Data): VtreeNode = {
    val mi = MutualInformation.pairwiseMutualInformation(data)
    val v2i = data.vars.indices.map(i=> data.vars(i)->i).toMap

    def split(vars: Array[Int], context: Array[Int]): (Array[Int],Array[Int]) = {
      var minX: Array[Int] = Array()
      var minY: Array[Int] = Array()
      var minMI = Double.PositiveInfinity

      for(x <- vars.combinations(vars.length/2)){
        val y = vars.filterNot(x.contains)
        val mutInfo = MutualInformation.setMi(x,y,mi,v2i)
        if (mutInfo<minMI){
          minX = x
          minY = y
          minMI = mutInfo
        }
      }
      if (vars.length % 2 == 0) (minX, minY) else (minY, minX)
    }

    VtreeNode.constructTopDown(data.vars, split)
  }
}