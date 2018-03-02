package structure

import java.io.File

import util.{BitSubSet, WeightedBitSubSet,Util}

import scala.collection.immutable.BitSet
import scala.collection.mutable

/**
  * This class stores a dataset as a bitmap and allows bitset operations
  *
  * Created by jessa on 7/26/16
  * Modified by yitao on 1/28/17
  *
  * @param backend
  * @param weights
  * @param vars
  * @param elements
  */
class Data(backend: Array[Map[Int,Boolean]], weights: Array[Double], val vars: Array[Int], elements: BitSet) extends WeightedBitSubSet[Map[Int,Boolean]](backend, weights, elements){
  override def weightedIterator: Iterator[(Map[Int,Boolean],Double)] = elements.iterator.map { e => (backend(e),weights(e)) }

  override def empty = new Data(backend, weights, vars, BitSet.empty)

  override def filter(p: (Map[Int,Boolean]) => Boolean): Data = {
    new Data(backend,weights, vars, elements.filter { i => p(backend(i)) })
  }

  override def filterNot(p: (Map[Int,Boolean]) => Boolean): Data = {
    new Data(backend,weights, vars, elements.filterNot { i => p(backend(i)) })
  }

  override def intersect(that: BitSubSet[Map[Int,Boolean]]): Data = {
    require(this.backend eq that.backend)
    new Data(backend, weights, vars, this.elements intersect that.elements)
  }

  override def union(that: BitSubSet[Map[Int,Boolean]]): Data = {
    require(this.backend eq that.backend)
    new Data(backend, weights, vars, this.elements union that.elements)
  }


  override def diff(that: BitSubSet[Map[Int,Boolean]]): Data = {
    require(this.backend eq that.backend)
    val diffElements = this.elements diff that.elements
    new Data(backend, weights, vars, diffElements)
  }

  def checkOperation(that: BitSubSet[Map[Int,Boolean]]): Boolean = (this.backend eq that.backend)

  def copy() = new Data(backend.map(Map[Int,Boolean]()++_),weights.map(x=>x),vars.map(x=>x),BitSet()++elements)

  override def toString = "Data("+size+","+total+")"
}

object Data {

  def apply(backend: Array[Map[Int,Boolean]], weights: Array[Double], vars: Array[Int]): Data = new Data(backend, weights, vars, BitSet(backend.indices:_*))

  def readFromFile(file: File): Data = {
    val assignments = mutable.Map[String,Double]()
    io.Source.fromFile(file).getLines().withFilter(_.nonEmpty).foreach{ line =>
      val split = line.split("\\|")
      val (as,w) = if (split.size==1) (split.head,1.0) else (split(1),split.head.toDouble)
      assignments.put(as,w+assignments.getOrElse(as,0.0))
    }
    val (b,weights) = assignments.toArray.unzip
    val backend = b.map(Util.convertToAssignment(_))

    Data(backend, weights, (1 to backend.head.size).toArray)
  }

  def readFromArray(samples: Array[Array[Int]],weights:Array[Double]): Data ={
    val backend = samples.map(Util.convertToAssignment(_))
    Data(backend, weights, (1 to backend.head.size).toArray)
  }

  def readDataAndWeights(assignments:Array[(Map[Int,Boolean],Double)]): Data = {
    val backend = assignments.map(_._1)
    val w = assignments.map(_._2)
    Data(backend,w, (1 to backend.head.size).toArray)
  }

}

/**
  * This class stores training validation an test data. Keeping the three together makes it easy to
  * apply the same operations on all of them in a PSDD
  * @param dataSets
  */
class DataSets(val dataSets: Array[Data]) {

  def this(train: Data, valid: Data, test: Data) = this(Array(train,valid,test))

  def empty = new DataSets(dataSets.map(_.empty))

  def filter(p: (Map[Int,Boolean]) => Boolean) = new DataSets(dataSets.map(_.filter(p)))
  def filterNot(p: (Map[Int,Boolean]) => Boolean) = new DataSets(dataSets.map(_.filterNot(p)))
  def intersect(other: DataSets) = new DataSets(dataSets.zip(other.dataSets).map{ case (a,b) =>a.intersect(b)})
  def union(other: DataSets) = new DataSets(dataSets.zip(other.dataSets).map{ case (a,b) =>a.union(b)})
  def diff(other: DataSets) = new DataSets(dataSets.zip(other.dataSets).map{ case (a,b) =>a.diff(b)})

  def checkOperation(other:DataSets): Boolean = dataSets.zip(other.dataSets).forall{case(a,b)=>a.checkOperation(b)}

  def isEmpty = dataSets.forall(_.isEmpty)

  def forall(p: Map[Int,Boolean]=>Boolean) = dataSets.forall(_.forall(p))
  def exists(p: Map[Int,Boolean]=>Boolean) = dataSets.exists(_.exists(p))

  def train = dataSets.head
  def valid = dataSets(1)
  def test = dataSets(2)

  override def equals(other: Any): Boolean = other match {
    case other: DataSets =>
      if (this.dataSets.length != other.dataSets.length) return false
      this.dataSets.zip(other.dataSets).forall{case (data1,data2) => data1==data2}
    case _ => false
  }

  def copy() = new DataSets(this.dataSets.map(_.copy()))

  override def toString: String = dataSets.mkString("[",",","]")
}