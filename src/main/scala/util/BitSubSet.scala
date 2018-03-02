package util

import scala.collection.immutable.BitSet

/**
  * A Bitset with its backend that makes it possible to add, remove and filter using the original representation
  * @param backend
  * @param elements
  * @tparam T
  */
class BitSubSet[T](val backend: Array[T], val elements: BitSet) extends Set[T]{

  override def iterator: Iterator[T] = elements.iterator.map { e => backend(e) }

  def weightedIterator: Iterator[(T,Double)] = elements.iterator.map { e => (backend(e),1.0) }

  // move to a sorted list for backend when calling these frequently
  override def -(elem: T): Set[T] = new BitSubSet(backend, elements - backend.indexOf(elem))
  override def +(elem: T): Set[T] = new BitSubSet(backend, elements + backend.indexOf(elem))

  override def contains(elem: T): Boolean = elements.contains(backend.indexOf(elem))

  override def empty = new BitSubSet(backend,BitSet.empty)

  override def filter(p: (T) => Boolean): BitSubSet[T] = {
    new BitSubSet(backend,elements.filter { i => p(backend(i)) })
  }

  override def filterNot(p: (T) => Boolean): BitSubSet[T] = {
    new BitSubSet(backend,elements.filterNot { i => p(backend(i)) })
  }

  def intersect(that: BitSubSet[T]): BitSubSet[T] = {
    require(this.backend eq that.backend)
    new BitSubSet(backend, this.elements intersect that.elements)
  }

  def union(that: BitSubSet[T]): BitSubSet[T] = {
    require(this.backend eq that.backend)
    new BitSubSet(backend, this.elements union that.elements)
  }

  def diff(that: BitSubSet[T]): BitSubSet[T] = {
    require(this.backend eq that.backend)
    new BitSubSet(backend, this.elements diff that.elements)
  }

  override val size = elements.size
  val total = size.toDouble

  override def toString = mkString("BitSubSet(",",",")")

  override def equals(other: Any): Boolean = other match {
    case other: BitSubSet[T] => this.elements == other.elements
    case _ => false
  }


}

object BitSubSet{

  def apply[T](backend: Array[T]): BitSubSet[T] = new BitSubSet(backend, BitSet(backend.indices:_*))

}

/**
  * Weighted version for the bitsubset. This is used for the Datasets in the PSDD nodes
  * @param backend
  * @param weights
  * @param elements
  * @tparam T
  */
class WeightedBitSubSet[T](override val backend: Array[T], val weights: Array[Double], override val elements: BitSet) extends BitSubSet(backend, elements){

  override def weightedIterator: Iterator[(T,Double)] = elements.iterator.map { e => (backend(e),weights(e)) }

  override def empty = new WeightedBitSubSet(backend, weights, BitSet.empty)

  override def filter(p: (T) => Boolean): WeightedBitSubSet[T] = {
    new WeightedBitSubSet(backend,weights, elements.filter { i => p(backend(i)) })
  }

  override def filterNot(p: (T) => Boolean): WeightedBitSubSet[T] = {
    new WeightedBitSubSet(backend,weights, elements.filterNot { i => p(backend(i)) })
  }

  override def intersect(that: BitSubSet[T]): WeightedBitSubSet[T] = {
    require(this.backend eq that.backend)
    new WeightedBitSubSet(backend, weights, this.elements intersect that.elements)
  }

  override def union(that: BitSubSet[T]): WeightedBitSubSet[T] = {
    require(this.backend eq that.backend)
    new WeightedBitSubSet(backend, weights, this.elements union that.elements)
  }


  override def diff(that: BitSubSet[T]): WeightedBitSubSet[T] = {
    require(this.backend eq that.backend)
    val diffElements = this.elements diff that.elements
    new WeightedBitSubSet(backend, weights, diffElements)
  }

  override val total = elements.foldLeft(0.0)((sum,i)=>sum+weights(i))

  override def toString = mkString("WeightedBitSubSet(",",",")")
}

object WeightedBitSubSet{

  def apply[T](backend: Array[T], weights: Array[Double]): WeightedBitSubSet[T] = new WeightedBitSubSet(backend, weights, BitSet(backend.indices:_*))

}