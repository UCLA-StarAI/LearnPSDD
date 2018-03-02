package operations

import structure.{PsddDecision, PsddElement}

import scala.collection.mutable

/**
 * Created by jessa on 9/1/16.
 *
 */
case class CloneSpecification(cloneAll: Boolean, clones: Array[Boolean], parents: Array[mutable.Set[(PsddDecision, PsddElement)]]) {
  override def toString: String = (cloneAll, if (clones==null) null else clones.mkString("[",",","]"), if (parents==null) null else parents.mkString("[",",","]")).toString()
}
