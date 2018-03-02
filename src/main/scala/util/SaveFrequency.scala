package main

/**
  * Created by jessa on 9/18/16.
  */
abstract class SaveFrequency

case class All(k: Int) extends SaveFrequency
case class Best(k: Int) extends SaveFrequency


