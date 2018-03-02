package main

import algo.{SoftEM}

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length < 3) {
      println("Please provide the name of the ensemble learner, the name of the dataset and the number of learners.")
    }else {
      val learner = args(0) match {
        case "SoftEM" => new SoftEM(args(1),args(2).toInt)
      }
      learner.learn()
    }
  }
}
