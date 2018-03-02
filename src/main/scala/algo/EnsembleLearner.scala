
/**
  * Created by MikeLyt on 1/29/17.
  */

package algo

import com.typesafe.config.ConfigFactory
import java.io.{File, PrintWriter}

import scala.util.Random
import scala.annotation.tailrec
import spire.math._
import operations.{PsddManager, PsddOperation, PsddQueries}
import structure.{Data, DataSets, PsddDecision, VtreeNode}
import util.{Parser, Util}
import main._
import algo._

import sdd.{SddManager, Vtree}



abstract class EnsembleLearner(datasetName: String) {
  //read config as the initialization
  val configRead = ConfigFactory.load()
  val vtreeName = configRead.getString("vtrees."+datasetName)
  val datasetPath = configRead.getString("datasets."+datasetName)
  val vtreeFiles = Array(vtreeName+"_miBlossom.vtree",vtreeName+"_miMetis.vtree",
    vtreeName+"_ord.vtree",vtreeName+"_rand1.vtree",vtreeName+"_rand2.vtree",
    vtreeName+"_rand3.vtree")
  val validData = Data.readFromFile(new File(datasetPath+".valid.data"))
  val validSize = BigDecimal.decimal(validData.weights.sum)
  val validDataWeightsBigDecimal = validData.weights.map(BigDecimal.decimal(_))
  val testData = Data.readFromFile(new File(datasetPath+".test.data"))
  val testSize = BigDecimal.decimal(testData.weights.sum)
  val testDataWeightsBigDecimal = testData.weights.map(BigDecimal.decimal(_))
  val parameterCalculator = Parser.parseParameterCalculator(configRead.getString("general.parameterCalculator"))
  val scorer = Parser.parseScorer(configRead.getString("general.scorer"))
  val minOperationCompletionType = Parser.parseOperationCompletionType("minimal")
  val completeOperationCompletionType = Parser.parseOperationCompletionType("maxDepth-3")
  val savingFrequency = Parser.parseSavingFrequency(configRead.getString("general.savingFrequency"))
  val maxNumberOfCloneParents = configRead.getInt("general.maxNumberOfCloneParents")

  protected def initializeOutput(dataSetOutputDir:String): PrintWriter = {
    Output.init(new File(dataSetOutputDir))
    val outputForMixture = new PrintWriter(new File(dataSetOutputDir+"progress.txt"))
    return outputForMixture
  }

  def learn(): Unit = {}  //implement by concerete ensemble learner class
}

class EM(datasetName:String, numLearners:Int) extends EnsembleLearner(datasetName){
  val iterationTimeForEM = configRead.getInt("EM.iterationTimeForEM")
  val iteratorNumberForParameterChange = configRead.getInt("EM.iteratorNumberForParameterChange")
  val iteratorNumberForStructureChange = configRead.getInt("EM.iteratorNumberForStructureChange")
  val hybridChangeFrequency = configRead.getInt("EM.hybridChangeFrequency")
  val hybridStartingProbability = configRead.getInt("EM.hybridStartingProbability")
  val randomGenerator = new Random()


  protected override def initializeOutput(dataSetOutputDir:String): PrintWriter = {
    val outputForMixture =  super.initializeOutput(dataSetOutputDir)
    0 until numLearners foreach(i=>(new File(dataSetOutputDir+i.toString+"/")).mkdirs())
    outputForMixture.println("It;Time;TotalTime;Size;ComponentsWeights[Array];TestLl")
    outputForMixture.flush()
    return outputForMixture
  }

  protected def changeStructure(psddMgr:PsddManager, psdd:PsddDecision,splitOperationFinder:SplitOperationFinder, cloneOperationFinder: CloneOperationFinder, nbOfTrainingSamples:Double):PsddOperation = {

    val levelOnWhichOperationsArePerformed = randomGenerator.nextInt(psddMgr.totalVtreeLevels+1)
    val nodesWithParents = PsddQueries.decisionNodesWithParents(psdd).filter{case (node,_)=>node.vtree.level==levelOnWhichOperationsArePerformed}

    //find split operations
    val splitOps = nodesWithParents.flatMap { case (node, _) => node.elements.map { element => splitOperationFinder.find(node, element, psdd) } }.filterNot(_._1 == null)
    val bestSplitOp = if (!splitOps.isEmpty) splitOps.maxBy(_._2) else null

    //find clone operations
    val cloneOps = nodesWithParents.map { case (node, parents) => cloneOperationFinder.find(node, parents, psdd) }.filterNot(_._1 == null)
    val bestCloneOp = if (!cloneOps.isEmpty) cloneOps.maxBy(x=>(x._2,x._3)) else null

    //find best Op
    val candidates = Array(bestSplitOp,bestCloneOp).filterNot(_==null)
    val bestOp = if (candidates.length > 0) candidates.maxBy(_._2)._1 else null

    //execute the best op
    if (bestOp != null) {
      val executeResult = bestOp.execute()
      if (executeResult) bestOp else null
    }else{
      null
    }
  }

  protected def buildMixture(psdds:IndexedSeq[PsddDecision],componentsWeights:IndexedSeq[BigDecimal],output:PrintWriter): Unit ={
    //combine to form an ensembler learner
    val mixtureTestLlonEachExample = testData.backend.map(example=> (psdds.map(PsddQueries.bigDecimalProb(_,example)),componentsWeights).zipped.map(_*_).sum)
    val testLl = (mixtureTestLlonEachExample.map(x=>spire.math.log(x)/testSize),testDataWeightsBigDecimal).zipped.map(_*_).sum
    val totalSize = psdds.map(psdd=>PsddQueries.size(psdd)).sum

    val time = (System.nanoTime()-currentMixtureTime)*math.pow(10,-9)
    mixtureTimer+= time
    currentMixtureTime = System.nanoTime()

    output.println(Array(getIt(),time,mixtureTimer,totalSize,componentsWeights.map("%1.2f".format(_)).mkString(";"),testLl).mkString(";"))
    output.flush()
  }

  protected def updateDataInPsdd(psddMgr:PsddManager, psdd:PsddDecision, trainData: Data): Unit = {
    psddMgr.distributeData(psdd,new DataSets(trainData,validData,testData))
    psddMgr.calculateParameters(psdd,parameterCalculator,psdd)
  }

  protected def reportComponent(output:PrintWriter,psdd:PsddDecision, time: Double, totalTime:Double, op:PsddOperation): Unit = {
    val trainLl = PsddQueries.logLikelihood(psdd, "train")/psdd.data.train.total
    val size = PsddQueries.size(psdd)
    output.println(Array(getIt(), time, totalTime, size, op, trainLl).mkString(";"))
    output.flush()
  }


  protected def calculateComponentsWeights(clusterData:IndexedSeq[Data]): IndexedSeq[BigDecimal] = {
    val samplesTotalNumber = BigDecimal.decimal(clusterData.map(_.weights.sum).sum)
    for (i<-0 until numLearners) yield BigDecimal.decimal(clusterData(i).weights.sum)/samplesTotalNumber
  }

  var lastValidLl = BigDecimal.decimal(-1000.0)
  protected def whetherImprovedOnValidLl(psdds:IndexedSeq[PsddDecision],componentsWeights:IndexedSeq[BigDecimal]): Boolean = {
    val mixtureValidLlonEachExample = validData.backend.map(example=> (psdds.map(PsddQueries.bigDecimalProb(_,example)),componentsWeights).zipped.map(_*_).sum)
    val validLl = (mixtureValidLlonEachExample.map(x=>spire.math.log(x)/validSize),validDataWeightsBigDecimal).zipped.map(_*_).sum
    val improved = validLl > lastValidLl
    lastValidLl = validLl
    return improved
  }

  var mixtureTimer = 0.0
  val learnerTimer = Array.fill[Double](numLearners)(0.0)
  var currentLearnerTime = System.nanoTime()
  var currentMixtureTime = System.nanoTime()

  protected def updateTimer(learnerIndex:Int): Double ={
    val time = (System.nanoTime()-currentLearnerTime)*math.pow(10,-9)
    learnerTimer(learnerIndex) += time
    currentLearnerTime = System.nanoTime()
    return time
  }

  protected def getLearnerTotalTime(learnerIndex:Int): Double = {
    return learnerTimer(learnerIndex)
  }

  var it = 0

  protected def updateIt(): Unit = {
    it +=1
  }

  protected def getIt(): Int = it

  @tailrec
  final def kMeansClustering(samples: Array[Array[Int]], weights:Array[Double], centers: IndexedSeq[Array[Double]] = null,nbOfIterations:Int = 0): Array[Int] = {
    val c = if (centers == null) {for (i<-0 until numLearners) yield samples(randomGenerator.nextInt(samples.length)).map(_.toDouble)} else centers

    //reassign points to closest clusters
    val pos = samples.map{sample=>
      val distance = c.map(Util.euclideanDistance(sample,_))
      distance.indexOf(distance.min)
    }

    //recalculate each cluster's center
    val newCenters = for (i<-0 until numLearners) yield {
      val filteredPos = pos.zipWithIndex.filter { case (p, _) => p == i }
      if (!filteredPos.isEmpty) {
        val filteredSamples = filteredPos.map { case (_, index) => samples(index) }
        val filteredWeights = filteredPos.map { case (_, index) => weights(index) }
        val weightedSamples = (filteredSamples, filteredWeights).zipped.map { case (s, w) => s.map(_ * w) }
        weightedSamples.transpose.map(x => (x.sum / filteredWeights.sum))
      }else{
        samples(randomGenerator.nextInt(samples.length)).map(_.toDouble)
      }
    }

    if (c.corresponds(newCenters)(sampleCenterComp) || nbOfIterations>=2000) return pos else kMeansClustering(samples,weights,newCenters,nbOfIterations+1)
  }
  val sampleCenterComp = (a:Array[Double],b:Array[Double]) => if (a.size != b.size) false else (a,b).zipped.forall{case(aa,bb)=> Math.abs(aa-bb)<0.0001}
}


class SoftEM(datasetName:String, numLearners:Int) extends EM(datasetName,numLearners){
  val dataSetOutputDir =  configRead.getString("EM.SoftEM.outputdir")+datasetName+"/"
  val lambdaWeight = configRead.getDouble("EM.SoftEM.lambdaWeight")

  override def learn(): Unit = {
    //initialize output files
    val outputForMixture = super.initializeOutput(dataSetOutputDir)
    val outputForLearners = for (i<-0 until numLearners) yield new PrintWriter(new File(dataSetOutputDir+i.toString+"/progress.txt"))
    outputForLearners.foreach{x=>
      x.println("It;Time;TotalTime;Size;Op;TrainLL")
      x.flush()
    }

    //read training samples
    val (trainingSamples,weights) = Util.readLines(configRead.getString("datasets."+datasetName)+".train.data")
    val trainingData = Data.readFromFile(new File(configRead.getString("datasets."+datasetName)+".train.data"))

    //cluster initial training samples
    var pos = kMeansClustering(trainingSamples,weights)

    //initiliaze psdd learners
    val sddMgr = new SddManager(Vtree.read(vtreeFiles(0)))
    val vtree = VtreeNode.read(new File(vtreeFiles(0)))
    val psddMgr = new PsddManager(sddMgr)
    var trainingSampleClusters = clusterTrainingSamplesAccordingToPos(trainingSamples,weights,pos)
    val psdds = for (i<- 0 until numLearners) yield psddMgr.newPsdd(vtree,new DataSets(trainingSampleClusters(i),validData,testData),parameterCalculator)

    //initialize operation finders
    val minSplitOperationFinder = new SplitOperationFinder(psddMgr,minOperationCompletionType,scorer,parameterCalculator)
    val minCloneOperationFinder = new CloneOperationFinder(psddMgr,minOperationCompletionType,scorer,parameterCalculator,maxNumberOfCloneParents)
    val completeSplitOperationFinder = new SplitOperationFinder(psddMgr,completeOperationCompletionType,scorer,parameterCalculator)
    val completeCloneOperationFinder = new CloneOperationFinder(psddMgr,completeOperationCompletionType,scorer,parameterCalculator,maxNumberOfCloneParents)

    //report scores with initial psdds
    0 until numLearners foreach {i =>
      val time = updateTimer(i)
      reportComponent(outputForLearners(i), psdds(i), time, getLearnerTotalTime((i)), null)
    }
    buildMixture(psdds, calculateComponentsWeights(trainingSampleClusters), outputForMixture)
    updateIt()

    while (mixtureTimer < iterationTimeForEM){
      //update structure
      outputForMixture.println("Strucutre Changed")
      outputForMixture.flush()
      0 until numLearners foreach { i => updateDataInPsdd(psddMgr, psdds(i), trainingSampleClusters(i)) }
      val structureChangeUntilItNumber = getIt()+iteratorNumberForStructureChange

      val possibilityForMinOperation = Math.max(0,hybridStartingProbability - (mixtureTimer/hybridChangeFrequency))

      do{
        0 until numLearners foreach {i =>
          val op = if (randomGenerator.nextInt(10)<possibilityForMinOperation) {
            changeStructure(psddMgr, psdds(i), minSplitOperationFinder, minCloneOperationFinder,trainingSampleClusters(i).weights.sum)
          }else{
            changeStructure(psddMgr, psdds(i), completeSplitOperationFinder, minCloneOperationFinder, trainingSampleClusters(i).weights.sum)
          }
          val time = updateTimer(i)
          reportComponent(outputForLearners(i), psdds(i), time, getLearnerTotalTime((i)), op)
        }
        buildMixture(psdds, calculateComponentsWeights(trainingSampleClusters), outputForMixture)
        updateIt()
      }while(getIt()<structureChangeUntilItNumber && whetherImprovedOnValidLl(psdds,calculateComponentsWeights(trainingSampleClusters)))

      //optimize parameters
      outputForMixture.println("Strucutre Fixed")
      outputForMixture.flush()
      val parameterOptimizeUntilItNumber = getIt()+iteratorNumberForParameterChange
      do{
        trainingSampleClusters = redistributeTrainingData(trainingData.backend, trainingData.weights, psdds)
        0 until numLearners foreach { i =>
          updateDataInPsdd(psddMgr, psdds(i), trainingSampleClusters(i))
          val time = updateTimer(i)
          reportComponent(outputForLearners(i), psdds(i), time, getLearnerTotalTime((i)), null)
        }
        buildMixture(psdds, calculateComponentsWeights(trainingSampleClusters), outputForMixture)
        updateIt()
      }while(getIt()<parameterOptimizeUntilItNumber && whetherImprovedOnValidLl(psdds,calculateComponentsWeights(trainingSampleClusters)))
      trainingSampleClusters = redistributeTrainingData(trainingData.backend, trainingData.weights, psdds)
    }

    outputForMixture.close()
    outputForLearners.foreach(_.close())
  }

  private def clusterTrainingSamplesAccordingToPos(samples:Array[Array[Int]],weights:Array[Double],pos:Array[Int]):IndexedSeq[Data] = {
    require(samples.length == pos.length)
    require(pos.length == pos.length)
    for (learnerIndex<- 0 until numLearners) yield {
      val softWeights = pos.zipWithIndex.map{case(p,i)=> if (p==learnerIndex) weights(i)-(numLearners-1)*lambdaWeight else lambdaWeight}
      Data.readFromArray(samples,softWeights)
    }
  }

  private def redistributeTrainingData(samples:Array[Map[Int,Boolean]], weights:Array[Double], psdds:IndexedSeq[PsddDecision]): Array[Data] = {
    val redistributedWeights = (samples,weights).zipped.map{case(sample,weight)=>
      val probs = psdds.map(PsddQueries.bigDecimalProb(_,sample)).toArray
      probs.map(p=>(p/probs.sum*weight).toDouble)
    }
    redistributedWeights.transpose.map(w=> Data.readDataAndWeights((samples,w).zipped.toArray))
  }
}



/*class HardEM(datasetName:String, numLearners:Int) extends EM(datasetName,numLearners){
  val dataSetOutputDir =  configRead.getString("EM.HardEM.outputdir")+datasetName+"/"

  override def learn(): Unit = {
    //initialize output files
    val outputForMixture = super.initializeOutput(dataSetOutputDir)
    val outputForLearners = for (i<-0 until numLearners) yield new PrintWriter(new File(dataSetOutputDir+i.toString+"/progress.txt"))
    outputForLearners.foreach{x=>
      x.println("It;Time;TotalTime;Size;Op;TrainLL")
      x.flush()
    }

    //read training samples
    val (trainingSamples,weights) = Util.readLines(configRead.getString("datasets."+datasetName)+".train.data")
    val trainingData = Data.readFromFile(new File(configRead.getString("datasets."+datasetName)+".train.data"))

    //cluster initial training samples
    var pos = kMeansClustering(trainingSamples,weights)

    //initiliaze psdd learners
    val sddMgr = new SddManager(Vtree.read(vtreeFiles(0)))
    val vtree = VtreeNode.read(new File(vtreeFiles(0)))
    val psddMgr = new PsddManager(sddMgr)
    var trainingSampleClusters = clusterTrainingSamplesAccordingToPos(trainingSamples,weights,pos)
    val psdds = for (i<- 0 until numLearners) yield psddMgr.newPsdd(vtree,new DataSets(trainingSampleClusters(i),validData,testData),parameterCalculator)

    //initialize operation finders
    val splitOperationFinder = new SplitOperationFinder(psddMgr,splitOperationCompletionType,scorer,parameterCalculator)
    val cloneOperationFinder = new CloneOperationFinder(psddMgr,cloneOperationCompletionType,scorer,parameterCalculator,maxNumberOfCloneParents)

    //report scores with initial psdds
    0 until numLearners foreach {i =>
      val time = updateTimer(i)
      reportComponent(outputForLearners(i), psdds(i), time, getLearnerTotalTime((i)), null)
    }
    buildMixture(psdds, calculateComponentsWeights(trainingSampleClusters), outputForMixture)
    updateIt()

    while (getIt()<iterationNumberForEM){
      //update structure
      outputForMixture.println("Strucutre Changed")
      outputForMixture.flush()
      0 until numLearners foreach { i => updateDataInPsdd(psddMgr, psdds(i), trainingSampleClusters(i)) }
      val structureChangeUntilItNumber = getIt()+iteratorNumberForStructureChange

      do{
        0 until numLearners foreach {i =>
          val op = changeStructure(psddMgr, psdds(i), splitOperationFinder,cloneOperationFinder,trainingSampleClusters(i).weights.sum)
          val time = updateTimer(i)
          reportComponent(outputForLearners(i), psdds(i), time, getLearnerTotalTime((i)), op)
        }
        buildMixture(psdds, calculateComponentsWeights(trainingSampleClusters), outputForMixture)
        updateIt()
      }while(getIt()<structureChangeUntilItNumber && whetherImprovedOnValidLl(psdds,calculateComponentsWeights(trainingSampleClusters)))

      //optimize parameters
      outputForMixture.println("Strucutre Fixed")
      outputForMixture.flush()
      val parameterOptimizeUntilItNumber = getIt()+iteratorNumberForParameterChange
      do{
        pos = redistributeTrainingData(trainingData.backend, psdds, pos)
        trainingSampleClusters = clusterTrainingDataAndWeightsAccordingToPos(trainingData.backend,trainingData.weights,pos)
        0 until numLearners foreach { i =>
          updateDataInPsdd(psddMgr, psdds(i), trainingSampleClusters(i))
          val time = updateTimer(i)
          reportComponent(outputForLearners(i), psdds(i), time, getLearnerTotalTime((i)), null)
        }
        buildMixture(psdds, calculateComponentsWeights(trainingSampleClusters), outputForMixture)
        updateIt()
      }while(getIt()<parameterOptimizeUntilItNumber && !whetherConverged() && whetherImprovedOnValidLl(psdds,calculateComponentsWeights(trainingSampleClusters)))
      if (!whetherConverged()){
        pos = redistributeTrainingData(trainingData.backend, psdds, pos)
        trainingSampleClusters = clusterTrainingDataAndWeightsAccordingToPos(trainingData.backend,trainingData.weights,pos)
      }else{
        resetWhetherConverged()
      }
    }

    outputForMixture.close()
    outputForLearners.foreach(_.close())
  }

  private def redistributeTrainingData(samples:Array[Map[Int,Boolean]], psdds:IndexedSeq[PsddDecision], oldPos:Array[Int]): Array[Int] = {
    var redistributionAmount = 0
    val pos = (samples,oldPos).zipped.map{case(sample,p)=>
      val prob = psdds.map(PsddQueries.logProb(_,sample))
      val newP = prob.indexOf(prob.max)
      if (newP!=p) redistributionAmount+=1
      newP
    }
    updateWhetherConverged(redistributionAmount)
    return pos
  }

  private def clusterTrainingSamplesAccordingToPos(samples:Array[Array[Int]],weights:Array[Double],pos:Array[Int]):IndexedSeq[Data] = {
    require(samples.length == pos.length)
    require(pos.length == pos.length)
    for (learnerIndex<- 0 until numLearners) yield {
      val filteredPos = pos.zipWithIndex.filter{case(p,_)=>p==learnerIndex}
      val filteredSamples = filteredPos.map{case(_,i)=>samples(i)}
      val filteredWeights = filteredPos.map{case(_,i)=>weights(i)}
      Data.readFromArray(filteredSamples,filteredWeights)
    }
  }

  private def clusterTrainingDataAndWeightsAccordingToPos(samples:Array[Map[Int,Boolean]],weights:Array[Double],pos:Array[Int]):IndexedSeq[Data] = {
    require(samples.length == pos.length && pos.length == weights.length)
    for (learnerIndex<- 0 until numLearners) yield Data.readDataAndWeights(pos.zipWithIndex.filter{case(p,i) => p==learnerIndex}.map{case(p,i) => (samples(i),weights(i))})
  }

  var converged = false
  private def updateWhetherConverged(redistributionAmount:Int):Unit = {
    converged = (redistributionAmount==0)
  }

  private def whetherConverged():Boolean = converged

  private def resetWhetherConverged():Unit = {
    converged = false
  }
}*/




