package so.modernized.psl_scala.examples

import edu.umd.cs.psl.application.inference.MPEInference
import edu.umd.cs.psl.application.learning.weight.maxlikelihood.MaxLikelihoodMPE
import edu.umd.cs.psl.config.ConfigManager
import edu.umd.cs.psl.database.{DatabasePopulator, Partition, DataStore}
import edu.umd.cs.psl.database.rdbms.{RDBMSUniqueIntID, RDBMSDataStore}
import edu.umd.cs.psl.database.rdbms.driver.H2DatabaseDriver
import edu.umd.cs.psl.model.Model
import edu.umd.cs.psl.model.argument.{GroundTerm, UniqueID}
import edu.umd.cs.psl.model.predicate.StandardPredicate
import edu.umd.cs.psl.util.database.Queries

import so.modernized.psl_scala.psl._
import so.modernized.psl_scala.psl.FunctionConversions._

import org.apache.log4j.{Logger, Level}

import scala.collection.JavaConverters._
import scala.io.Source

// this is a copy of the basic example from psl-groovy using the scala DSL
object BasicExample {

  def levenshteinDistance(s1:String, s2: String) = {
    if (s1.length == 0) s2.length
    else if (s2.length == 0) s1.length
    else {
      val d = Array.ofDim[Int](s1.length + 1, s2.length + 1)
      for (i <- 0 to s1.length)
        d(i)(0) = i
      for (i <- 0 to s2.length)
        d(0)(i) = i
      for (i <- 1 to s1.length; j <- 1 to s2.length) {
        val cost = if (s1(i - 1) == s2(j - 1)) 0 else 1
        d(i)(j) = math.min(d(i - 1)(j) + 1, math.min(d(i)(j - 1) + 1, d(i - 1)(j - 1) + cost))
      }
      d(s1.length)(s2.length)
    }
  }

  val levenshteinSimilarity = (s1:String, s2:String) =>
    math.max(s1.length, s2.length) match {
      case 0 => 1.0
      case denom =>
        val sim = levenshteinDistance(s1,s2).toDouble / denom
        if (sim > 0.5) sim else 0.0
    }


  lazy val logging = Logger.getLogger(this.getClass)

  def readFile(fileName:String) = Source.fromFile(fileName).getLines().flatMap {line =>
    line.split("""\s+""") match {
      case Array(x,y) => Some(x.toInt -> y.toInt)
      case otw => None
    }
  }

  def readLabelFile(fileName:String) = Source.fromFile(fileName).getLines().flatMap { line =>
    line.split("""\s+""") match {
      case Array(x,y,conf) => Some((x.toInt, y.toInt, conf.toDouble))
      case otw => None
    }
  }

  // takes one arg, which should be the path to the psl-groovy example resources dir
  def main(args:Array[String]): Unit = {

    logging.setLevel(Level.ALL)

    val config = ConfigManager.getManager.getBundle("basic-example")

    val defaultPath = sys.props("java.io.tmpdir") + "/basic-example"
    val dbPath = config.getString("dbpath", defaultPath)

    implicit val ds:DataStore = new RDBMSDataStore(new H2DatabaseDriver(H2DatabaseDriver.Type.Disk, dbPath, true), config)
    implicit val m:Model = new Model

    val Network = R[UniqueID, UniqueID]("Network")
    val Name = R[UniqueID, String]("Name")
    val Knows = R[UniqueID, UniqueID]("Knows")
    val SamePerson = new R[UniqueID with PartialFunctional, UniqueID with PartialFunctional]("SamePerson") with Symmetry
    val SameName = f("SameName", levenshteinSimilarity)

    val snA:GroundTerm = ds.getUniqueID(1)
    val snB:GroundTerm = ds.getUniqueID(2)

    ((Network(v"A", snA) & Network(v"B", snB)
      & Name(v"A", v"X") & Name(v"B", v"Y")
      & SameName(v"X", v"Y")) >> SamePerson(v"A", v"B")).where(weight = 5.0, isSquared = true)

    ((Network(v"A", snA) & Network(v"B", snB)
      & SamePerson(v"A", v"B")
      & Knows(v"A", v"Friend1") & Knows(v"B", v"Friend2")) >> SamePerson(v"Friend1", v"Friend2")).where(weight = 3.2, isSquared = true)

    SamePerson(v"A",v"B").where(weight = -1.0, isSquared = true)

    println(m)

    val evidencePart = new Partition(0)

    val networkA = Seq (
      (1, "John Braker"),
      (2, "Mr. Jack Ressing"),
      (3, "Peter Larry Smith"),
      (4, "Tim Barosso"),
      (5, "Jessica Pannillo"),
      (6, "Peter Smithsonian"),
      (7, "Miranda Parker")
    ).map{case (i,n) => id(i) -> n}

    val networkB = Seq(
      (11, "Johny Braker"),
      (12, "Jack Ressing"),
      (13, "PL S."),
      (14, "Tim Barosso"),
      (15, "J. Panelo"),
      (16, "Gustav Heinrich Gans"),
      (17, "Otto v. Lautern")
    ).map{case (i,n) => id(i) -> n}

    networkA foreach {case (id, name) => Name.load(evidencePart)(id, name)}
    networkB foreach {case (id, name) => Name.load(evidencePart)(id, name)}

    val dir = args(0)

    val networkData = readFile(dir + "sn_network.txt").collect{case(i1,i2) => id(i1) -> id(i2)}
    val knowsData = readFile(dir + "sn_knows.txt").collect{case(i1,i2)  => id(i1) -> id(i2)}

    networkData foreach {case (i1, i2) => Network.load(evidencePart)(i1, i2)}
    knowsData foreach {case (i1, i2) => Knows.load(evidencePart)(i1, i2)}

    val targetPart = new Partition(1)
    val preds:Set[StandardPredicate] = Set(Network, Name, Knows)
    val db = ds.getDatabase(targetPart, preds.asJava, evidencePart)

    val population = Map(v"UserA" -> networkA.map{case (id,_) => id.asInstanceOf[GroundTerm]}.toSet.asJava,
      v"UserB" -> networkB.map{case (id,_) => id.asInstanceOf[GroundTerm]}.toSet.asJava).asJava

    val populator = new DatabasePopulator(db)
    populator.populate(SamePerson(v"UserA", v"UserB"), population)
    populator.populate(SamePerson(v"UserB", v"UserA"), population)

    val inf = new MPEInference(m, db, config)
    inf.mpeInference()
    inf.close()

    println("Hand-tunes weights output")
    Queries.getAllAtoms(db, SamePerson).asScala.toSeq.sortBy(-_.getValue) foreach { atom =>
      val Array(a,b) = atom.getArguments
      val ai = extract[RDBMSUniqueIntID](a)
      val bi = extract[RDBMSUniqueIntID](b)

      (networkA.toMap.get(ai),  networkB.toMap.get(bi)) match {
        case (Some(n1), Some(n2)) => println(n1, n2, atom.getValue)
        case otw => ()
      }

      val truePart = new Partition(2)
      val truth = readLabelFile(dir + "sn_align.txt")
      truth.foreach { case(a,b,conf) => SamePerson.loadLabeled(truePart)(id(a),id(b), conf) }
      val targetSet = Set(SamePerson:StandardPredicate).asJava
      val trueDb = ds.getDatabase(truePart, targetSet)

      val weightLearning = new MaxLikelihoodMPE(m, db, trueDb, config)
      weightLearning.learn()
      weightLearning.close()

      println("Learned model:\n" + m.toString)


      val inf = new MPEInference(m, db, config)
      inf.mpeInference()
      inf.close()

      val groundings = Queries.getAllAtoms(db, SamePerson).asScala

      println("learned weights output")
      groundings.toSeq.sortBy(-_.getValue) foreach { atom =>
        val Array(a,b) = atom.getArguments
        val ai = extract[RDBMSUniqueIntID](a)
        val bi = extract[RDBMSUniqueIntID](b)
        (networkA.toMap.get(ai),  networkB.toMap.get(bi)) match {
          case (Some(n1), Some(n2)) => println(n1, n2, atom.getValue)
          case otw => ()
        }
      }

    }
  }
}

