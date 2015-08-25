package so.modernized.psl_scala.primitives

import edu.umd.cs.psl.database.ReadOnlyDatabase
import edu.umd.cs.psl.database.rdbms.{RDBMSUniqueIntID, RDBMSUniqueStringID}

import edu.umd.cs.psl.model.argument._
import edu.umd.cs.psl.model.function.ExternalFunction

trait PSLUnapplicable[A] {
  type T <: GroundTerm
  def argType:ArgumentType
  def unapply(v:GroundTerm):Option[A]
  def apply(a:A):T
}

object PSLUnapplicable {
  implicit object PSLInt extends PSLUnapplicable[Int] {
    type T = IntegerAttribute
    val argType = ArgumentType.Integer
    def unapply(v:GroundTerm) = v match {
      case i:IntegerAttribute => Some(i.getValue.intValue())
      case _ => None
    }
    def apply(i:Int) = new IntegerAttribute(i)
  }

  implicit object PSLDouble extends PSLUnapplicable[Double] {
    type T = DoubleAttribute
    val argType = ArgumentType.Double
    def unapply(v:GroundTerm) = v match {
      case d:DoubleAttribute => Some(d.getValue.doubleValue())
      case _ => None
    }
    def apply(d:Double) = new DoubleAttribute(d)
  }

  implicit object PSLString extends PSLUnapplicable[String] {
    type T = StringAttribute
    val argType = ArgumentType.String
    def unapply(v:GroundTerm) = v match {
      case s:StringAttribute => Some(new StringAttribute(s))
      case _ => None
    }
    def apply(s:String) = new StringAttribute(s)
  }

  implicit object PSLId extends PSLUnapplicable[UniqueID] {
    type T = UniqueID
    val argType = ArgumentType.UniqueID
    def unapply(v:GroundTerm) = v match {
      case id:UniqueID => Some(id)
      case _ => None
    }
    def apply(id:UniqueID) = id
  }

  def wrap[A](a:A)(implicit c:PSLUnapplicable[A]) = c(a)
  def extract[A](v:GroundTerm)(implicit conv:PSLUnapplicable[A]):A = conv.unapply(v).get
}

trait CanId[A] {
  type IdType <: UniqueID
  def unapply(id:UniqueID):Option[A]
  def apply(a:A):IdType
}

object CanId {
  implicit object StringId extends CanId[String] {
    type IdType = RDBMSUniqueStringID

    def unapply(id: UniqueID) = id match {
      case sId:RDBMSUniqueStringID => Some(sId.getID)
      case _ => None
    }

    def apply(a: String) = new RDBMSUniqueStringID(a)
  }

  implicit object IntId extends CanId[Int] {
    type IdType = RDBMSUniqueIntID

    def unapply(id: UniqueID) = id match {
      case sId:RDBMSUniqueIntID => Some(sId.getID)
      case _ => None
    }

    def apply(a: Int) = new RDBMSUniqueIntID(a)
  }
}

object LiteralConversion {
  implicit def wrap[A](a:A)(implicit c:PSLUnapplicable[A]) = c(a)
  def id[A](a:A)(implicit c:CanId[A]) = c(a)
}

object FunctionConversion {

  implicit def fn1ToPSl[A](fn:A => Double)(implicit aC:PSLUnapplicable[A]) = new ExternalFunction {

    def getValue(readOnlyDatabase: ReadOnlyDatabase, groundTerms: GroundTerm*) = groundTerms match {
      case Seq(aC(a)) => fn(a)
      case otw => throw new IllegalArgumentException("Expected Seq(%s), got %s".format(aC.argType, otw))
    }

    val getArity = 1
    val getArgumentTypes = Array(aC.argType)
  }

  implicit def fn2ToPSl[A, B](fn:(A, B) => Double)(implicit aC:PSLUnapplicable[A], bC:PSLUnapplicable[B]) = new ExternalFunction {

    def getValue(readOnlyDatabase: ReadOnlyDatabase, groundTerms: GroundTerm*) = groundTerms match {
      case Seq(aC(a), bC(b)) => fn(a, b)
      case otw => throw new IllegalArgumentException("Expected Seq(%s), got %s".format(Seq(aC.argType, bC.argType).mkString(", "), otw))
    }

    val getArity = 2
    val getArgumentTypes = Array(aC.argType, bC.argType)
  }

  implicit def fn3ToPSl[A, B, C](fn:(A, B, C) => Double)(implicit aC:PSLUnapplicable[A], bC:PSLUnapplicable[B], cC:PSLUnapplicable[C]) = new ExternalFunction {
    def getValue(readOnlyDatabase: ReadOnlyDatabase, groundTerms: GroundTerm*) = groundTerms match {
      case Seq(aC(a), bC(b), cC(c)) => fn(a, b, c)
      case otw => throw new IllegalArgumentException("Expected Seq(%s), got %s".format(Seq(aC.argType, bC.argType, cC.argType).mkString(", "), otw))
    }

    def getArity = 3

    def getArgumentTypes = Array(aC.argType, bC.argType, cC.argType)
  }

  implicit def fn4ToPSl[A, B, C, D](fn:(A, B, C, D) => Double)(implicit aC:PSLUnapplicable[A], bC:PSLUnapplicable[B], cC:PSLUnapplicable[C], dC:PSLUnapplicable[D]) = new ExternalFunction {
    def getValue(readOnlyDatabase: ReadOnlyDatabase, groundTerms: GroundTerm*) = groundTerms match {
      case Seq(aC(a), bC(b), cC(c), dC(d)) => fn(a, b, c, d)
      case otw => throw new IllegalArgumentException("Expected Seq(%s), got %s".format(Seq(aC.argType, bC.argType, cC.argType, dC.argType).mkString(", "), otw))
    }

    def getArity = 4

    def getArgumentTypes = Array(aC.argType, bC.argType, cC.argType, dC.argType)
  }

}
