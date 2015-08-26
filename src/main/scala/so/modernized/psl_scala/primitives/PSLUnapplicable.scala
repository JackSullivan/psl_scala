package so.modernized.psl_scala.primitives

import edu.umd.cs.psl.model.argument._

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
      case s:StringAttribute => Some(s.getValue)
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

