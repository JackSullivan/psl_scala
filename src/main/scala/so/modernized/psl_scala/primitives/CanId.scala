package so.modernized.psl_scala.primitives

import edu.umd.cs.psl.database.rdbms.{RDBMSUniqueIntID, RDBMSUniqueStringID}
import edu.umd.cs.psl.model.argument.UniqueID

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

