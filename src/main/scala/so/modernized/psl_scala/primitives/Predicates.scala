package so.modernized.psl_scala.primitives

import edu.umd.cs.psl.database.loading.Inserter
import edu.umd.cs.psl.database.{DataStore, Partition}
import edu.umd.cs.psl.model.Model
import edu.umd.cs.psl.model.argument.Term
import edu.umd.cs.psl.model.atom.QueryAtom
import edu.umd.cs.psl.model.function.ExternalFunction
import edu.umd.cs.psl.model.kernel.predicateconstraint.{DomainRangeConstraintKernel, DomainRangeConstraintType, SymmetryConstraintKernel}
import edu.umd.cs.psl.model.predicate.{PredicateFactory, StandardPredicate}

trait Predicates {
  sealed trait ArgumentQualifications
  object Functional extends ArgumentQualifications
  object PartialFunctional extends ArgumentQualifications
  object ArgNone extends ArgumentQualifications

  sealed trait PredicateQualifications
  trait Symmetry extends PredicateQualifications
  trait Inversion extends PredicateQualifications
  object PredNone extends PredicateQualifications

  def R[A,B](predName:String)(implicit ds:DataStore, model:Model, aC:PSLUnapplicable[A], bC:PSLUnapplicable[B]) =
    Relation[A,B](predName)(ArgNone, ArgNone, PredNone)(ds, model, aC, bC)

  def R[A,B](predName:String, aQ:ArgumentQualifications, bQ:ArgumentQualifications, p:PredicateQualifications)(implicit ds:DataStore, model:Model, aC:PSLUnapplicable[A], bC:PSLUnapplicable[B]) =
    Relation[A,B](predName)(aQ, bQ, p)(ds, model, aC, bC)

  protected[Predicates] case class Relation[A, B](predName:String)(aQ:ArgumentQualifications, bQ:ArgumentQualifications, pQ:PredicateQualifications)
                                                 (implicit ds:DataStore, model:Model, aC:PSLUnapplicable[A], bC:PSLUnapplicable[B]) {

    protected[primitives] val pred = PredicateFactory.getFactory.createStandardPredicate(predName, aC.argType, bC.argType)

    ds registerPredicate pred

    aQ match {
      case Functional => model addKernel new DomainRangeConstraintKernel(pred, DomainRangeConstraintType.Functional)
      case PartialFunctional => model addKernel new DomainRangeConstraintKernel(pred, DomainRangeConstraintType.PartialFunctional)
      case ArgNone => ()
    }

    bQ match {
      case Functional => model addKernel new DomainRangeConstraintKernel(pred, DomainRangeConstraintType.InverseFunctional)
      case PartialFunctional => model addKernel new DomainRangeConstraintKernel(pred, DomainRangeConstraintType.PartialInverseFunctional)
      case ArgNone => ()
    }

    pQ match {
      case _:Symmetry => model addKernel new SymmetryConstraintKernel(pred)
      case _ => ()
    }

    def apply(a:Term, b:Term) = pQ match {
      case _:Inversion => new QueryAtom(pred, b, a)
      case _ => new QueryAtom(pred, a, b)
    }

    private var inserter:Inserter = null
    private var lastPart:Partition = null
    def load(part:Partition)(a:A, b:B): Unit = {
      if(inserter==null || part != lastPart) {
        inserter = ds.getInserter(pred, part)
        lastPart = part
      }
      inserter.insert(a.asInstanceOf[AnyRef], b.asInstanceOf[AnyRef])
    }

    private var labeledInserter:Inserter = null
    private var labeledLastPart:Partition = null
    def loadLabeled(part:Partition)(a:A, b:B, conf:Double=1.0) {
      if(labeledInserter==null || part != labeledLastPart) {
        labeledInserter = ds.getInserter(pred, part)
        labeledLastPart = part
      }
      labeledInserter.insertValue(conf, a.asInstanceOf[AnyRef], b.asInstanceOf[AnyRef])
    }
  }

  implicit def r2Pred(r:Relation[_,_]) = r.pred
  implicit def rTrav2Pred(rs:Traversable[Relation[_,_]]):Traversable[StandardPredicate] = rs map r2Pred

  case class f(predName:String, ext:ExternalFunction) {
    private val pred = PredicateFactory.getFactory.createFunctionalPredicate(predName,ext)
    def apply(ts:Term*) = new QueryAtom(pred, ts:_*)
  }
}

