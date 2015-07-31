package so.modernized.psl_scala

import so.modernized.psl_scala.union._

import edu.umd.cs.psl.database.loading.Inserter
import edu.umd.cs.psl.database.rdbms.{RDBMSUniqueIntID, RDBMSUniqueStringID}
import edu.umd.cs.psl.database.{DataStore, Partition, ReadOnlyDatabase}
import edu.umd.cs.psl.model.Model
import edu.umd.cs.psl.model.argument._
import edu.umd.cs.psl.model.atom.QueryAtom
import edu.umd.cs.psl.model.formula._
import edu.umd.cs.psl.model.function.ExternalFunction
import edu.umd.cs.psl.model.kernel.predicateconstraint.{DomainRangeConstraintKernel, DomainRangeConstraintType, SymmetryConstraintKernel}
import edu.umd.cs.psl.model.kernel.rule.{CompatibilityRuleKernel, ConstraintRuleKernel}
import edu.umd.cs.psl.model.kernel.setdefinition.SetDefinitionKernel
import edu.umd.cs.psl.model.predicate.{PredicateFactory, SpecialPredicate, StandardPredicate}
import edu.umd.cs.psl.model.set.aggregator.EntityAggregatorFunction
import edu.umd.cs.psl.model.set.term._
import edu.umd.cs.psl.ui.aggregators.AggregateSetEquality

import scala.collection.JavaConverters._
import scala.reflect.runtime.universe._

package object psl {

  type PslType = union[String]#or[Int]#or[Double]#or[UniqueID]
  type IdType = union[Int]#or[String]

  def extract[T : prove[PslType]#containsType](gt: GroundTerm): T = gt match {
    case intTerm: IntegerAttribute => intTerm.getValue.asInstanceOf[T]
    case dblTerm: DoubleAttribute => dblTerm.getValue.asInstanceOf[T]
    case strTerm: StringAttribute => strTerm.getValue.asInstanceOf[T]
    case idTerm: UniqueID => idTerm.asInstanceOf[T]
  }

  object AutoConvertIds {
    implicit def idTypeToId[Id : prove[IdType]#containsType](id:Id) = id match {
      case iId:Int    => new RDBMSUniqueIntID(iId)
      case sId:String => new RDBMSUniqueStringID(sId)
    }
  }

  object FunctionConversions {



    implicit def fn1ToPslFn[A: prove[PslType]#containsType : TypeTag](fn: A => Double): ExternalFunction = new ExternalFunction {

      def getValue(db: ReadOnlyDatabase, args: GroundTerm*) = fn(extract[A](args(0)))

      val getArity = 1
      val getArgumentTypes = Array(argType[A])
    }

    implicit def fn2ToPslFn[A: prove[PslType]#containsType : TypeTag, B: prove[PslType]#containsType : TypeTag](fn: (A, B) => Double): ExternalFunction = new ExternalFunction {

      def getValue(db: ReadOnlyDatabase, args: GroundTerm*) = fn(extract[A](args(0)), extract[B](args(1)))

      val getArity = 2
      val getArgumentTypes = Array(argType[A], argType[B])
    }

    implicit def fn3ToPslFn[A : prove[PslType]#containsType : TypeTag, B : prove[PslType]#containsType : TypeTag, C : prove[PslType]#containsType : TypeTag](fn: (A, B, C) => Double): ExternalFunction = new ExternalFunction {

      def getValue(db: ReadOnlyDatabase, args: GroundTerm*) = fn(extract[A](args(0)), extract[B](args(1)), extract[C](args(2)))

      val getArity = 3
      val getArgumentTypes = Array(argType[A], argType[B], argType[C])
    }
  }

  implicit def pslTypeToGroundTerm[T : prove[PslType]#containsType](t:T):GroundTerm = t match {
    case i:Int => new IntegerAttribute(i)
    case d:Double => new DoubleAttribute(d)
    case s:String => new StringAttribute(s)
    case id:UniqueID => id
  }

  implicit class FormulaMethods(val f1:Formula) extends AnyVal{

    def &(f2:Formula) = new Conjunction(f1, f2)
    def |(f2:Formula) = new Disjunction(f1, f2)
    def >>(f2:Formula) = new Rule(f1, f2)
    def unary_~ = new Negation(f1)
    def where(weight:Double, isSquared:Boolean = true)(implicit m:Model) {
      val (form, w) = if(weight > 0.0) (f1, weight) else (~f1, -weight)
      m.addKernel(new CompatibilityRuleKernel(form, w, isSquared))
    }
  }

  def constraint(r:Formula)(implicit m:Model) {m addKernel new ConstraintRuleKernel(r)}

  implicit class VarStringContext(val sc:StringContext) extends AnyVal {
    def v(args:Any*) = new Variable(sc.parts.mkString)
  }

  implicit class VariableMethods(val v1:Variable) extends AnyVal {
    def ^(v2:Variable) = new QueryAtom(SpecialPredicate.NonSymmetric, v1, v2)
    def !(v2:Variable) = new QueryAtom(SpecialPredicate.NotEqual, v1, v2)
    def ===(v2:Variable) = new QueryAtom(SpecialPredicate.Equal, v1, v2)
  }

  private def argType[A : prove[PslType]#containsType](implicit aTpe:TypeTag[A]) = typeOf[A] match {
    case i if i <:< typeOf[Int]      => ArgumentType.Integer
    case d if d <:< typeOf[Double]   => ArgumentType.Double
    case s if s <:< typeOf[String]   => ArgumentType.String
    case u if u <:< typeOf[UniqueID] => ArgumentType.UniqueID
  }

  def id[ID : prove[IdType]#containsType](idType:ID):UniqueID = idType match {
    case sId:String => new RDBMSUniqueStringID(sId)
    case iId:Int => new RDBMSUniqueIntID(iId)
  }

  trait Symmetry {
    this: R[_,_] =>
     model addKernel new SymmetryConstraintKernel(pred)
  }

  trait Functional {this: PslType =>}
  trait PartialFunctional {this: PslType =>}

  object R {

    protected trait Inversion {
      this:R[_,_] =>

      abstract override def apply(ts:Term*) = new QueryAtom(pred, ts.reverse:_*)
    }
  }

  implicit def decorate[T : prove[PslType]#containsType](t:T):T with Functional with PartialFunctional = t.asInstanceOf[T with Functional with PartialFunctional]

  case class R[A : prove[PslType]#containsType, B :prove[PslType]#containsType](predName:String)(implicit ds:DataStore, m:Model, aTpe:TypeTag[A], bTpe:TypeTag[B]) {
    protected[psl] val pred = PredicateFactory.getFactory.createStandardPredicate(predName, argType[A], argType[B])
    protected val model = m

    ds.registerPredicate(pred)

    typeOf[A] match {
      case a if a <:< typeOf[Functional] => model addKernel new DomainRangeConstraintKernel(pred, DomainRangeConstraintType.Functional)
      case a if a <:< typeOf[PartialFunctional] => model addKernel new DomainRangeConstraintKernel(pred, DomainRangeConstraintType.PartialFunctional)
      case otw => ()
    }

    typeOf[B] match {
      case b if b <:< typeOf[Functional] => model addKernel new DomainRangeConstraintKernel(pred, DomainRangeConstraintType.InverseFunctional)
      case b if b <:< typeOf[PartialFunctional] => model addKernel new DomainRangeConstraintKernel(pred, DomainRangeConstraintType.PartialInverseFunctional)
      case otw => ()
    }



    def apply(ts:Term*) = new QueryAtom(pred, ts:_*)

    private var inserter:Inserter = null
    private var lastPart:Partition = null
    def load(part:Partition)(a:A, b:B): Unit = {
      if(inserter==null || part != lastPart) {
        inserter = ds.getInserter(pred, part)
        lastPart = part
      }
      inserter.insert(a.asInstanceOf[AnyRef],b.asInstanceOf[AnyRef])
    }

    private var labeledInserter:Inserter = null
    private var labeledLastPart:Partition = null
    def loadLabeled[A1, B1](part:Partition)(a:A1, b:B1, conf:Double)(implicit toA:A1 => A, toB:B1 => B): Unit = {
      if(labeledInserter==null || part != labeledLastPart) {
        labeledInserter = ds.getInserter(pred, part)
        labeledLastPart = part
      }
      labeledInserter.insertValue(conf, toA(a).asInstanceOf[AnyRef],toB(b).asInstanceOf[AnyRef])
    }

    def set(setName:String=predName + "Set") = new SetPredicate(setName, pred, ds, m)

    import R._
    def inverse = if (this.isInstanceOf[Symmetry]) {
      new R[A,B](predName + "__inverse") with Symmetry with Inversion
    } else {
      new R[A,B](predName + "__inverse") with Inversion
    }
  }

  implicit def rToPred(r:R[_,_]) = r.pred
  implicit def rTravToPredTrav(rs:Traversable[R[_,_]]) = rs map rToPred

  case class f(predName:String, ext:ExternalFunction) {
    private val pred = PredicateFactory.getFactory.createFunctionalPredicate(predName,ext)
    def apply(ts:Term*) = new QueryAtom(pred, ts:_*)
  }

  private var auxVarIdx = 0
  private def nextAuxId = {
    val res = auxVarIdx
    auxVarIdx += 1
    res
  }

  private def auxVariable = new Variable("aux__" + nextAuxId)

  protected class SetPredicate(predName:String, rootPred:StandardPredicate, ds:DataStore, m:Model) {

    private var agg:EntityAggregatorFunction = new AggregateSetEquality()
    def aggregator(agg:EntityAggregatorFunction) = {this.agg = agg; this}

    protected type VarWithPath = union[Variable]#or[(Variable, Seq[R[_,_]])]

    private def makeCompoundTerm[V : prove[VarWithPath]#containsType](vwp:V):BasicSetTerm = {
      vwp match {
        case v:Variable => makeCompoundTerm(v -> Seq.empty[R[_,_]])
        case tup:(Variable, Seq[R[_,_]]) => val v = tup._1; val path = tup._2
          if (path.isEmpty) {
            new VariableSetTerm(v, ArgumentType.UniqueID)
          } else {
            var v1 = v
            var v2 = auxVariable
            val formula = path.map { pred =>
              val qa = pred(v1, v2)
              v1 = v2
              v2 = auxVariable
              qa
            }.reduce[Formula](_ & _)
            new FormulaSetTerm(formula, v2, Set(v).asJava)
          }
      }
    }

    def apply[V1 : prove[VarWithPath]#containsType, V2:prove[VarWithPath]#containsType](vwp1:V1, vwp2:V2) = {
      val t1 = makeCompoundTerm(vwp1)
      val t2 = makeCompoundTerm(vwp2)

      val (variables, argTypes) = t2.getAnchorVariables(t1.getAnchorVariables(new VariableTypeMap))
        .asScala.toSeq.sortBy(_._1.getName).unzip

      val auxPred = PredicateFactory.getFactory.createStandardPredicate(predName, argTypes:_*)
      ds.registerPredicate(auxPred)

      m addKernel new SetDefinitionKernel(auxPred, t1, t2, variables.toArray, rootPred, agg)
      new QueryAtom(auxPred, variables:_*)
    }
  }

}

