package so.modernized.psl_scala.primitives

import edu.umd.cs.psl.model.argument.{Term, Variable}
import edu.umd.cs.psl.model.atom.QueryAtom
import edu.umd.cs.psl.model.predicate.SpecialPredicate


trait VariableConversion {
  implicit class VarStringContext(sc:StringContext) {
    def v(args:Any*) = new Variable(sc.parts.mkString)
  }

  implicit class VariableMethods(v1:Variable) {
    def ^(v2:Variable) = new QueryAtom(SpecialPredicate.NonSymmetric, v1, v2)
    def !(v2:Variable) = new QueryAtom(SpecialPredicate.NotEqual, v1, v2)
    def ===(v2:Variable) = new QueryAtom(SpecialPredicate.Equal, v1, v2)
  }
}

object PSLVar {
  def unapply(t:Term) = t match {
    case v:Variable => Some(v)
    case _ => None
  }
}
