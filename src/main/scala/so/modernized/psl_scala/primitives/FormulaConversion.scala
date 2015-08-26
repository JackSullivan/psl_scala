package so.modernized.psl_scala.primitives

import edu.umd.cs.psl.model.Model
import edu.umd.cs.psl.model.formula._
import edu.umd.cs.psl.model.kernel.rule.{ConstraintRuleKernel, CompatibilityRuleKernel}

trait FormulaConversion {
  implicit class FormulaMethods(f1:Formula) {

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
}


