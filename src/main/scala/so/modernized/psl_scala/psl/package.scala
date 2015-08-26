package so.modernized.psl_scala

import so.modernized.psl_scala.primitives._

package object psl {

  object predicates extends Predicates
  object functions extends FunctionConversion
  object literals extends LiteralConversion
  object variable extends VariableConversion
  object formulas extends FormulaConversion
  object everything extends Predicates with FunctionConversion with LiteralConversion with VariableConversion with FormulaConversion

}

