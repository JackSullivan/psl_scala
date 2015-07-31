package so.modernized.psl_scala

import scala.annotation.unchecked.uncheckedVariance

package object union {

  type union[A] = {
    type or[B] = Disjunction[not[A]]#or[B]#apply
    type apply = not[not[A]]
  }

  type prove[U] = { type containsType[X] = not[not[X]] <:< U }

  sealed trait not[-A] {
    type or[B] = Disjunction[A @uncheckedVariance]#or[B]#apply
    type apply = not[A @uncheckedVariance]
    type unapply = (A @uncheckedVariance)
  }

  private trait Disjunction[A] {
    type or[B] = Disjunction[A with not[B]]
    type apply = not[A]
  }
}
