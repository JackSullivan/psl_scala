package so.modernized.psl_scala.primitives

trait LiteralConversion {
  def pslWrap[A](a:A)(implicit c:PSLUnapplicable[A]) = c(a)
  def id[A](a:A)(implicit c:CanId[A]) = c(a)
}

