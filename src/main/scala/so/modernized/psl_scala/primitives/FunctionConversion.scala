package so.modernized.psl_scala.primitives

import edu.umd.cs.psl.database.ReadOnlyDatabase
import edu.umd.cs.psl.model.argument._
import edu.umd.cs.psl.model.function.ExternalFunction

trait FunctionConversion {

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




