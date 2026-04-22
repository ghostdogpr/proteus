package proteus

import scala.deriving.Mirror
import scala.quoted.*

import zio.blocks.schema.derive.{ModifierOverride, ModifierTermOverrideByType}
import zio.blocks.typeid.TypeId

private[proteus] object ProtobufDeriverMacros {

  def termModifiersImpl[A: Type](
    self: Expr[ProtobufDeriver],
    entries: Expr[Seq[FieldMod[?]]],
    typeIdExpr: Expr[TypeId[A]]
  )(using q: Quotes): Expr[ProtobufDeriver] = {
    import q.reflect.*

    val xs = entries match {
      case Varargs(es) => es
      case _           =>
        report.errorAndAbort("Expected varargs of FieldMod entries.", entries)
    }

    val (labels, mirrorLabel, memberKind) = mirrorInfo[A]

    val pairs: Seq[(String, Expr[FieldMod[?]])] = xs.map { e =>
      val n = literalNameOf(e)
      if (!labels.contains(n)) {
        report.errorAndAbort(s"$memberKind '$n' does not exist in $mirrorLabel.", e)
      }
      (n, e)
    }

    val overrideExprs: Seq[Expr[ModifierOverride]] = pairs.map { case (n, e) =>
      '{ ModifierTermOverrideByType($typeIdExpr, ${ Expr(n) }, $e.modifier) }
    }

    '{ $self.appendModifierOverrides(${ Expr.ofSeq(overrideExprs) }) }
  }

  private def literalNameOf(using Quotes)(e: Expr[FieldMod[?]]): String = {
    import quotes.reflect.*
    e.asTerm.tpe.widenTermRefByName match {
      case AppliedType(_, List(ConstantType(StringConstant(s)))) => s
      case other                                                 =>
        report.errorAndAbort(
          s"Could not extract literal name; use Modifiers.field(\"name\", ...) with a string literal. Got type: ${other.show}",
          e
        )
    }
  }

  private def mirrorInfo[A: Type](using q: Quotes): (Set[String], String, String) = {
    import q.reflect.*

    def labelsOf(t: Type[?]): List[String] = t match {
      case '[EmptyTuple] => Nil
      case '[h *: ts]    =>
        val head = TypeRepr.of(using Type.of[h]) match {
          case ConstantType(StringConstant(s)) => s
          case other                           =>
            report.errorAndAbort(s"Expected string literal label, got: ${other.show}")
        }
        head :: labelsOf(Type.of[ts])
      case _             =>
        report.errorAndAbort(s"Expected tuple of labels, got: ${TypeRepr.of(using t).show}")
    }

    def stringConst(tpe: TypeRepr, fallback: => String): String = tpe match {
      case ConstantType(StringConstant(s)) => s
      case _                               => fallback
    }

    Expr.summon[Mirror.Of[A]] match {
      case Some('{ $m: Mirror.ProductOf[A] { type MirroredElemLabels = ls; type MirroredLabel = ml } }) =>
        (labelsOf(Type.of[ls]).toSet, stringConst(TypeRepr.of[ml], Type.show[A]), "Field")
      case Some('{ $m: Mirror.SumOf[A] { type MirroredElemLabels = ls; type MirroredLabel = ml } })     =>
        (labelsOf(Type.of[ls]).toSet, stringConst(TypeRepr.of[ml], Type.show[A]), "Case")
      case _                                                                                            =>
        report.errorAndAbort(s"Could not find a Mirror for ${Type.show[A]}.")
    }
  }
}
