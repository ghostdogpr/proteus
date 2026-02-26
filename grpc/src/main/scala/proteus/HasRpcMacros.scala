package proteus

import scala.quoted.*

object HasRpcMacros {

  private def decomposeIntersection(using Quotes)(tpe: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    tpe.dealias match {
      case AndType(left, right)        => decomposeIntersection(left) ++ decomposeIntersection(right)
      case t if t =:= TypeRepr.of[Any] => Nil
      case t                           => List(t)
    }
  }

  private def rpcName(using Quotes)(tpe: quotes.reflect.TypeRepr): String = {
    import quotes.reflect.*
    tpe match {
      case TermRef(_, name) => name
      case _                => tpe.show
    }
  }

  def checkAllRpcs[S: Type, R: Type](using Quotes): Expr[HasAllRpcs[S, R]] = {
    import quotes.reflect.*
    if (TypeRepr.of[S] <:< TypeRepr.of[R])
      '{ null.asInstanceOf[HasAllRpcs[S, R]] }
    else {
      val sComponents = decomposeIntersection(TypeRepr.of[S])
      val rComponents = decomposeIntersection(TypeRepr.of[R])
      val missing     = rComponents.filterNot(r => sComponents.exists(s => s =:= r))
      val names       = missing.map(rpcName).mkString(", ")
      report.errorAndAbort(
        s"The service definition is missing the following RPCs that were registered on the server: $names. " +
          "Make sure all RPCs added via .rpc(...) are also defined in the Service."
      )
    }
  }

  def checkAllServerRpcs[S: Type, R: Type](using Quotes): Expr[HasAllServerRpcs[S, R]] = {
    import quotes.reflect.*
    if (TypeRepr.of[S] <:< TypeRepr.of[R])
      '{ null.asInstanceOf[HasAllServerRpcs[S, R]] }
    else {
      val sComponents = decomposeIntersection(TypeRepr.of[S])
      val rComponents = decomposeIntersection(TypeRepr.of[R])
      val missing     = rComponents.filterNot(r => sComponents.exists(s => s =:= r))
      val names       = missing.map(rpcName).mkString(", ")
      report.errorAndAbort(
        s"The server is missing implementations for the following RPCs: $names. " +
          "Make sure all RPCs in the Service definition are registered on the server via .rpc(...)."
      )
    }
  }

  def checkRpc[S: Type, R: Type](using Quotes): Expr[HasRpc[S, R]] = {
    import quotes.reflect.*
    if (TypeRepr.of[S] <:< TypeRepr.of[R])
      '{ null.asInstanceOf[HasRpc[S, R]] }
    else {
      val name = rpcName(TypeRepr.of[R])
      report.errorAndAbort(
        s"The RPC '$name' is not registered in the service. " +
          "Make sure the RPC is included in the Service definition."
      )
    }
  }
}
