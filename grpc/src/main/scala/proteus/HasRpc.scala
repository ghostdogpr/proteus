package proteus

import scala.annotation.implicitNotFound

@implicitNotFound(
  "The service definition is missing RPCs that were registered on the server. Make sure all RPCs added via .rpc(...) are also defined in the Service."
)
sealed trait HasAllRpcs[-S, R]

object HasAllRpcs {
  given [S <: R, R]: HasAllRpcs[S, R] = null.asInstanceOf[HasAllRpcs[S, R]]
}

@implicitNotFound(
  "The server is missing RPCs that are defined in the service. Make sure all RPCs in the Service definition are registered on the server via .rpc(...)."
)
sealed trait HasAllServerRpcs[-S, R]

object HasAllServerRpcs {
  given [S <: R, R]: HasAllServerRpcs[S, R] = null.asInstanceOf[HasAllServerRpcs[S, R]]
}

@implicitNotFound(
  "The RPC is not registered in the service. Make sure the RPC is included in the Service definition."
)
sealed trait HasRpc[-S, R]

object HasRpc {
  given [S <: R, R]: HasRpc[S, R] = null.asInstanceOf[HasRpc[S, R]]
}
