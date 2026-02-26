package proteus

sealed trait HasAllRpcs[S, R]

object HasAllRpcs {
  inline given [S, R]: HasAllRpcs[S, R] = ${ HasRpcMacros.checkAllRpcs[S, R] }
}

sealed trait HasAllServerRpcs[S, R]

object HasAllServerRpcs {
  inline given [S, R]: HasAllServerRpcs[S, R] = ${ HasRpcMacros.checkAllServerRpcs[S, R] }
}

sealed trait HasRpc[S, R]

object HasRpc {
  inline given [S, R]: HasRpc[S, R] = ${ HasRpcMacros.checkRpc[S, R] }
}
