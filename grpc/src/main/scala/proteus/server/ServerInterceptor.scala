package proteus.server

trait ServerInterceptor[Unary[_], Streaming[_], InitialContext, Context] {
  def unary[A](io: Context => Unary[A]): (InitialContext => Unary[A])
  def stream[A](io: Context => Streaming[A]): (InitialContext => Streaming[A])
}

object ServerInterceptor {
  def empty[Unary[_], Streaming[_], Context]: ServerInterceptor[Unary, Streaming, Context, Context] =
    new ServerInterceptor[Unary, Streaming, Context, Context] {
      def unary[A](io: Context => Unary[A]): (Context => Unary[A])          = io
      def stream[A](io: Context => Streaming[A]): (Context => Streaming[A]) = io
    }
}
