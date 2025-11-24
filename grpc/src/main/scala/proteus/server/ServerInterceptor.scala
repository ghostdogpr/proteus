package proteus.server

import proteus.ProtobufCodec

/**
  * An interface for a server interceptor that can run on every request and can transform the context as well as the wrapper types for unary and streaming RPCs.
  *
  * @param InitialUnary the type of unary RPCs before the interceptor is applied.
  * @param Unary the type of unary RPCs after the interceptor is applied.
  * @param InitialStreaming the type of streaming RPCs before the interceptor is applied.
  * @param Streaming the type of streaming RPCs after the interceptor is applied.
  * @param InitialContext the type of context before the interceptor is applied.
  * @param Context the type of context after the interceptor is applied.
  */
trait ServerInterceptor[InitialUnary[_], Unary[_], InitialStreaming[_], Streaming[_], InitialContext, Context] {

  /**
    * Wraps the logic of a unary RPC to transform its context and wrapper types.
    */
  def unary[Req: ProtobufCodec, Resp: ProtobufCodec](io: Context => Unary[Resp]): (Req => InitialContext => InitialUnary[Resp])

  /**
    * Wraps the logic of a client streaming RPC to transform its context and wrapper types.
    */
  def clientStreaming[Req: ProtobufCodec, Resp: ProtobufCodec](
    io: Streaming[Req] => Context => Unary[Resp]
  ): (InitialStreaming[Req] => InitialContext => InitialUnary[Resp])

  /**
    * Wraps the logic of a server streaming RPC to transform its context and wrapper types.
    */
  def serverStreaming[Req: ProtobufCodec, Resp: ProtobufCodec](io: Context => Streaming[Resp]): (Req => InitialContext => InitialStreaming[Resp])

  /**
    * Wraps the logic of a server streaming RPC to transform its context and wrapper types.
    */
  def bidiStreaming[Req: ProtobufCodec, Resp: ProtobufCodec](
    io: Streaming[Req] => Context => Streaming[Resp]
  ): (InitialStreaming[Req] => InitialContext => InitialStreaming[Resp])
}

/**
  * An interface for a server interceptor that can run on every request and can transform the context.
  *
  * @param Unary the type of unary RPCs.
  * @param Streaming the type of streaming RPCs.
  * @param InitialContext the type of context before the interceptor is applied.
  * @param Context the type of context after the interceptor is applied.
  */
trait ServerContextInterceptor[Unary[_], Streaming[_], InitialContext, Context]
  extends ServerInterceptor[Unary, Unary, Streaming, Streaming, InitialContext, Context] {

  /**
    * Transforms the context before it is passed to the next interceptor.
    */
  def transformContext(context: InitialContext): Context

  /**
    * Wraps the logic of a unary RPC to transform its context.
    */
  def unary[Req: ProtobufCodec, Resp: ProtobufCodec](io: Context => Unary[Resp]): (Req => InitialContext => Unary[Resp]) =
    _ => ctx => io(transformContext(ctx))

  /**
    * Wraps the logic of a client streaming RPC to transform its context.
    */
  def clientStreaming[Req: ProtobufCodec, Resp: ProtobufCodec](
    io: Streaming[Req] => Context => Unary[Resp]
  ): (Streaming[Req] => InitialContext => Unary[Resp]) = stream => ctx => io(stream)(transformContext(ctx))

  /**
    * Wraps the logic of a server streaming RPC to transform its context.
    */
  def serverStreaming[Req: ProtobufCodec, Resp: ProtobufCodec](io: Context => Streaming[Resp]): (Req => InitialContext => Streaming[Resp]) = _ =>
    ctx => io(transformContext(ctx))

  /**
    * Wraps the logic of a bidirectional streaming RPC to transform its context.
    */
  def bidiStreaming[Req: ProtobufCodec, Resp: ProtobufCodec](
    io: Streaming[Req] => Context => Streaming[Resp]
  ): (Streaming[Req] => InitialContext => Streaming[Resp]) = stream => ctx => io(stream)(transformContext(ctx))
}

object ServerInterceptor {

  /**
    * Creates a new empty server interceptor that does nothing.
    */
  def empty[Unary[_], Streaming[_], Context]: ServerContextInterceptor[Unary, Streaming, Context, Context] =
    new ServerContextInterceptor[Unary, Streaming, Context, Context] {
      def transformContext(context: Context): Context = context
    }
}
