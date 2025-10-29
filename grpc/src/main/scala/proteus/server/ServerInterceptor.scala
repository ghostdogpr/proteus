package proteus.server

import proteus.ProtobufCodec

trait ServerInterceptor[InitialUnary[_], Unary[_], InitialStreaming[_], Streaming[_], InitialContext, Context] {
  def unary[Req: ProtobufCodec, Resp: ProtobufCodec](io: Context => Unary[Resp]): (Req => InitialContext => InitialUnary[Resp])
  def clientStreaming[Req: ProtobufCodec, Resp: ProtobufCodec](
    io: Streaming[Req] => Context => Unary[Resp]
  ): (InitialStreaming[Req] => InitialContext => InitialUnary[Resp])
  def serverStreaming[Req: ProtobufCodec, Resp: ProtobufCodec](io: Context => Streaming[Resp]): (Req => InitialContext => InitialStreaming[Resp])
  def bidiStreaming[Req: ProtobufCodec, Resp: ProtobufCodec](
    io: Streaming[Req] => Context => Streaming[Resp]
  ): (InitialStreaming[Req] => InitialContext => InitialStreaming[Resp])
}

trait ServerContextInterceptor[Unary[_], Streaming[_], InitialContext, Context]
  extends ServerInterceptor[Unary, Unary, Streaming, Streaming, InitialContext, Context] {
  def transformContext(context: InitialContext): Context
  def unary[Req: ProtobufCodec, Resp: ProtobufCodec](io: Context => Unary[Resp]): (Req => InitialContext => Unary[Resp])                   =
    _ => ctx => io(transformContext(ctx))
  def clientStreaming[Req: ProtobufCodec, Resp: ProtobufCodec](
    io: Streaming[Req] => Context => Unary[Resp]
  ): (Streaming[Req] => InitialContext => Unary[Resp]) = stream => ctx => io(stream)(transformContext(ctx))
  def serverStreaming[Req: ProtobufCodec, Resp: ProtobufCodec](io: Context => Streaming[Resp]): (Req => InitialContext => Streaming[Resp]) = _ =>
    ctx => io(transformContext(ctx))
  def bidiStreaming[Req: ProtobufCodec, Resp: ProtobufCodec](
    io: Streaming[Req] => Context => Streaming[Resp]
  ): (Streaming[Req] => InitialContext => Streaming[Resp]) = stream => ctx => io(stream)(transformContext(ctx))
}

object ServerInterceptor {
  def empty[Unary[_], Streaming[_], Context]: ServerContextInterceptor[Unary, Streaming, Context, Context] =
    new ServerContextInterceptor[Unary, Streaming, Context, Context] {
      def transformContext(context: Context): Context = context
    }
}
