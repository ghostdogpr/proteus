package proteus

import zio.blocks.schema.Modifier.config
import zio.blocks.schema.Schema

@config("proteus.reserved", "2")
case class ServerReflectionRequest(host: String, messageRequest: MessageRequest) derives Schema
@config("proteus.reserved", "3")
case class ServerReflectionResponse(validHost: String, originalRequest: ServerReflectionRequest, messageResponse: MessageResponse) derives Schema

@config("proteus.inline", "true")
sealed trait MessageRequest derives Schema
object MessageRequest {
  case class FileByFileName(value: String) extends MessageRequest
  object FileByFileName       {
    given Schema[FileByFileName] = Schema.derived[FileByFileName].wrap[String](v => Right(FileByFileName(v)), _.value)
  }
  case class FileContainingSymbol(value: String) extends MessageRequest
  object FileContainingSymbol {
    given Schema[FileContainingSymbol] = Schema.derived[FileContainingSymbol].wrap[String](v => Right(FileContainingSymbol(v)), _.value)
  }
  @config("proteus.rename", "ExtensionRequest")
  case class FileContainingExtension(containingType: String, extensionNumber: Int) extends MessageRequest
  case class AllExtensionNumbersOfType(value: String) extends MessageRequest
  object AllExtensionNumbersOfType {
    given Schema[AllExtensionNumbersOfType] =
      Schema.derived[AllExtensionNumbersOfType].wrap[String](v => Right(AllExtensionNumbersOfType(v)), _.value)
  }
  case class ListServices(value: String) extends MessageRequest
  object ListServices              {
    given Schema[ListServices] = Schema.derived[ListServices].wrap[String](v => Right(ListServices(v)), _.value)
  }
}

@config("proteus.inline", "true")
enum MessageResponse derives Schema {
  case FileDescriptorResponse(fileDescriptorProto: Array[Byte])
  @config("proteus.rename", "ExtensionNumberResponse") case AllExtensionNumbersResponse(baseTypeName: String, extensionNumber: List[Int])
  @config("proteus.rename", "ListServiceResponse") case ListServicesResponse(service: List[ServiceResponse])
  case ErrorResponse(errorCode: Int, errorMessage: String)
}

case class ServiceResponse(name: String) derives Schema

given deriver: ProtobufDeriver = ProtobufDeriver()

val serverReflectionInfoRpc = Rpc.bidiStreaming[ServerReflectionRequest, ServerReflectionResponse]("ServerReflectionInfo")
val serverReflectionService = Service("grpc.reflection.v1", "ServerReflection").rpc(serverReflectionInfoRpc)

def reflectionClient[Unary[_], Streaming[_]](
  backend: client.ClientBackend[Unary, Streaming]
): Unary[Streaming[ServerReflectionRequest] => Streaming[ServerReflectionResponse]] =
  backend.client(serverReflectionService, serverReflectionInfoRpc)
