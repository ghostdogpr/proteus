package proteus

import zio.blocks.schema.Schema

import proteus.Modifiers.*
import proteus.Modifiers.OneOfFlag.*

case class ServerReflectionRequest(host: String, messageRequest: MessageRequest) derives Schema, ProtobufCodec
case class ServerReflectionResponse(validHost: String, originalRequest: ServerReflectionRequest, messageResponse: MessageResponse)
  derives Schema,
    ProtobufCodec

sealed trait MessageRequest derives Schema
object MessageRequest {
  case class FileByFileName(value: String) extends MessageRequest
  object FileByFileName       {
    given Schema[FileByFileName] = Schema.derived[FileByFileName].wrapTotal(FileByFileName(_), _.value)
  }
  case class FileContainingSymbol(value: String) extends MessageRequest
  object FileContainingSymbol {
    given Schema[FileContainingSymbol] = Schema.derived[FileContainingSymbol].wrapTotal(FileContainingSymbol(_), _.value)
  }
  case class FileContainingExtension(containingType: String, extensionNumber: Int) extends MessageRequest derives Schema
  case class AllExtensionNumbersOfType(value: String) extends MessageRequest
  object AllExtensionNumbersOfType {
    given Schema[AllExtensionNumbersOfType] =
      Schema.derived[AllExtensionNumbersOfType].wrapTotal[String](AllExtensionNumbersOfType(_), _.value)
  }
  case class ListServices(value: String) extends MessageRequest
  object ListServices              {
    given Schema[ListServices] = Schema.derived[ListServices].wrapTotal[String](ListServices(_), _.value)
  }
}

sealed trait MessageResponse derives Schema
object MessageResponse {
  case class FileDescriptorResponse(fileDescriptorProto: Array[Byte])                      extends MessageResponse
  case class AllExtensionNumbersResponse(baseTypeName: String, extensionNumber: List[Int]) extends MessageResponse derives Schema
  case class ListServicesResponse(service: List[ServiceResponse])                          extends MessageResponse derives Schema
  case class ErrorResponse(errorCode: Int, errorMessage: String)                           extends MessageResponse
}

case class ServiceResponse(name: String) derives Schema

given ProtobufDeriver = ProtobufDeriver
  .modifier[ServerReflectionRequest](reserved(2))
  .modifier[ServerReflectionResponse](reserved(3))
  .modifier[MessageRequest](oneOf(Inline))
  .modifier[MessageResponse](oneOf(Inline))
  .modifier[MessageRequest.FileContainingExtension](rename("ExtensionRequest"))
  .modifier[MessageResponse.AllExtensionNumbersResponse](rename("ExtensionNumberResponse"))
  .modifier[MessageResponse.ListServicesResponse](rename("ListServiceResponse"))

val serverReflectionInfoRpc = Rpc.bidiStreaming[ServerReflectionRequest, ServerReflectionResponse]("ServerReflectionInfo")
val serverReflectionService = Service("grpc.reflection.v1", "ServerReflection").rpc(serverReflectionInfoRpc)

/**
  * A client for the server reflection service.
  *
  * @param backend the backend to use for the client.
  */
def reflectionClient[Unary[_], Streaming[_]](
  backend: client.ClientBackend[Unary, Streaming]
): Unary[Streaming[ServerReflectionRequest] => Streaming[ServerReflectionResponse]] =
  backend.client(serverReflectionInfoRpc, serverReflectionService)
