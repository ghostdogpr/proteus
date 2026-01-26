package proteus

import zio.blocks.schema.Schema

import proteus.Modifiers.*
import proteus.Modifiers.OneOfFlag.*

case class ServerReflectionRequest(host: String, messageRequest: MessageRequest) derives ProtobufCodec
case class ServerReflectionResponse(validHost: String, originalRequest: ServerReflectionRequest, messageResponse: MessageResponse)
  derives ProtobufCodec

sealed trait MessageRequest
object MessageRequest {
  case class FileByFileName(value: String) extends MessageRequest
  object FileByFileName       {
    given Schema[FileByFileName] = Schema[String].transform(FileByFileName(_), _.value)
  }
  case class FileContainingSymbol(value: String) extends MessageRequest
  object FileContainingSymbol {
    given Schema[FileContainingSymbol] = Schema[String].transform(FileContainingSymbol(_), _.value)
  }
  case class FileContainingExtension(containingType: String, extensionNumber: Int) extends MessageRequest
  case class AllExtensionNumbersOfType(value: String) extends MessageRequest
  object AllExtensionNumbersOfType {
    given Schema[AllExtensionNumbersOfType] = Schema[String].transform(AllExtensionNumbersOfType(_), _.value)
  }
  case class ListServices(value: String) extends MessageRequest
  object ListServices              {
    given Schema[ListServices] = Schema[String].transform(ListServices(_), _.value)
  }
}

sealed trait MessageResponse
object MessageResponse {
  case class FileDescriptorResponse(fileDescriptorProto: Array[Byte])                      extends MessageResponse
  case class AllExtensionNumbersResponse(baseTypeName: String, extensionNumber: List[Int]) extends MessageResponse
  case class ListServicesResponse(service: List[ServiceResponse])                          extends MessageResponse
  case class ErrorResponse(errorCode: Int, errorMessage: String)                           extends MessageResponse
}

case class ServiceResponse(name: String)

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
