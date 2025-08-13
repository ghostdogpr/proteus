package proteus
package server

import io.grpc.Metadata

case class RequestResponseMetadata(requestMetadata: Metadata, responseMetadata: Metadata)
