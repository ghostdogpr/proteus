package proteus
package server

import io.grpc.Metadata

/**
  * A class that contains the request and response metadata for a RPC.
  *
  * @param requestMetadata the metadata sent by the client.
  * @param responseMetadata the metadata sent by the server.
  */
case class RequestResponseMetadata(requestMetadata: Metadata, responseMetadata: Metadata)
