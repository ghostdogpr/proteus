package proteus

import proteus.ProtoIR.*
import proteus.internal.Renderer

/**
  * A single detected change between two proto3 definitions.
  *
  * [[path]] gives the nesting context (e.g. `List("Outer", "Inner")`).
  */
enum Change {
  def path: List[String]

  case PackageChanged(path: List[String], oldName: Option[String], newName: Option[String])
  case FileAdded(path: List[String])
  case FileRemoved(path: List[String])
  case MessageMoved(path: List[String], name: String, oldFile: String, newFile: String)
  case EnumMoved(path: List[String], name: String, oldFile: String, newFile: String)
  case ImportAdded(path: List[String], importPath: String)
  case ImportRemoved(path: List[String], importPath: String)
  case ImportModifierChanged(path: List[String], importPath: String, oldModifier: Option[String], newModifier: Option[String])
  case MessageAdded(path: List[String], name: String)
  case MessageRemoved(path: List[String], name: String)
  case MessageRenamed(path: List[String], oldName: String, newName: String)
  case FieldAdded(path: List[String], name: String, number: Int)
  case FieldRemoved(path: List[String], name: String, number: Int, numberReserved: Boolean)
  case FieldNumberChanged(path: List[String], name: String, oldNumber: Int, newNumber: Int)
  case FieldRenamed(path: List[String], number: Int, oldName: String, newName: String)
  case FieldTypeChanged(path: List[String], name: String, number: Int, oldType: Type, newType: Type)
  case FieldTypeRefRenamed(path: List[String], name: String, number: Int, oldType: Type, newType: Type)
  case FieldOptionalityChanged(path: List[String], name: String, number: Int, wasOptional: Boolean)
  case FieldOrderChanged(path: List[String])
  case FieldOneOfChanged(path: List[String], name: String, number: Int, oldOneOf: Option[String], newOneOf: Option[String])
  case EnumAdded(path: List[String], name: String)
  case EnumRemoved(path: List[String], name: String)
  case EnumRenamed(path: List[String], oldName: String, newName: String)
  case EnumValueAdded(path: List[String], name: String, number: Int)
  case EnumValueRemoved(path: List[String], name: String, number: Int, numberReserved: Boolean)
  case EnumValueNumberChanged(path: List[String], name: String, oldNumber: Int, newNumber: Int)
  case EnumValueRenamed(path: List[String], number: Int, oldName: String, newName: String)
  case ReservedAdded(path: List[String], reserved: Reserved)
  case ReservedRemoved(path: List[String], reserved: Reserved)
  case OptionAdded(path: List[String], optionName: String)
  case OptionRemoved(path: List[String], optionName: String)
  case OptionChanged(path: List[String], optionName: String, oldValue: OptionVal, newValue: OptionVal)
  case ServiceAdded(path: List[String], name: String)
  case ServiceRemoved(path: List[String], name: String)
  case RpcAdded(path: List[String], name: String)
  case RpcRemoved(path: List[String], name: String)
  case RpcRequestTypeChanged(path: List[String], name: String, oldType: String, newType: String)
  case RpcResponseTypeChanged(path: List[String], name: String, oldType: String, newType: String)
  case RpcStreamingChanged(path: List[String], name: String, direction: String, wasStreaming: Boolean)
  case CommentAdded(path: List[String], element: String)
  case CommentRemoved(path: List[String], element: String)
  case CommentChanged(path: List[String], element: String)

  override def toString: String = {
    val prefix = if (path.isEmpty) "" else path.mkString(".") + ": "
    prefix + (this match {
      case PackageChanged(_, oldName, newName)                   => s"package changed from ${oldName.getOrElse("<none>")} to ${newName.getOrElse("<none>")}"
      case FileAdded(_)                                          => "file added"
      case FileRemoved(_)                                        => "file removed"
      case MessageMoved(_, name, oldFile, newFile)               => s"message '$name' moved from $oldFile to $newFile"
      case EnumMoved(_, name, oldFile, newFile)                  => s"enum '$name' moved from $oldFile to $newFile"
      case ImportAdded(_, importPath)                            => s"import '$importPath' added"
      case ImportRemoved(_, importPath)                          => s"import '$importPath' removed"
      case ImportModifierChanged(_, importPath, oldMod, newMod)  =>
        s"import '$importPath' modifier changed from ${oldMod.getOrElse("<none>")} to ${newMod.getOrElse("<none>")}"
      case MessageAdded(_, name)                                 => s"message '$name' added"
      case MessageRemoved(_, name)                               => s"message '$name' removed"
      case MessageRenamed(_, oldName, newName)                   => s"message renamed from '$oldName' to '$newName'"
      case FieldAdded(_, name, _)                                => s"field '$name' added"
      case FieldRemoved(_, name, _, reserved)                    => s"field '$name' removed${if (reserved) " (number reserved)" else ""}"
      case FieldNumberChanged(_, name, oldNum, newNum)           => s"field '$name' number changed from $oldNum to $newNum"
      case FieldRenamed(_, _, oldName, newName)                  => s"field '$oldName' renamed to '$newName'"
      case FieldTypeChanged(_, name, _, oldType, newType)        =>
        s"field '$name' type changed from ${Renderer.renderType(oldType)} to ${Renderer.renderType(newType)}"
      case FieldTypeRefRenamed(_, name, _, oldType, newType)     =>
        s"field '$name' type ref renamed from ${Renderer.renderType(oldType)} to ${Renderer.renderType(newType)}"
      case FieldOptionalityChanged(_, name, _, wasOptional)      =>
        s"field '$name' ${if (wasOptional) "optional removed" else "made optional"}"
      case FieldOrderChanged(_)                                  => "field order changed"
      case FieldOneOfChanged(_, name, _, oldOneOf, newOneOf)     =>
        val from = oldOneOf.map(o => s"oneof '$o'").getOrElse("top-level")
        val to   = newOneOf.map(o => s"oneof '$o'").getOrElse("top-level")
        s"field '$name' moved from $from to $to"
      case EnumAdded(_, name)                                    => s"enum '$name' added"
      case EnumRemoved(_, name)                                  => s"enum '$name' removed"
      case EnumRenamed(_, oldName, newName)                      => s"enum renamed from '$oldName' to '$newName'"
      case EnumValueAdded(_, name, _)                            => s"enum value '$name' added"
      case EnumValueRemoved(_, name, _, reserved)                => s"enum value '$name' removed${if (reserved) " (number reserved)" else ""}"
      case EnumValueNumberChanged(_, name, oldNum, newNum)       => s"enum value '$name' number changed from $oldNum to $newNum"
      case EnumValueRenamed(_, _, oldName, newName)              => s"enum value '$oldName' renamed to '$newName'"
      case ReservedAdded(_, reserved)                            => s"reserved ${Renderer.renderReservedValue(reserved)} added"
      case ReservedRemoved(_, reserved)                          => s"reserved ${Renderer.renderReservedValue(reserved)} removed"
      case OptionAdded(_, optionName)                            => s"option '$optionName' added"
      case OptionRemoved(_, optionName)                          => s"option '$optionName' removed"
      case OptionChanged(_, optionName, _, _)                    => s"option '$optionName' changed"
      case ServiceAdded(_, name)                                 => s"service '$name' added"
      case ServiceRemoved(_, name)                               => s"service '$name' removed"
      case RpcAdded(_, name)                                     => s"rpc '$name' added"
      case RpcRemoved(_, name)                                   => s"rpc '$name' removed"
      case RpcRequestTypeChanged(_, name, oldType, newType)      => s"rpc '$name' request type changed from $oldType to $newType"
      case RpcResponseTypeChanged(_, name, oldType, newType)     => s"rpc '$name' response type changed from $oldType to $newType"
      case RpcStreamingChanged(_, name, direction, wasStreaming) =>
        s"rpc '$name' $direction ${if (wasStreaming) "streaming removed" else "made streaming"}"
      case CommentAdded(_, element)                              => s"comment added on '$element'"
      case CommentRemoved(_, element)                            => s"comment removed on '$element'"
      case CommentChanged(_, element)                            => s"comment changed on '$element'"
    })
  }
}
