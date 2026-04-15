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
  case FieldOptionalityChanged(path: List[String], name: String, number: Int, wasOptional: Boolean)
  case FieldOrderChanged(path: List[String])
  case FieldOneOfChanged(path: List[String], name: String, number: Int, oldOneOf: Option[String], newOneOf: Option[String])
  case OneOfAdded(path: List[String], name: String)
  case OneOfRemoved(path: List[String], name: String)
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

  override def toString: String = {
    val prefix = if (path.isEmpty) "" else path.mkString(".") + ": "
    prefix + (this match {
      case PackageChanged(_, oldName, newName)                    => s"package changed from ${oldName.getOrElse("<none>")} to ${newName.getOrElse("<none>")}"
      case ImportAdded(_, importPath)                             => s"import '$importPath' added"
      case ImportRemoved(_, importPath)                           => s"import '$importPath' removed"
      case ImportModifierChanged(_, importPath, oldMod, newMod)   =>
        s"import '$importPath' modifier changed from ${oldMod.getOrElse("<none>")} to ${newMod.getOrElse("<none>")}"
      case MessageAdded(_, name)                                  => s"message '$name' added"
      case MessageRemoved(_, name)                                => s"message '$name' removed"
      case MessageRenamed(_, oldName, newName)                    => s"message renamed from '$oldName' to '$newName'"
      case FieldAdded(_, name, number)                            => s"field '$name' ($number) added"
      case FieldRemoved(_, name, number, reserved)                => s"field '$name' ($number) removed${if (reserved) " (number reserved)" else ""}"
      case FieldNumberChanged(_, name, oldNum, newNum)            => s"field '$name' number changed from $oldNum to $newNum"
      case FieldRenamed(_, number, oldName, newName)              => s"field $number renamed from '$oldName' to '$newName'"
      case FieldTypeChanged(_, name, number, oldType, newType)    =>
        s"field '$name' ($number) type changed from ${Renderer.renderType(oldType)} to ${Renderer.renderType(newType)}"
      case FieldOptionalityChanged(_, name, number, wasOptional)  =>
        s"field '$name' ($number) ${if (wasOptional) "optional removed" else "made optional"}"
      case FieldOrderChanged(_)                                   => "field order changed"
      case FieldOneOfChanged(_, name, number, oldOneOf, newOneOf) =>
        s"field '$name' ($number) moved from ${oldOneOf.getOrElse("top-level")} to ${newOneOf.getOrElse("top-level")}"
      case OneOfAdded(_, name)                                    => s"oneof '$name' added"
      case OneOfRemoved(_, name)                                  => s"oneof '$name' removed"
      case EnumAdded(_, name)                                     => s"enum '$name' added"
      case EnumRemoved(_, name)                                   => s"enum '$name' removed"
      case EnumRenamed(_, oldName, newName)                       => s"enum renamed from '$oldName' to '$newName'"
      case EnumValueAdded(_, name, number)                        => s"enum value '$name' ($number) added"
      case EnumValueRemoved(_, name, number, reserved)            => s"enum value '$name' ($number) removed${if (reserved) " (number reserved)" else ""}"
      case EnumValueNumberChanged(_, name, oldNum, newNum)        => s"enum value '$name' number changed from $oldNum to $newNum"
      case EnumValueRenamed(_, number, oldName, newName)          => s"enum value $number renamed from '$oldName' to '$newName'"
      case ReservedAdded(_, reserved)                             => s"reserved ${Renderer.renderReservedValue(reserved)} added"
      case ReservedRemoved(_, reserved)                           => s"reserved ${Renderer.renderReservedValue(reserved)} removed"
      case OptionAdded(_, optionName)                             => s"option '$optionName' added"
      case OptionRemoved(_, optionName)                           => s"option '$optionName' removed"
      case OptionChanged(_, optionName, _, _)                     => s"option '$optionName' changed"
      case ServiceAdded(_, name)                                  => s"service '$name' added"
      case ServiceRemoved(_, name)                                => s"service '$name' removed"
      case RpcAdded(_, name)                                      => s"rpc '$name' added"
      case RpcRemoved(_, name)                                    => s"rpc '$name' removed"
      case RpcRequestTypeChanged(_, name, oldType, newType)       => s"rpc '$name' request type changed from $oldType to $newType"
      case RpcResponseTypeChanged(_, name, oldType, newType)      => s"rpc '$name' response type changed from $oldType to $newType"
      case RpcStreamingChanged(_, name, direction, wasStreaming)  =>
        s"rpc '$name' $direction ${if (wasStreaming) "streaming removed" else "made streaming"}"
    })
  }
}
