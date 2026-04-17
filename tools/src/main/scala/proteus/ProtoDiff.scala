package proteus

import proteus.ProtoIR.*

/**
  * Computes structural changes between two proto3 [[ProtoIR.CompilationUnit]] definitions.
  *
  * Changes are returned as a mode-independent list. Use [[severity]] to classify
  * each change as Error / Warning / Info under a chosen [[CompatMode]]:
  *
  *   - [[CompatMode.Wire]]     &ndash; field numbers, wire types, enum value numbers matter.
  *   - [[CompatMode.Source]]   &ndash; field names, type names, declaration order matter.
  *   - [[CompatMode.Strictest]] &ndash; max(wire, source) severity.
  *
  * {{{
  * val changes = ProtoDiff.diff(oldUnit, newUnit)
  * val errors  = changes.filter(c => ProtoDiff.severity(c, CompatMode.Wire) == Severity.Error)
  * }}}
  */
object ProtoDiff {

  import Change.*

  /**
    * Returns the severity of a [[Change]] under the given [[CompatMode]].
    *
    * @param change the change to classify.
    * @param mode the compatibility mode to evaluate against.
    * @param overrides optional severity overrides.
    */
  def severity(change: Change, mode: CompatMode, overrides: SeverityOverrides = SeverityOverrides.empty): Severity = {
    import Severity.*
    val name           = change.productPrefix
    val (wire, source) = change match {
      case _: PackageChanged          => (Info, Error)
      case _: FileAdded               => (Info, Info)
      case _: FileRemoved             => (Error, Error)
      case _: MessageMoved            => (Info, Warning)
      case _: EnumMoved               => (Info, Warning)
      case _: ImportAdded             => (Info, Info)
      case _: ImportRemoved           => (Info, Info)
      case _: ImportModifierChanged   => (Info, Warning)
      case _: MessageAdded            => (Info, Info)
      case _: MessageRemoved          => (Error, Error)
      case _: MessageRenamed          => (Info, Error)
      case _: FieldAdded              => (Info, Warning)
      case c: FieldRemoved            => (if (c.numberReserved) Info else Error, Error)
      case _: FieldNumberChanged      => (Error, Info)
      case _: FieldRenamed            => (Info, Error)
      case _: FieldTypeChanged        => (Error, Error)
      case _: FieldTypeRefRenamed     => (Info, Error)
      case _: FieldOptionalityChanged => (Warning, Warning)
      case _: FieldOrderChanged       => (Info, Warning)
      case _: FieldOneOfChanged       => (Error, Error)
      case _: EnumAdded               => (Info, Info)
      case _: EnumRemoved             => (Error, Error)
      case _: EnumRenamed             => (Info, Error)
      case _: EnumValueAdded          => (Info, Info)
      case c: EnumValueRemoved        => (if (c.numberReserved) Info else Error, Error)
      case _: EnumValueNumberChanged  => (Error, Info)
      case _: EnumValueRenamed        => (Info, Error)
      case _: ReservedAdded           => (Info, Info)
      case _: ReservedRemoved         => (Warning, Warning)
      case _: OptionAdded             => (Info, Info)
      case _: OptionRemoved           => (Warning, Warning)
      case _: OptionChanged           => (Warning, Warning)
      case _: ServiceAdded            => (Info, Info)
      case _: ServiceRemoved          => (Error, Error)
      case _: RpcAdded                => (Info, Info)
      case _: RpcRemoved              => (Error, Error)
      case _: RpcRequestTypeChanged   => (Error, Error)
      case _: RpcResponseTypeChanged  => (Error, Error)
      case _: RpcStreamingChanged     => (Error, Error)
      case _: CommentAdded            => (Info, Info)
      case _: CommentRemoved          => (Info, Info)
      case _: CommentChanged          => (Info, Info)
    }
    val w              = overrides.wire.getOrElse(name, wire)
    val s              = overrides.source.getOrElse(name, source)
    mode match {
      case CompatMode.Wire      => w
      case CompatMode.Source    => s
      case CompatMode.Strictest => if (w.level >= s.level) w else s
    }
  }

  /**
    * Computes all changes between two [[ProtoIR.CompilationUnit]] definitions.
    *
    * @param oldUnit the original compilation unit.
    * @param newUnit the updated compilation unit.
    * @param path optional initial path prefix (e.g. `List("foo.proto")` when diffing files inside a multi-file diff).
    * @return a list of mode-independent changes; use [[severity]] to classify them.
    */
  def diff(oldUnit: CompilationUnit, newUnit: CompilationUnit, path: List[String] = Nil): List[Change] = {
    val raw = diffRaw(oldUnit, newUnit, path)
    resolveTypeRefChanges(raw, buildTypeRegistry(List(oldUnit)), buildTypeRegistry(List(newUnit)))
  }

  private def diffRaw(oldUnit: CompilationUnit, newUnit: CompilationUnit, path: List[String]): List[Change] =
    diffPackage(oldUnit.packageName, newUnit.packageName, path) ++
      diffImports(oldUnit.statements, newUnit.statements, path) ++
      diffTopLevelOptions(oldUnit.options, newUnit.options, path) ++
      diffTopLevelDefs(oldUnit.statements, newUnit.statements, path)

  /**
    * Computes all changes between two groups of proto files keyed by relative path.
    *
    * Top-level messages and enums that appear in different files between old and new but have the
    * same name and identical structure are reported as [[Change.MessageMoved]] / [[Change.EnumMoved]]
    * instead of remove + add.
    *
    * @param oldFiles old file map (relative path -> compilation unit).
    * @param newFiles new file map.
    * @return a list of mode-independent changes; use [[severity]] to classify them.
    */
  def diffFiles(oldFiles: Map[String, CompilationUnit], newFiles: Map[String, CompilationUnit]): List[Change] = {
    val oldRegistry = buildTypeRegistry(oldFiles.values)
    val newRegistry = buildTypeRegistry(newFiles.values)
    val common      = (oldFiles.keySet & newFiles.keySet).toList.sorted
    val onlyOld     = (oldFiles.keySet -- newFiles.keySet).toList.sorted
    val onlyNew     = (newFiles.keySet -- oldFiles.keySet).toList.sorted
    // Resolve type refs against the full multi-file registry below, not per-file.
    val matched     = common.flatMap(f => diffRaw(oldFiles(f), newFiles(f), List(f)))
    val fileRemoved = onlyOld.map(f => FileRemoved(List(f)))
    val fileAdded   = onlyNew.map(f => FileAdded(List(f)))
    val baseChanges = fileRemoved ++ fileAdded ++ matched
    val afterMoves  = detectCrossFileMoves(baseChanges, oldFiles, newFiles)
    resolveTypeRefChanges(afterMoves, oldRegistry, newRegistry)
  }

  /**
    * Detects top-level message/enum moves across files: if a top-level type with the same name and
    * identical structure was removed from old-file-A and added in new-file-B, replace the pair with
    * a single Move change.
    */
  private def detectCrossFileMoves(
    changes: List[Change],
    oldFiles: Map[String, CompilationUnit],
    newFiles: Map[String, CompilationUnit]
  ): List[Change] = {
    def topMessages(unit: CompilationUnit): Map[String, Message] =
      unit.statements.collect { case Statement.TopLevelStatement(TopLevelDef.MessageDef(m)) => m.name -> m }.toMap
    def topEnums(unit: CompilationUnit): Map[String, Enum]       =
      unit.statements.collect { case Statement.TopLevelStatement(TopLevelDef.EnumDef(e)) => e.name -> e }.toMap

    val removedFiles = changes.collect { case FileRemoved(file :: Nil) => file }
    val addedFiles   = changes.collect { case FileAdded(file :: Nil) => file }

    // Direct removals/additions (file kept, but type added/removed inside it)
    val msgRemovedDirect  = changes.collect { case MessageRemoved(file :: Nil, name) => (file, name) }
    val msgAddedDirect    = changes.collect { case MessageAdded(file :: Nil, name) => (file, name) }
    val enumRemovedDirect = changes.collect { case EnumRemoved(file :: Nil, name) => (file, name) }
    val enumAddedDirect   = changes.collect { case EnumAdded(file :: Nil, name) => (file, name) }

    // Implicit removals/additions from whole-file changes
    val msgRemovedFromFile  = removedFiles.flatMap(f => topMessages(oldFiles(f)).keys.map(n => (f, n)))
    val msgAddedFromFile    = addedFiles.flatMap(f => topMessages(newFiles(f)).keys.map(n => (f, n)))
    val enumRemovedFromFile = removedFiles.flatMap(f => topEnums(oldFiles(f)).keys.map(n => (f, n)))
    val enumAddedFromFile   = addedFiles.flatMap(f => topEnums(newFiles(f)).keys.map(n => (f, n)))

    val msgRemovedRaw  = msgRemovedDirect ++ msgRemovedFromFile
    val msgAddedRaw    = msgAddedDirect ++ msgAddedFromFile
    val enumRemovedRaw = enumRemovedDirect ++ enumRemovedFromFile
    val enumAddedRaw   = enumAddedDirect ++ enumAddedFromFile

    val msgMoves: List[(String, String, String)] = msgRemovedRaw.flatMap { case (oldFile, name) =>
      msgAddedRaw.find { case (newFile, n) => n == name && newFile != oldFile }.flatMap { case (newFile, _) =>
        for {
          oldMsg <- topMessages(oldFiles(oldFile)).get(name)
          newMsg <- topMessages(newFiles(newFile)).get(name)
          if messageFingerprint(oldMsg) == messageFingerprint(newMsg)
        } yield (oldFile, newFile, name)
      }
    }

    val enumMoves: List[(String, String, String)] = enumRemovedRaw.flatMap { case (oldFile, name) =>
      enumAddedRaw.find { case (newFile, n) => n == name && newFile != oldFile }.flatMap { case (newFile, _) =>
        for {
          oldEnum <- topEnums(oldFiles(oldFile)).get(name)
          newEnum <- topEnums(newFiles(newFile)).get(name)
          if enumFingerprint(oldEnum) == enumFingerprint(newEnum)
        } yield (oldFile, newFile, name)
      }
    }

    val removedMovedMsg  = msgMoves.map { case (of, _, n) => (of, n) }.toSet
    val addedMovedMsg    = msgMoves.map { case (_, nf, n) => (nf, n) }.toSet
    val removedMovedEnum = enumMoves.map { case (of, _, n) => (of, n) }.toSet
    val addedMovedEnum   = enumMoves.map { case (_, nf, n) => (nf, n) }.toSet

    val filtered = changes.filterNot {
      case MessageRemoved(file :: Nil, name) => removedMovedMsg.contains((file, name))
      case MessageAdded(file :: Nil, name)   => addedMovedMsg.contains((file, name))
      case EnumRemoved(file :: Nil, name)    => removedMovedEnum.contains((file, name))
      case EnumAdded(file :: Nil, name)      => addedMovedEnum.contains((file, name))
      case _                                 => false
    }

    // Group the move under the destination file so it appears alongside changes in that file.
    val moves: List[Change] =
      msgMoves.map { case (of, nf, n) => MessageMoved(List(nf), n, of, nf) } ++
        enumMoves.map { case (of, nf, n) => EnumMoved(List(nf), n, of, nf) }

    filtered ++ moves
  }

  private type TypeEntry = Either[Message, Enum]

  private def buildTypeRegistry(units: Iterable[CompilationUnit]): Map[String, TypeEntry] = {
    def collectFromMessage(msg: Message, prefix: String): List[(String, TypeEntry)] = {
      val fqn    = if (prefix.isEmpty) msg.name else s"$prefix.${msg.name}"
      val self   = fqn -> Left(msg)
      val nested = msg.elements.flatMap {
        case MessageElement.NestedMessageElement(m) => collectFromMessage(m, fqn)
        case MessageElement.NestedEnumElement(e)    => List(s"$fqn.${e.name}" -> Right(e))
        case _                                      => Nil
      }
      self :: nested
    }

    units.flatMap { unit =>
      unit.statements.flatMap {
        case Statement.TopLevelStatement(TopLevelDef.MessageDef(m)) => collectFromMessage(m, "")
        case Statement.TopLevelStatement(TopLevelDef.EnumDef(e))    => List(e.name -> Right(e))
        case _                                                      => Nil
      }
    }.toMap
  }

  private def areWireEquivalent(oldType: Type, newType: Type, oldRegistry: Map[String, TypeEntry], newRegistry: Map[String, TypeEntry]): Boolean = {
    val oldNorm = normalizeType(oldType)
    val newNorm = normalizeType(newType)
    if (oldNorm == newNorm) true
    else
      (oldNorm, newNorm) match {
        case (Type.RefType(oldName), Type.RefType(newName)) =>
          (oldRegistry.get(oldName), newRegistry.get(newName)) match {
            case (Some(Left(oldMsg)), Some(Left(newMsg)))     => messageFingerprint(oldMsg) == messageFingerprint(newMsg)
            case (Some(Right(oldEnum)), Some(Right(newEnum))) => enumFingerprint(oldEnum) == enumFingerprint(newEnum)
            case _                                            => false
          }
        case (Type.ListType(v1), Type.ListType(v2))         => areWireEquivalent(v1, v2, oldRegistry, newRegistry)
        case (Type.MapType(k1, v1), Type.MapType(k2, v2))   =>
          areWireEquivalent(k1, k2, oldRegistry, newRegistry) && areWireEquivalent(v1, v2, oldRegistry, newRegistry)
        case _                                              => false
      }
  }

  private def resolveTypeRefChanges(changes: List[Change], oldRegistry: Map[String, TypeEntry], newRegistry: Map[String, TypeEntry]): List[Change] =
    changes.map {
      case FieldTypeChanged(path, name, number, oldType, newType) if areWireEquivalent(oldType, newType, oldRegistry, newRegistry) =>
        FieldTypeRefRenamed(path, name, number, oldType, newType)
      case c                                                                                                                       => c
    }

  private def normalizeType(ty: Type): Type = ty match {
    case Type.RefType(name)     => Type.RefType(name.stripPrefix("."))
    case Type.EnumRefType(name) => Type.RefType(name.stripPrefix("."))
    case Type.MapType(k, v)     => Type.MapType(normalizeType(k), normalizeType(v))
    case Type.ListType(v)       => Type.ListType(normalizeType(v))
    case other                  => other
  }

  private def stripFieldCosmetics(f: Field): Field =
    f.copy(comment = None, options = Nil, ty = normalizeType(f.ty))

  private def stripElementCosmetics(elem: MessageElement): MessageElement = elem match {
    case MessageElement.FieldElement(f)         =>
      MessageElement.FieldElement(stripFieldCosmetics(f))
    case MessageElement.OneOfElement(o)         =>
      MessageElement.OneOfElement(o.copy(comment = None, options = Nil, fields = o.fields.map(stripFieldCosmetics)))
    case MessageElement.NestedMessageElement(m) =>
      MessageElement.NestedMessageElement(stripMessageCosmetics(m))
    case MessageElement.NestedEnumElement(e)    =>
      MessageElement.NestedEnumElement(stripEnumCosmetics(e))
  }

  private def stripMessageCosmetics(m: Message): Message =
    m.copy(comment = None, options = Nil, nested = false, elements = m.elements.map(stripElementCosmetics))

  private def stripEnumCosmetics(e: Enum): Enum =
    e.copy(comment = None, options = Nil, nested = false, values = e.values.map(v => v.copy(comment = None, options = Nil)))

  private def diffPackage(oldPkg: Option[String], newPkg: Option[String], path: List[String]): List[Change] =
    if (oldPkg == newPkg) Nil else List(PackageChanged(path, oldPkg, newPkg))

  private def diffImports(oldStmts: List[Statement], newStmts: List[Statement], path: List[String]): List[Change] = {
    val oldImports = oldStmts.collect { case i: Statement.ImportStatement => i }
    val newImports = newStmts.collect { case i: Statement.ImportStatement => i }
    val oldByPath  = oldImports.map(i => i.path -> i).toMap
    val newByPath  = newImports.map(i => i.path -> i).toMap
    val removed    = (oldByPath.keySet -- newByPath.keySet).toList.map(p => ImportRemoved(path, p))
    val added      = (newByPath.keySet -- oldByPath.keySet).toList.map(p => ImportAdded(path, p))
    val changed    = (oldByPath.keySet & newByPath.keySet).toList.flatMap { p =>
      val oldMod = oldByPath(p).modifier
      val newMod = newByPath(p).modifier
      if (oldMod == newMod) Nil else List(ImportModifierChanged(path, p, oldMod, newMod))
    }
    removed ++ added ++ changed
  }

  private def diffTopLevelOptions(oldOpts: List[TopLevelOption], newOpts: List[TopLevelOption], path: List[String]): List[Change] =
    diffOptionPairs(oldOpts.map(o => o.name.render -> o.value), newOpts.map(o => o.name.render -> o.value), path)

  private def diffOptions(oldOpts: List[OptionValue], newOpts: List[OptionValue], path: List[String]): List[Change] =
    diffOptionPairs(oldOpts.map(o => o.name.render -> o.value), newOpts.map(o => o.name.render -> o.value), path)

  private def diffOptionPairs(oldPairs: List[(String, OptionVal)], newPairs: List[(String, OptionVal)], path: List[String]): List[Change] = {
    val oldMap  = oldPairs.toMap
    val newMap  = newPairs.toMap
    val removed = (oldMap.keySet -- newMap.keySet).toList.map(n => OptionRemoved(path, n))
    val added   = (newMap.keySet -- oldMap.keySet).toList.map(n => OptionAdded(path, n))
    val changed = (oldMap.keySet & newMap.keySet).toList.flatMap { n =>
      if (oldMap(n) == newMap(n)) Nil else List(OptionChanged(path, n, oldMap(n), newMap(n)))
    }
    removed ++ added ++ changed
  }

  private def diffTopLevelDefs(oldStmts: List[Statement], newStmts: List[Statement], path: List[String]): List[Change] = {
    val oldMsgs  = oldStmts.collect { case Statement.TopLevelStatement(TopLevelDef.MessageDef(m)) => m }
    val newMsgs  = newStmts.collect { case Statement.TopLevelStatement(TopLevelDef.MessageDef(m)) => m }
    val oldEnums = oldStmts.collect { case Statement.TopLevelStatement(TopLevelDef.EnumDef(e)) => e }
    val newEnums = newStmts.collect { case Statement.TopLevelStatement(TopLevelDef.EnumDef(e)) => e }
    val oldSvcs  = oldStmts.collect { case Statement.TopLevelStatement(TopLevelDef.ServiceDef(s)) => s }
    val newSvcs  = newStmts.collect { case Statement.TopLevelStatement(TopLevelDef.ServiceDef(s)) => s }
    diffMessages(oldMsgs, newMsgs, path) ++ diffEnums(oldEnums, newEnums, path) ++ diffServices(oldSvcs, newSvcs, path)
  }

  private def diffMessages(oldMsgs: List[Message], newMsgs: List[Message], path: List[String]): List[Change] = {
    val oldByName                 = oldMsgs.map(m => m.name -> m).toMap
    val newByName                 = newMsgs.map(m => m.name -> m).toMap
    val common                    = oldByName.keySet & newByName.keySet
    val unmOld                    = oldMsgs.filterNot(m => common.contains(m.name))
    val unmNew                    = newMsgs.filterNot(m => common.contains(m.name))
    val (renames, remOld, remNew) =
      detectRenames[Message, Message](unmOld, unmNew, messageFingerprint, (o, n) => MessageRenamed(path, o.name, n.name))
    val removed                   = remOld.map(m => MessageRemoved(path, m.name))
    val added                     = remNew.map(m => MessageAdded(path, m.name))
    val matched                   = common.toList.sorted.flatMap(n => diffMessage(oldByName(n), newByName(n), path))
    removed ++ added ++ renames ++ matched
  }

  private def messageFingerprint(m: Message): Message =
    stripMessageCosmetics(m).copy(name = "")

  private def diffComment(oldComment: Option[String], newComment: Option[String], path: List[String], element: String): List[Change] =
    (oldComment, newComment) match {
      case (None, Some(_))              => List(CommentAdded(path, element))
      case (Some(_), None)              => List(CommentRemoved(path, element))
      case (Some(o), Some(n)) if o != n => List(CommentChanged(path, element))
      case _                            => Nil
    }

  private def diffMessage(oldMsg: Message, newMsg: Message, path: List[String]): List[Change] = {
    val msgPath         = path :+ oldMsg.name
    val oldFlat         = flattenFields(oldMsg)
    val newFlat         = flattenFields(newMsg)
    val fieldChanges    = diffFields(oldFlat, newFlat, msgPath, newMsg.reserved)
    val oneOfChanges    = diffOneOfs(oldMsg, newMsg, msgPath)
    val oldNested       = oldMsg.elements.collect { case MessageElement.NestedMessageElement(m) => m }
    val newNested       = newMsg.elements.collect { case MessageElement.NestedMessageElement(m) => m }
    val nestedMsgDiffs  = diffMessages(oldNested, newNested, msgPath)
    val oldNestedEnums  = oldMsg.elements.collect { case MessageElement.NestedEnumElement(e) => e }
    val newNestedEnums  = newMsg.elements.collect { case MessageElement.NestedEnumElement(e) => e }
    val nestedEnumDiffs = diffEnums(oldNestedEnums, newNestedEnums, msgPath)
    val reservedDiffs   = diffReserved(oldMsg.reserved, newMsg.reserved, msgPath)
    val optionDiffs     = diffOptions(oldMsg.options, newMsg.options, msgPath)
    val commentDiffs    = diffComment(oldMsg.comment, newMsg.comment, path, oldMsg.name)
    fieldChanges ++ oneOfChanges ++ nestedMsgDiffs ++ nestedEnumDiffs ++ reservedDiffs ++ optionDiffs ++ commentDiffs
  }

  private case class ContainedField(field: Field, container: Option[String], index: Int)

  private def flattenFields(msg: Message): List[ContainedField] = {
    val result = List.newBuilder[ContainedField]
    var idx    = 0
    msg.elements.foreach {
      case MessageElement.FieldElement(f) =>
        result += ContainedField(f, None, idx)
        idx += 1
      case MessageElement.OneOfElement(o) =>
        o.fields.foreach { f =>
          result += ContainedField(f, Some(o.name), idx)
          idx += 1
        }
      case _                              => ()
    }
    result.result()
  }

  private def matchFields(
    oldFields: List[ContainedField],
    newFields: List[ContainedField]
  ): (List[(ContainedField, ContainedField)], List[ContainedField], List[ContainedField]) = {
    // Pass 1: exact (name, number)
    val oldByKey  = oldFields.map(cf => (cf.field.name, cf.field.number) -> cf).toMap
    val newByKey  = newFields.map(cf => (cf.field.name, cf.field.number) -> cf).toMap
    val exactKeys = oldByKey.keySet & newByKey.keySet
    val exact     = exactKeys.toList.map(k => (oldByKey(k), newByKey(k)))
    val rem1Old   = oldFields.filterNot(cf => exactKeys.contains((cf.field.name, cf.field.number)))
    val rem1New   = newFields.filterNot(cf => exactKeys.contains((cf.field.name, cf.field.number)))

    // Pass 2: name only
    val oldByName = rem1Old.map(cf => cf.field.name -> cf).toMap
    val newByName = rem1New.map(cf => cf.field.name -> cf).toMap
    val nameKeys  = oldByName.keySet & newByName.keySet
    val byName    = nameKeys.toList.map(n => (oldByName(n), newByName(n)))
    val rem2Old   = rem1Old.filterNot(cf => nameKeys.contains(cf.field.name))
    val rem2New   = rem1New.filterNot(cf => nameKeys.contains(cf.field.name))

    // Pass 3: number only
    val oldByNum = rem2Old.map(cf => cf.field.number -> cf).toMap
    val newByNum = rem2New.map(cf => cf.field.number -> cf).toMap
    val numKeys  = oldByNum.keySet & newByNum.keySet
    val byNum    = numKeys.toList.map(n => (oldByNum(n), newByNum(n)))
    val rem3Old  = rem2Old.filterNot(cf => numKeys.contains(cf.field.number))
    val rem3New  = rem2New.filterNot(cf => numKeys.contains(cf.field.number))

    (exact ++ byName ++ byNum, rem3Old, rem3New)
  }

  private def diffFields(
    oldFields: List[ContainedField],
    newFields: List[ContainedField],
    path: List[String],
    newReserved: List[Reserved]
  ): List[Change] = {
    val (matched, unmatchedOld, unmatchedNew) = matchFields(oldFields, newFields)

    val matchChanges = matched.flatMap { case (oldCf, newCf) =>
      val changes  = List.newBuilder[Change]
      val (oF, nF) = (oldCf.field, newCf.field)
      if (oF.name != nF.name)
        changes += FieldRenamed(path, oF.number, oF.name, nF.name)
      if (oF.number != nF.number)
        changes += FieldNumberChanged(path, oF.name, oF.number, nF.number)
      if (normalizeType(oF.ty) != normalizeType(nF.ty))
        changes += FieldTypeChanged(path, oF.name, oF.number, oF.ty, nF.ty)
      if (oF.optional != nF.optional)
        changes += FieldOptionalityChanged(path, oF.name, oF.number, oF.optional)
      if (oldCf.container != newCf.container)
        changes += FieldOneOfChanged(path, oF.name, oF.number, oldCf.container, newCf.container)
      changes ++= diffOptions(oF.options, nF.options, path :+ oF.name)
      changes ++= diffComment(oF.comment, nF.comment, path, oF.name)
      changes.result()
    }

    val removed = unmatchedOld.map(cf => FieldRemoved(path, cf.field.name, cf.field.number, isNumberReserved(cf.field.number, newReserved)))
    val added   = unmatchedNew.map(cf => FieldAdded(path, cf.field.name, cf.field.number))

    // Order detection: within-container only, exclude fields whose container changed
    val sameContainer = matched.filter { case (o, n) => o.container == n.container }
    val orderChanged  = sameContainer.groupBy(_._1.container).values.exists { pairs =>
      val oldOrder = pairs.sortBy(_._1.index).map(_._1.field.number)
      val newOrder = pairs.sortBy(_._2.index).map(_._1.field.number)
      oldOrder != newOrder
    }
    val orderChange   = if (orderChanged) List(FieldOrderChanged(path)) else Nil

    matchChanges ++ removed ++ added ++ orderChange
  }

  private def isNumberReserved(number: Int, reserved: List[Reserved]): Boolean =
    reserved.exists {
      case Reserved.Number(n)   => n == number
      case Reserved.Range(s, e) => number >= s && number <= e
      case _: Reserved.Name     => false
    }

  private def diffOneOfs(oldMsg: Message, newMsg: Message, path: List[String]): List[Change] = {
    val oldOneOfs = oldMsg.elements.collect { case MessageElement.OneOfElement(o) => o }
    val newOneOfs = newMsg.elements.collect { case MessageElement.OneOfElement(o) => o }
    val oldByName = oldOneOfs.map(o => o.name -> o).toMap
    val newByName = newOneOfs.map(o => o.name -> o).toMap
    (oldByName.keySet & newByName.keySet).toList.flatMap { n =>
      diffOptions(oldByName(n).options, newByName(n).options, path :+ n) ++
        diffComment(oldByName(n).comment, newByName(n).comment, path, n)
    }
  }

  private def diffEnums(oldEnums: List[Enum], newEnums: List[Enum], path: List[String]): List[Change] = {
    val oldByName                 = oldEnums.map(e => e.name -> e).toMap
    val newByName                 = newEnums.map(e => e.name -> e).toMap
    val common                    = oldByName.keySet & newByName.keySet
    val unmOld                    = oldEnums.filterNot(e => common.contains(e.name))
    val unmNew                    = newEnums.filterNot(e => common.contains(e.name))
    val (renames, remOld, remNew) =
      detectRenames[Enum, Enum](unmOld, unmNew, enumFingerprint, (o, n) => EnumRenamed(path, o.name, n.name))
    val removed                   = remOld.map(e => EnumRemoved(path, e.name))
    val added                     = remNew.map(e => EnumAdded(path, e.name))
    val matched                   = common.toList.sorted.flatMap(n => diffEnum(oldByName(n), newByName(n), path))
    removed ++ added ++ renames ++ matched
  }

  private def enumFingerprint(e: Enum): Enum =
    stripEnumCosmetics(e).copy(name = "")

  private def diffEnum(oldEnum: Enum, newEnum: Enum, path: List[String]): List[Change] = {
    val enumPath = path :+ oldEnum.name
    diffEnumValues(oldEnum.values, newEnum.values, enumPath, newEnum.reserved) ++
      diffReserved(oldEnum.reserved, newEnum.reserved, enumPath) ++
      diffOptions(oldEnum.options, newEnum.options, enumPath) ++
      diffComment(oldEnum.comment, newEnum.comment, path, oldEnum.name)
  }

  private def diffEnumValues(
    oldValues: List[EnumValue],
    newValues: List[EnumValue],
    path: List[String],
    newReserved: List[Reserved]
  ): List[Change] = {
    // Pass 1: exact (name, intValue)
    val oldByKey   = oldValues.map(v => (v.name, v.intValue) -> v).toMap
    val newByKey   = newValues.map(v => (v.name, v.intValue) -> v).toMap
    val exactKeys  = oldByKey.keySet & newByKey.keySet
    val exactDiffs = exactKeys.toList.flatMap { k =>
      diffOptions(oldByKey(k).options, newByKey(k).options, path :+ k._1) ++
        diffComment(oldByKey(k).comment, newByKey(k).comment, path, k._1)
    }
    val rem1Old    = oldValues.filterNot(v => exactKeys.contains((v.name, v.intValue)))
    val rem1New    = newValues.filterNot(v => exactKeys.contains((v.name, v.intValue)))

    // Pass 2: name only → number changed
    val oldByName = rem1Old.map(v => v.name -> v).toMap
    val newByName = rem1New.map(v => v.name -> v).toMap
    val nameKeys  = oldByName.keySet & newByName.keySet
    val nameDiffs = nameKeys.toList.flatMap { n =>
      EnumValueNumberChanged(path, n, oldByName(n).intValue, newByName(n).intValue) ::
        diffOptions(oldByName(n).options, newByName(n).options, path :+ n) ++
        diffComment(oldByName(n).comment, newByName(n).comment, path, n)
    }
    val rem2Old   = rem1Old.filterNot(v => nameKeys.contains(v.name))
    val rem2New   = rem1New.filterNot(v => nameKeys.contains(v.name))

    // Pass 3: number only → renamed
    val oldByNum = rem2Old.map(v => v.intValue -> v).toMap
    val newByNum = rem2New.map(v => v.intValue -> v).toMap
    val numKeys  = oldByNum.keySet & newByNum.keySet
    val numDiffs = numKeys.toList.flatMap { n =>
      EnumValueRenamed(path, n, oldByNum(n).name, newByNum(n).name) ::
        diffOptions(oldByNum(n).options, newByNum(n).options, path :+ oldByNum(n).name) ++
        diffComment(oldByNum(n).comment, newByNum(n).comment, path, oldByNum(n).name)
    }
    val rem3Old  = rem2Old.filterNot(v => numKeys.contains(v.intValue))
    val rem3New  = rem2New.filterNot(v => numKeys.contains(v.intValue))

    // Pass 4: unmatched
    val removed = rem3Old.map(v => EnumValueRemoved(path, v.name, v.intValue, isNumberReserved(v.intValue, newReserved)))
    val added   = rem3New.map(v => EnumValueAdded(path, v.name, v.intValue))

    exactDiffs ++ nameDiffs ++ numDiffs ++ removed ++ added
  }

  private def detectRenames[A, F](
    oldItems: List[A],
    newItems: List[A],
    fingerprint: A => F,
    mkChange: (A, A) => Change
  ): (List[Change], List[A], List[A]) = {
    val oldByFp    = oldItems.groupBy(fingerprint)
    val newByFp    = newItems.groupBy(fingerprint)
    val pairs      = oldByFp.toList.flatMap { case (fp, olds) =>
      newByFp.get(fp) match {
        case Some(news) if olds.length == 1 && news.length == 1 => List((olds.head, news.head))
        case _                                                  => Nil
      }
    }
    val renamedOld = pairs.map(_._1).toSet
    val renamedNew = pairs.map(_._2).toSet
    val changes    = pairs.map { case (o, n) => mkChange(o, n) }
    (changes, oldItems.filterNot(renamedOld.contains), newItems.filterNot(renamedNew.contains))
  }

  private def diffReserved(oldRes: List[Reserved], newRes: List[Reserved], path: List[String]): List[Change] = {
    val oldSet  = oldRes.toSet
    val newSet  = newRes.toSet
    val removed = (oldSet -- newSet).toList.map(r => ReservedRemoved(path, r))
    val added   = (newSet -- oldSet).toList.map(r => ReservedAdded(path, r))
    removed ++ added
  }

  private def diffServices(oldSvcs: List[Service], newSvcs: List[Service], path: List[String]): List[Change] = {
    val oldByName = oldSvcs.map(s => s.name -> s).toMap
    val newByName = newSvcs.map(s => s.name -> s).toMap
    val removed   = (oldByName.keySet -- newByName.keySet).toList.map(n => ServiceRemoved(path, n))
    val added     = (newByName.keySet -- oldByName.keySet).toList.map(n => ServiceAdded(path, n))
    val matched   = (oldByName.keySet & newByName.keySet).toList.sorted.flatMap(n => diffService(oldByName(n), newByName(n), path))
    removed ++ added ++ matched
  }

  private def diffService(oldSvc: Service, newSvc: Service, path: List[String]): List[Change] = {
    val svcPath = path :+ oldSvc.name
    diffRpcs(oldSvc.rpcs, newSvc.rpcs, svcPath) ++
      diffOptions(oldSvc.options, newSvc.options, svcPath) ++
      diffComment(oldSvc.comment, newSvc.comment, path, oldSvc.name)
  }

  private def diffRpcs(oldRpcs: List[Rpc], newRpcs: List[Rpc], path: List[String]): List[Change] = {
    val oldByName = oldRpcs.map(r => r.name -> r).toMap
    val newByName = newRpcs.map(r => r.name -> r).toMap
    val removed   = (oldByName.keySet -- newByName.keySet).toList.map(n => RpcRemoved(path, n))
    val added     = (newByName.keySet -- oldByName.keySet).toList.map(n => RpcAdded(path, n))
    val matched   = (oldByName.keySet & newByName.keySet).toList.sorted.flatMap(n => diffRpc(oldByName(n), newByName(n), path))
    removed ++ added ++ matched
  }

  private def diffRpc(oldRpc: Rpc, newRpc: Rpc, path: List[String]): List[Change] = {
    val changes = List.newBuilder[Change]
    val oldReq  = oldRpc.request.fqn.render
    val newReq  = newRpc.request.fqn.render
    if (oldReq != newReq)
      changes += RpcRequestTypeChanged(path, oldRpc.name, oldReq, newReq)
    val oldResp = oldRpc.response.fqn.render
    val newResp = newRpc.response.fqn.render
    if (oldResp != newResp)
      changes += RpcResponseTypeChanged(path, oldRpc.name, oldResp, newResp)
    if (oldRpc.streamingRequest != newRpc.streamingRequest)
      changes += RpcStreamingChanged(path, oldRpc.name, "request", oldRpc.streamingRequest)
    if (oldRpc.streamingResponse != newRpc.streamingResponse)
      changes += RpcStreamingChanged(path, oldRpc.name, "response", oldRpc.streamingResponse)
    changes ++= diffOptions(oldRpc.options, newRpc.options, path :+ oldRpc.name)
    changes ++= diffComment(oldRpc.comment, newRpc.comment, path, oldRpc.name)
    changes.result()
  }
}
