package proteus

import zio.test.*

import proteus.Change.*
import proteus.ProtoIR.*

object ProtoDiffSpec extends ZIOSpecDefault {

  private def parse(input: String): CompilationUnit =
    ProtoParser.parse(input).fold(e => throw new RuntimeException(e), identity)

  def spec = suite("ProtoDiffSpec")(
    suite("No changes")(
      test("identical protos produce empty diff") {
        val proto = parse("""syntax = "proto3";
                            |package test;
                            |message Foo {
                            |  string name = 1;
                            |  int32 id = 2;
                            |}
                            |""".stripMargin)
        assertTrue(ProtoDiff.diff(proto, proto) == Nil)
      }
    ),
    suite("Package changes")(
      test("package changed") {
        val old = parse("""syntax = "proto3"; package old.pkg;""")
        val nw  = parse("""syntax = "proto3"; package new.pkg;""")
        assertTrue(ProtoDiff.diff(old, nw) == List(PackageChanged(Nil, Some("old.pkg"), Some("new.pkg"))))
      },
      test("package removed") {
        val old = parse("""syntax = "proto3"; package test;""")
        val nw  = parse("""syntax = "proto3";""")
        assertTrue(ProtoDiff.diff(old, nw) == List(PackageChanged(Nil, Some("test"), None)))
      }
    ),
    suite("Import changes")(
      test("import added") {
        val old = parse("""syntax = "proto3";""")
        val nw  = parse("""syntax = "proto3"; import "other.proto";""")
        assertTrue(ProtoDiff.diff(old, nw) == List(ImportAdded(Nil, "other.proto")))
      },
      test("import removed") {
        val old = parse("""syntax = "proto3"; import "other.proto";""")
        val nw  = parse("""syntax = "proto3";""")
        assertTrue(ProtoDiff.diff(old, nw) == List(ImportRemoved(Nil, "other.proto")))
      },
      test("import modifier changed") {
        val old = parse("""syntax = "proto3"; import "other.proto";""")
        val nw  = parse("""syntax = "proto3"; import public "other.proto";""")
        assertTrue(ProtoDiff.diff(old, nw) == List(ImportModifierChanged(Nil, "other.proto", None, Some("public"))))
      }
    ),
    suite("Message changes")(
      test("message added") {
        val old = parse("""syntax = "proto3";""")
        val nw  = parse("""syntax = "proto3"; message Foo { string name = 1; }""")
        assertTrue(ProtoDiff.diff(old, nw) == List(MessageAdded(Nil, "Foo")))
      },
      test("message removed") {
        val old = parse("""syntax = "proto3"; message Foo { string name = 1; }""")
        val nw  = parse("""syntax = "proto3";""")
        assertTrue(ProtoDiff.diff(old, nw) == List(MessageRemoved(Nil, "Foo")))
      },
      test("message renamed detected via structural fingerprint") {
        val old = parse("""syntax = "proto3"; message Foo { string name = 1; int32 id = 2; }""")
        val nw  = parse("""syntax = "proto3"; message Bar { string name = 1; int32 id = 2; }""")
        assertTrue(ProtoDiff.diff(old, nw) == List(MessageRenamed(Nil, "Foo", "Bar")))
      },
      test("ambiguous rename falls back to add/remove") {
        val old     = parse(
          """syntax = "proto3";
            |message Foo { string name = 1; }
            |message Bar { string name = 1; }
            |""".stripMargin
        )
        val nw      = parse(
          """syntax = "proto3";
            |message Baz { string name = 1; }
            |message Qux { string name = 1; }
            |""".stripMargin
        )
        val changes = ProtoDiff.diff(old, nw)
        assertTrue(
          changes.collect { case _: MessageRenamed => true }.isEmpty,
          changes.collect { case c: MessageRemoved => c.name }.toSet == Set("Foo", "Bar"),
          changes.collect { case c: MessageAdded => c.name }.toSet == Set("Baz", "Qux")
        )
      }
    ),
    suite("Field changes")(
      test("field added") {
        val old     = parse("""syntax = "proto3"; message Foo { string name = 1; }""")
        val nw      = parse("""syntax = "proto3"; message Foo { string name = 1; int32 id = 2; }""")
        val changes = ProtoDiff.diff(old, nw)
        assertTrue(changes == List(FieldAdded(List("Foo"), "id", 2)))
      },
      test("field removed without reservation") {
        val old     = parse("""syntax = "proto3"; message Foo { string name = 1; int32 id = 2; }""")
        val nw      = parse("""syntax = "proto3"; message Foo { string name = 1; }""")
        val changes = ProtoDiff.diff(old, nw)
        assertTrue(changes == List(FieldRemoved(List("Foo"), "id", 2, numberReserved = false)))
      },
      test("field removed with reservation") {
        val old     = parse("""syntax = "proto3"; message Foo { string name = 1; int32 id = 2; }""")
        val nw      = parse("""syntax = "proto3"; message Foo { string name = 1; reserved 2; }""")
        val changes = ProtoDiff.diff(old, nw)
        assertTrue(
          changes.contains(FieldRemoved(List("Foo"), "id", 2, numberReserved = true)),
          changes.contains(ReservedAdded(List("Foo"), Reserved.Number(2)))
        )
      },
      test("field number changed") {
        val old = parse("""syntax = "proto3"; message Foo { string name = 1; }""")
        val nw  = parse("""syntax = "proto3"; message Foo { string name = 2; }""")
        assertTrue(ProtoDiff.diff(old, nw) == List(FieldNumberChanged(List("Foo"), "name", 1, 2)))
      },
      test("field renamed") {
        val old = parse("""syntax = "proto3"; message Foo { string name = 1; }""")
        val nw  = parse("""syntax = "proto3"; message Foo { string title = 1; }""")
        assertTrue(ProtoDiff.diff(old, nw) == List(FieldRenamed(List("Foo"), 1, "name", "title")))
      },
      test("field type changed") {
        val old = parse("""syntax = "proto3"; message Foo { string name = 1; }""")
        val nw  = parse("""syntax = "proto3"; message Foo { int32 name = 1; }""")
        assertTrue(ProtoDiff.diff(old, nw) == List(FieldTypeChanged(List("Foo"), "name", 1, Type.String, Type.Int32)))
      },
      test("field optionality changed") {
        val old = parse("""syntax = "proto3"; message Foo { string name = 1; }""")
        val nw  = parse("""syntax = "proto3"; message Foo { optional string name = 1; }""")
        assertTrue(ProtoDiff.diff(old, nw) == List(FieldOptionalityChanged(List("Foo"), "name", 1, wasOptional = false)))
      },
      test("field with both name and number changed becomes add/remove") {
        val old     = parse("""syntax = "proto3"; message Foo { string name = 1; }""")
        val nw      = parse("""syntax = "proto3"; message Foo { int32 id = 2; }""")
        val changes = ProtoDiff.diff(old, nw)
        assertTrue(
          changes.contains(FieldRemoved(List("Foo"), "name", 1, numberReserved = false)),
          changes.contains(FieldAdded(List("Foo"), "id", 2))
        )
      },
      test("field order changed") {
        val old     = parse(
          """syntax = "proto3";
            |message Foo {
            |  string name = 1;
            |  int32 id = 2;
            |  bool active = 3;
            |}
            |""".stripMargin
        )
        val nw      = parse(
          """syntax = "proto3";
            |message Foo {
            |  int32 id = 2;
            |  string name = 1;
            |  bool active = 3;
            |}
            |""".stripMargin
        )
        val changes = ProtoDiff.diff(old, nw)
        assertTrue(changes == List(FieldOrderChanged(List("Foo"))))
      }
    ),
    suite("OneOf changes")(
      test("oneof added") {
        val old     = parse("""syntax = "proto3"; message Foo { string name = 1; }""")
        val nw      = parse(
          """syntax = "proto3";
            |message Foo {
            |  string name = 1;
            |  oneof contact { string email = 2; string phone = 3; }
            |}
            |""".stripMargin
        )
        val changes = ProtoDiff.diff(old, nw)
        assertTrue(
          changes.contains(OneOfAdded(List("Foo"), "contact")),
          changes.contains(FieldAdded(List("Foo"), "email", 2)),
          changes.contains(FieldAdded(List("Foo"), "phone", 3))
        )
      },
      test("oneof removed") {
        val old     = parse(
          """syntax = "proto3";
            |message Foo {
            |  string name = 1;
            |  oneof contact { string email = 2; string phone = 3; }
            |}
            |""".stripMargin
        )
        val nw      = parse("""syntax = "proto3"; message Foo { string name = 1; }""")
        val changes = ProtoDiff.diff(old, nw)
        assertTrue(
          changes.contains(OneOfRemoved(List("Foo"), "contact")),
          changes.contains(FieldRemoved(List("Foo"), "email", 2, numberReserved = false)),
          changes.contains(FieldRemoved(List("Foo"), "phone", 3, numberReserved = false))
        )
      },
      test("field moved into oneof") {
        val old     = parse(
          """syntax = "proto3";
            |message Foo {
            |  string name = 1;
            |  string email = 2;
            |}
            |""".stripMargin
        )
        val nw      = parse(
          """syntax = "proto3";
            |message Foo {
            |  string name = 1;
            |  oneof contact { string email = 2; }
            |}
            |""".stripMargin
        )
        val changes = ProtoDiff.diff(old, nw)
        assertTrue(
          changes.contains(FieldOneOfChanged(List("Foo"), "email", 2, None, Some("contact"))),
          changes.contains(OneOfAdded(List("Foo"), "contact"))
        )
      },
      test("field moved between oneofs") {
        val old     = parse(
          """syntax = "proto3";
            |message Foo {
            |  string name = 1;
            |  oneof alpha { string email = 2; }
            |  oneof beta { string phone = 3; }
            |}
            |""".stripMargin
        )
        val nw      = parse(
          """syntax = "proto3";
            |message Foo {
            |  string name = 1;
            |  oneof alpha { string phone = 3; }
            |  oneof beta { string email = 2; }
            |}
            |""".stripMargin
        )
        val changes = ProtoDiff.diff(old, nw)
        assertTrue(
          changes.contains(FieldOneOfChanged(List("Foo"), "email", 2, Some("alpha"), Some("beta"))),
          changes.contains(FieldOneOfChanged(List("Foo"), "phone", 3, Some("beta"), Some("alpha")))
        )
      }
    ),
    suite("Enum changes")(
      test("enum added") {
        val old = parse("""syntax = "proto3";""")
        val nw  = parse("""syntax = "proto3"; enum Status { UNKNOWN = 0; ACTIVE = 1; }""")
        assertTrue(ProtoDiff.diff(old, nw) == List(EnumAdded(Nil, "Status")))
      },
      test("enum removed") {
        val old = parse("""syntax = "proto3"; enum Status { UNKNOWN = 0; ACTIVE = 1; }""")
        val nw  = parse("""syntax = "proto3";""")
        assertTrue(ProtoDiff.diff(old, nw) == List(EnumRemoved(Nil, "Status")))
      },
      test("enum renamed detected via structural fingerprint") {
        val old = parse("""syntax = "proto3"; enum Status { UNKNOWN = 0; ACTIVE = 1; }""")
        val nw  = parse("""syntax = "proto3"; enum State { UNKNOWN = 0; ACTIVE = 1; }""")
        assertTrue(ProtoDiff.diff(old, nw) == List(EnumRenamed(Nil, "Status", "State")))
      },
      test("enum value added") {
        val old = parse("""syntax = "proto3"; enum Status { UNKNOWN = 0; }""")
        val nw  = parse("""syntax = "proto3"; enum Status { UNKNOWN = 0; ACTIVE = 1; }""")
        assertTrue(ProtoDiff.diff(old, nw) == List(EnumValueAdded(List("Status"), "ACTIVE", 1)))
      },
      test("enum value removed") {
        val old = parse("""syntax = "proto3"; enum Status { UNKNOWN = 0; ACTIVE = 1; }""")
        val nw  = parse("""syntax = "proto3"; enum Status { UNKNOWN = 0; }""")
        assertTrue(ProtoDiff.diff(old, nw) == List(EnumValueRemoved(List("Status"), "ACTIVE", 1, numberReserved = false)))
      },
      test("enum value number changed") {
        val old = parse("""syntax = "proto3"; enum Status { UNKNOWN = 0; ACTIVE = 1; }""")
        val nw  = parse("""syntax = "proto3"; enum Status { UNKNOWN = 0; ACTIVE = 2; }""")
        assertTrue(ProtoDiff.diff(old, nw) == List(EnumValueNumberChanged(List("Status"), "ACTIVE", 1, 2)))
      },
      test("enum value renamed") {
        val old = parse("""syntax = "proto3"; enum Status { UNKNOWN = 0; ACTIVE = 1; }""")
        val nw  = parse("""syntax = "proto3"; enum Status { UNKNOWN = 0; ENABLED = 1; }""")
        assertTrue(ProtoDiff.diff(old, nw) == List(EnumValueRenamed(List("Status"), 1, "ACTIVE", "ENABLED")))
      }
    ),
    suite("Nested messages")(
      test("change in nested message includes full path") {
        val old     = parse(
          """syntax = "proto3";
            |message Outer {
            |  message Inner {
            |    string name = 1;
            |  }
            |  Inner inner = 1;
            |}
            |""".stripMargin
        )
        val nw      = parse(
          """syntax = "proto3";
            |message Outer {
            |  message Inner {
            |    string name = 1;
            |    int32 id = 2;
            |  }
            |  Inner inner = 1;
            |}
            |""".stripMargin
        )
        val changes = ProtoDiff.diff(old, nw)
        assertTrue(changes == List(FieldAdded(List("Outer", "Inner"), "id", 2)))
      },
      test("nested enum change includes full path") {
        val old     = parse(
          """syntax = "proto3";
            |message Outer {
            |  enum Status { UNKNOWN = 0; }
            |  Status s = 1;
            |}
            |""".stripMargin
        )
        val nw      = parse(
          """syntax = "proto3";
            |message Outer {
            |  enum Status { UNKNOWN = 0; ACTIVE = 1; }
            |  Status s = 1;
            |}
            |""".stripMargin
        )
        val changes = ProtoDiff.diff(old, nw)
        assertTrue(changes == List(EnumValueAdded(List("Outer", "Status"), "ACTIVE", 1)))
      }
    ),
    suite("Service and RPC changes")(
      test("service added") {
        val old     = parse("""syntax = "proto3";""")
        val nw      = parse("""syntax = "proto3"; message Req {} message Resp {} service Svc { rpc Get(Req) returns (Resp); }""")
        val changes = ProtoDiff.diff(old, nw)
        assertTrue(
          changes.contains(ServiceAdded(Nil, "Svc"))
        )
      },
      test("service removed") {
        val old = parse("""syntax = "proto3"; message Req {} message Resp {} service Svc { rpc Get(Req) returns (Resp); }""")
        val nw  = parse("""syntax = "proto3"; message Req {} message Resp {}""")
        assertTrue(ProtoDiff.diff(old, nw).contains(ServiceRemoved(Nil, "Svc")))
      },
      test("rpc added") {
        val old = parse("""syntax = "proto3"; message Req {} message Resp {} service Svc { rpc Get(Req) returns (Resp); }""")
        val nw  = parse(
          """syntax = "proto3"; message Req {} message Resp {} service Svc { rpc Get(Req) returns (Resp); rpc Put(Req) returns (Resp); }"""
        )
        assertTrue(ProtoDiff.diff(old, nw).contains(RpcAdded(List("Svc"), "Put")))
      },
      test("rpc removed") {
        val old = parse(
          """syntax = "proto3"; message Req {} message Resp {} service Svc { rpc Get(Req) returns (Resp); rpc Put(Req) returns (Resp); }"""
        )
        val nw  = parse("""syntax = "proto3"; message Req {} message Resp {} service Svc { rpc Get(Req) returns (Resp); }""")
        assertTrue(ProtoDiff.diff(old, nw).contains(RpcRemoved(List("Svc"), "Put")))
      },
      test("rpc request type changed") {
        val old = parse("""syntax = "proto3"; message A {} message B {} service Svc { rpc Get(A) returns (A); }""")
        val nw  = parse("""syntax = "proto3"; message A {} message B {} service Svc { rpc Get(B) returns (A); }""")
        assertTrue(ProtoDiff.diff(old, nw).contains(RpcRequestTypeChanged(List("Svc"), "Get", "A", "B")))
      },
      test("rpc response type changed") {
        val old = parse("""syntax = "proto3"; message A {} message B {} service Svc { rpc Get(A) returns (A); }""")
        val nw  = parse("""syntax = "proto3"; message A {} message B {} service Svc { rpc Get(A) returns (B); }""")
        assertTrue(ProtoDiff.diff(old, nw).contains(RpcResponseTypeChanged(List("Svc"), "Get", "A", "B")))
      },
      test("rpc streaming changed") {
        val old     = parse("""syntax = "proto3"; message A {} service Svc { rpc Get(A) returns (A); }""")
        val nw      = parse("""syntax = "proto3"; message A {} service Svc { rpc Get(stream A) returns (stream A); }""")
        val changes = ProtoDiff.diff(old, nw)
        assertTrue(
          changes.contains(RpcStreamingChanged(List("Svc"), "Get", "request", wasStreaming = false)),
          changes.contains(RpcStreamingChanged(List("Svc"), "Get", "response", wasStreaming = false))
        )
      }
    ),
    suite("Reserved changes")(
      test("reserved number added") {
        val old = parse("""syntax = "proto3"; message Foo { string name = 1; }""")
        val nw  = parse("""syntax = "proto3"; message Foo { string name = 1; reserved 2; }""")
        assertTrue(ProtoDiff.diff(old, nw) == List(ReservedAdded(List("Foo"), Reserved.Number(2))))
      },
      test("reserved number removed") {
        val old = parse("""syntax = "proto3"; message Foo { string name = 1; reserved 2; }""")
        val nw  = parse("""syntax = "proto3"; message Foo { string name = 1; }""")
        assertTrue(ProtoDiff.diff(old, nw) == List(ReservedRemoved(List("Foo"), Reserved.Number(2))))
      }
    ),
    suite("Option changes")(
      test("top-level option added") {
        val old = parse("""syntax = "proto3";""")
        val nw  = parse("""syntax = "proto3"; option java_package = "com.example";""")
        assertTrue(ProtoDiff.diff(old, nw) == List(OptionAdded(Nil, "java_package")))
      },
      test("top-level option changed") {
        val old     = parse("""syntax = "proto3"; option java_package = "com.old";""")
        val nw      = parse("""syntax = "proto3"; option java_package = "com.new";""")
        val changes = ProtoDiff.diff(old, nw)
        assertTrue(changes == List(OptionChanged(Nil, "java_package", OptionVal.StringLit("com.old"), OptionVal.StringLit("com.new"))))
      }
    ),
    suite("Severity mapping")(
      test("field number change: Error in wire, Info in source") {
        val change = FieldNumberChanged(List("Foo"), "name", 1, 2)
        assertTrue(
          ProtoDiff.severity(change, CompatMode.Wire) == Severity.Error,
          ProtoDiff.severity(change, CompatMode.Source) == Severity.Info,
          ProtoDiff.severity(change, CompatMode.Strictest) == Severity.Error
        )
      },
      test("field renamed: Info in wire, Error in source") {
        val change = FieldRenamed(List("Foo"), 1, "name", "title")
        assertTrue(
          ProtoDiff.severity(change, CompatMode.Wire) == Severity.Info,
          ProtoDiff.severity(change, CompatMode.Source) == Severity.Error,
          ProtoDiff.severity(change, CompatMode.Strictest) == Severity.Error
        )
      },
      test("enum value number change: Error in wire, Info in source") {
        val change = EnumValueNumberChanged(List("Status"), "ACTIVE", 1, 2)
        assertTrue(
          ProtoDiff.severity(change, CompatMode.Wire) == Severity.Error,
          ProtoDiff.severity(change, CompatMode.Source) == Severity.Info
        )
      },
      test("field removed with reservation: Info in wire, Error in source") {
        val change = FieldRemoved(List("Foo"), "name", 1, numberReserved = true)
        assertTrue(
          ProtoDiff.severity(change, CompatMode.Wire) == Severity.Info,
          ProtoDiff.severity(change, CompatMode.Source) == Severity.Error
        )
      },
      test("field removed without reservation: Error in both") {
        val change = FieldRemoved(List("Foo"), "name", 1, numberReserved = false)
        assertTrue(
          ProtoDiff.severity(change, CompatMode.Wire) == Severity.Error,
          ProtoDiff.severity(change, CompatMode.Source) == Severity.Error
        )
      },
      test("field order changed: Info in wire, Warning in source") {
        val change = FieldOrderChanged(List("Foo"))
        assertTrue(
          ProtoDiff.severity(change, CompatMode.Wire) == Severity.Info,
          ProtoDiff.severity(change, CompatMode.Source) == Severity.Warning
        )
      },
      test("message renamed: Info in wire, Error in source") {
        val change = MessageRenamed(Nil, "Foo", "Bar")
        assertTrue(
          ProtoDiff.severity(change, CompatMode.Wire) == Severity.Info,
          ProtoDiff.severity(change, CompatMode.Source) == Severity.Error
        )
      },
      test("field type changed: Error in both") {
        val change = FieldTypeChanged(List("Foo"), "name", 1, Type.String, Type.Int32)
        assertTrue(
          ProtoDiff.severity(change, CompatMode.Wire) == Severity.Error,
          ProtoDiff.severity(change, CompatMode.Source) == Severity.Error
        )
      }
    ),
    suite("Type normalization")(
      test("leading dot in ref type is ignored") {
        val old = parse(
          """syntax = "proto3";
            |message Foo { Bar b = 1; }
            |message Bar { string name = 1; }
            |""".stripMargin
        )
        val nw  = parse(
          """syntax = "proto3";
            |message Foo { .Bar b = 1; }
            |message Bar { string name = 1; }
            |""".stripMargin
        )
        assertTrue(ProtoDiff.diff(old, nw) == Nil)
      }
    )
  )
}
