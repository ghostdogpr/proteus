package proteus

import zio.test.*

import proteus.ProtoIR.*
import proteus.internal.Renderer

object ProtoParserSpec extends ZIOSpecDefault {

  def spec = suite("ProtoParserSpec")(
    suite("Top-level")(
      test("syntax, package and message") {
        val input = """syntax = "proto3";
                      |
                      |package test;
                      |
                      |message SimpleMessage {
                      |    int32 id = 1;
                      |    string name = 2;
                      |    bool active = 3;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              Some("test"),
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "SimpleMessage",
                      List(
                        MessageElement.FieldElement(Field(Type.Int32, "id", 1, optional = false, comment = None)),
                        MessageElement.FieldElement(Field(Type.String, "name", 2, optional = false, comment = None)),
                        MessageElement.FieldElement(Field(Type.Bool, "active", 3, optional = false, comment = None))
                      ),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("without package") {
        val input = """syntax = "proto3";
                      |
                      |message Foo {
                      |    string bar = 1;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "Foo",
                      List(MessageElement.FieldElement(Field(Type.String, "bar", 1, optional = false, comment = None))),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("rejects non-proto3 syntax") {
        val input = """syntax = "proto2";
                      |
                      |package test;
                      |
                      |message Foo {}
                      |""".stripMargin

        assertTrue(ProtoParser.parse(input).isLeft)
      },
      test("without syntax or package") {
        val input = """message Foo {
                      |    string bar = 1;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "Foo",
                      List(MessageElement.FieldElement(Field(Type.String, "bar", 1, optional = false, comment = None))),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("imports") {
        val input = """syntax = "proto3";
                      |
                      |package test;
                      |
                      |import "other.proto";
                      |import "google/protobuf/timestamp.proto";
                      |
                      |message Foo {
                      |    string bar = 1;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              Some("test"),
              List(
                Statement.ImportStatement("other.proto"),
                Statement.ImportStatement("google/protobuf/timestamp.proto"),
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "Foo",
                      List(MessageElement.FieldElement(Field(Type.String, "bar", 1, optional = false, comment = None))),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("import modifiers") {
        val input = """syntax = "proto3";
                      |
                      |package test;
                      |
                      |import public "other.proto";
                      |import weak "legacy.proto";
                      |
                      |message Foo {}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              Some("test"),
              List(
                Statement.ImportStatement("other.proto", Some("public")),
                Statement.ImportStatement("legacy.proto", Some("weak")),
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(Message("Foo", List.empty, reserved = List.empty))
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("top-level string options") {
        val input = """syntax = "proto3";
                      |
                      |package test;
                      |
                      |option java_package = "com.example.test";
                      |option go_package = "test/pb";
                      |
                      |message Foo {
                      |    string bar = 1;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              Some("test"),
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "Foo",
                      List(MessageElement.FieldElement(Field(Type.String, "bar", 1, optional = false, comment = None))),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List(
                TopLevelOption("java_package", "com.example.test"),
                TopLevelOption("go_package", "test/pb")
              )
            )
          )
        )
      },
      test("rich top-level options") {
        val input = """syntax = "proto3";
                      |
                      |package test;
                      |
                      |option java_multiple_files = true;
                      |option optimize_for = CODE_SIZE;
                      |option (spec.analytics) = {
                      |    track: {}
                      |};
                      |
                      |message Foo {}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              Some("test"),
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(Message("Foo", List.empty, reserved = List.empty))
                )
              ),
              options = List(
                TopLevelOption(OptionName.BuiltIn("java_multiple_files"), OptionVal.BoolLit(true)),
                TopLevelOption(OptionName.BuiltIn("optimize_for"), OptionVal.Identifier("CODE_SIZE")),
                TopLevelOption(
                  OptionName.Extension("spec.analytics"),
                  OptionVal.MessageValue(List("track" -> OptionVal.MessageValue()))
                )
              )
            )
          )
        )
      },
      test("fully qualified identifier in option value") {
        val input = """syntax = "proto3";
                      |
                      |option optimize_for = SPEED;
                      |option (custom) = some.package.VALUE;
                      |
                      |message Foo {}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(Message("Foo", List.empty, reserved = List.empty))
                )
              ),
              options = List(
                TopLevelOption(OptionName.BuiltIn("optimize_for"), OptionVal.Identifier("SPEED")),
                TopLevelOption(OptionName.Extension("custom"), OptionVal.Identifier("some.package.VALUE"))
              )
            )
          )
        )
      },
      test("empty statements") {
        val input = """syntax = "proto3";
                      |
                      |;
                      |
                      |message Foo {
                      |    ;
                      |    string bar = 1;
                      |}
                      |
                      |enum Status {
                      |    ;
                      |    ACTIVE = 0;
                      |}
                      |
                      |service Svc {
                      |    ;
                      |    rpc Foo (Req) returns (Resp) {
                      |        ;
                      |    }
                      |}
                      |""".stripMargin

        assertTrue(ProtoParser.parse(input).isRight)
      }
    ),
    suite("Messages")(
      test("optional field") {
        val input = """syntax = "proto3";
                      |
                      |message Msg {
                      |    int32 id = 1;
                      |    optional string value = 2;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "Msg",
                      List(
                        MessageElement.FieldElement(Field(Type.Int32, "id", 1, optional = false, comment = None)),
                        MessageElement.FieldElement(Field(Type.String, "value", 2, optional = true, comment = None))
                      ),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("repeated field") {
        val input = """syntax = "proto3";
                      |
                      |message Msg {
                      |    repeated string tags = 1;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "Msg",
                      List(
                        MessageElement.FieldElement(
                          Field(Type.ListType(Type.String), "tags", 1, optional = false, comment = None)
                        )
                      ),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("map field") {
        val input = """syntax = "proto3";
                      |
                      |message Msg {
                      |    map<string, int32> values = 1;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "Msg",
                      List(
                        MessageElement.FieldElement(
                          Field(Type.MapType(Type.String, Type.Int32), "values", 1, optional = false, comment = None)
                        )
                      ),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("field options") {
        val input = """syntax = "proto3";
                      |
                      |message Msg {
                      |    int32 old_field = 1 [deprecated = true];
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "Msg",
                      List(
                        MessageElement.FieldElement(
                          Field(
                            Type.Int32,
                            "old_field",
                            1,
                            optional = false,
                            comment = None,
                            options = List(OptionValue(OptionName.BuiltIn("deprecated"), OptionVal.BoolLit(true)))
                          )
                        )
                      ),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("list-valued and negative float option values") {
        val input = """syntax = "proto3";
                      |
                      |message Msg {
                      |    int32 value = 1 [
                      |        samples = [-1.5, inf, -inf, nan],
                      |        labels = ["a", "b"],
                      |        entries = [{ key: "x" }, { key: "y" }]
                      |    ];
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result.isRight,
          result.toOption.exists {
            case CompilationUnit(
                  _,
                  List(
                    Statement.TopLevelStatement(
                      TopLevelDef.MessageDef(Message(_, List(MessageElement.FieldElement(Field(_, _, _, _, _, options))), _, _, _, _))
                    )
                  ),
                  _
                ) =>
              val hasExpectedSamples = options
                .collectFirst {
                  case OptionValue(
                        OptionName.BuiltIn("samples"),
                        OptionVal.ScalarList(
                          List(OptionVal.FloatLit(v1), OptionVal.FloatLit(v2), OptionVal.FloatLit(v3), OptionVal.FloatLit(v4))
                        )
                      ) =>
                    v1 == -1.5 && v2.isPosInfinity && v3.isNegInfinity && v4.isNaN
                }
                .contains(true)
              val hasExpectedLabels  = options.contains(
                OptionValue(
                  OptionName.BuiltIn("labels"),
                  OptionVal.ScalarList(List(OptionVal.StringLit("a"), OptionVal.StringLit("b")))
                )
              )
              val hasExpectedEntries = options.contains(
                OptionValue(
                  OptionName.BuiltIn("entries"),
                  OptionVal.MessageList(
                    List(
                      OptionVal.MessageValue(List("key" -> OptionVal.StringLit("x"))),
                      OptionVal.MessageValue(List("key" -> OptionVal.StringLit("y")))
                    )
                  )
                )
              )
              hasExpectedSamples && hasExpectedLabels && hasExpectedEntries
            case _ => false
          }
        )
      },
      test("leading dot in type reference") {
        val input = """syntax = "proto3";
                      |message Foo {
                      |    .google.protobuf.Timestamp created_at = 1;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "Foo",
                      List(
                        MessageElement.FieldElement(
                          Field(Type.RefType(".google.protobuf.Timestamp"), "created_at", 1, optional = false, comment = None)
                        )
                      ),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("leading dot in option extension name") {
        val input = """syntax = "proto3";
                      |message Foo {
                      |    int32 x = 1 [(.my_package.my_option) = true];
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "Foo",
                      List(
                        MessageElement.FieldElement(
                          Field(
                            Type.Int32,
                            "x",
                            1,
                            optional = false,
                            comment = None,
                            options = List(
                              OptionValue(OptionName.Extension(".my_package.my_option"), OptionVal.BoolLit(true))
                            )
                          )
                        )
                      ),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("reserved numbers, ranges and names") {
        val input = """syntax = "proto3";
                      |
                      |message Movie {
                      |    reserved 2, 5 to 10;
                      |    reserved "old_field";
                      |    int32 id = 1;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "Movie",
                      List(MessageElement.FieldElement(Field(Type.Int32, "id", 1, optional = false, comment = None))),
                      reserved = List(Reserved.Number(2), Reserved.Range(5, 10), Reserved.Name("old_field"))
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("extend block is parsed and ignored") {
        val input  = """syntax = "proto3";
                      |package ext;
                      |import "google/protobuf/descriptor.proto";
                      |
                      |extend google.protobuf.FieldOptions {
                      |  Opts opts = 2001;
                      |}
                      |
                      |message Opts {
                      |  bool not_null = 1;
                      |}
                      |""".stripMargin
        val result = ProtoParser.parse(input)
        assertTrue(
          result.isRight,
          result.toOption.exists(_.statements.collect { case Statement.TopLevelStatement(TopLevelDef.MessageDef(m)) => m.name } == List("Opts"))
        )
      },
      test("reserved preceded by a comment") {
        val input  = """syntax = "proto3";
                      |message Foo {
                      |    // a comment
                      |    reserved 5, 6;
                      |    int32 id = 1;
                      |}
                      |""".stripMargin
        val result = ProtoParser.parse(input)
        assertTrue(
          result.map(_.statements.collect { case Statement.TopLevelStatement(TopLevelDef.MessageDef(m)) => m.reserved }) ==
            Right(List(List(Reserved.Number(5), Reserved.Number(6))))
        )
      },
      test("reserved range with max") {
        val input = """syntax = "proto3";
                      |message Foo {
                      |    reserved 100 to max;
                      |    string bar = 1;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "Foo",
                      List(MessageElement.FieldElement(Field(Type.String, "bar", 1, optional = false, comment = None))),
                      reserved = List(Reserved.Range(100, 536870911))
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("rejects invalid field numbers") {
        val zero     = """syntax = "proto3";
                     |message Foo {
                     |    string bar = 0;
                     |}
                     |""".stripMargin
        val negative = """syntax = "proto3";
                         |message Foo {
                         |    string bar = -1;
                         |}
                         |""".stripMargin
        val reserved = """syntax = "proto3";
                         |message Foo {
                         |    string bar = 19000;
                         |}
                         |""".stripMargin
        val tooLarge = """syntax = "proto3";
                         |message Foo {
                         |    string bar = 3000000000;
                         |}
                         |""".stripMargin

        assertTrue(
          ProtoParser.parse(zero).isLeft,
          ProtoParser.parse(negative).isLeft,
          ProtoParser.parse(reserved).isLeft,
          ProtoParser.parse(tooLarge).isLeft
        )
      },
      test("rejects invalid reserved numbers and ranges") {
        val reservedImpl  = """syntax = "proto3";
                            |message Foo {
                            |    reserved 19000;
                            |}
                            |""".stripMargin
        val reversedRange = """syntax = "proto3";
                              |message Foo {
                              |    reserved 10 to 5;
                              |}
                              |""".stripMargin

        assertTrue(
          ProtoParser.parse(reservedImpl).isLeft,
          ProtoParser.parse(reversedRange).isLeft
        )
      },
      test("nested messages and enums") {
        val input = """syntax = "proto3";
                      |
                      |message Outer {
                      |    message Inner {
                      |        string value = 1;
                      |    }
                      |    enum Color {
                      |        RED = 0;
                      |        GREEN = 1;
                      |    }
                      |    Inner inner = 1;
                      |    Color color = 2;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "Outer",
                      List(
                        MessageElement.NestedMessageElement(
                          Message(
                            "Inner",
                            List(MessageElement.FieldElement(Field(Type.String, "value", 1, optional = false, comment = None))),
                            reserved = List.empty,
                            nested = true
                          )
                        ),
                        MessageElement.NestedEnumElement(
                          Enum(
                            "Color",
                            List(EnumValue("RED", 0), EnumValue("GREEN", 1)),
                            reserved = List.empty,
                            nested = true
                          )
                        ),
                        MessageElement.FieldElement(Field(Type.RefType("Inner"), "inner", 1, optional = false, comment = None)),
                        MessageElement.FieldElement(Field(Type.RefType("Color"), "color", 2, optional = false, comment = None))
                      ),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("oneof") {
        val input = """syntax = "proto3";
                      |
                      |message ReleaseStatus {
                      |    oneof value {
                      |        Released released = 1;
                      |        Unreleased unreleased = 2;
                      |    }
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "ReleaseStatus",
                      List(
                        MessageElement.OneOfElement(
                          OneOf(
                            "value",
                            List(
                              Field(Type.RefType("Released"), "released", 1, optional = false, comment = None),
                              Field(Type.RefType("Unreleased"), "unreleased", 2, optional = false, comment = None)
                            )
                          )
                        )
                      ),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("oneof with options") {
        val input = """syntax = "proto3";
                      |
                      |message ContactMessage {
                      |    oneof contact {
                      |        option (spec.type) = FIELD_TYPE_STRING;
                      |        Email email = 1;
                      |    }
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "ContactMessage",
                      List(
                        MessageElement.OneOfElement(
                          OneOf(
                            "contact",
                            List(Field(Type.RefType("Email"), "email", 1, optional = false, comment = None)),
                            options = List(
                              OptionValue(OptionName.Extension("spec.type"), OptionVal.Identifier("FIELD_TYPE_STRING"))
                            )
                          )
                        )
                      ),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      }
    ),
    suite("Enums")(
      test("basic enum") {
        val input = """syntax = "proto3";
                      |
                      |enum Status {
                      |    ACTIVE = 0;
                      |    INACTIVE = 1;
                      |    PENDING = 2;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.EnumDef(
                    Enum(
                      "Status",
                      List(EnumValue("ACTIVE", 0), EnumValue("INACTIVE", 1), EnumValue("PENDING", 2)),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("octal integer literal") {
        val input = """syntax = "proto3";
                      |message Foo {
                      |    reserved 010;
                      |    string bar = 1;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "Foo",
                      List(MessageElement.FieldElement(Field(Type.String, "bar", 1, optional = false, comment = None))),
                      reserved = List(Reserved.Number(8))
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("hex integer literal with uppercase X") {
        val input = """syntax = "proto3";
                      |enum Foo {
                      |    BAR = 0;
                      |    BAZ = 0X1A;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.EnumDef(
                    Enum("Foo", List(EnumValue("BAR", 0), EnumValue("BAZ", 26)), reserved = List.empty)
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("rejects enum value outside int32 range") {
        val input = """syntax = "proto3";
                      |enum Foo {
                      |    BAR = 3000000000;
                      |}
                      |""".stripMargin

        assertTrue(ProtoParser.parse(input).isLeft)
      }
    ),
    suite("Services")(
      test("service with rpcs") {
        val input = """syntax = "proto3";
                      |
                      |package examples;
                      |
                      |service Greeter {
                      |    rpc SayHello (HelloRequest) returns (HelloReply) {}
                      |}
                      |
                      |message HelloRequest {
                      |    string name = 1;
                      |}
                      |
                      |message HelloReply {
                      |    string message = 1;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              Some("examples"),
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.ServiceDef(
                    Service(
                      "Greeter",
                      List(
                        Rpc(
                          "SayHello",
                          RpcMessage(Fqn(None, "HelloRequest")),
                          RpcMessage(Fqn(None, "HelloReply")),
                          streamingRequest = false,
                          streamingResponse = false,
                          comment = None,
                          options = List.empty
                        )
                      )
                    )
                  )
                ),
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "HelloRequest",
                      List(MessageElement.FieldElement(Field(Type.String, "name", 1, optional = false, comment = None))),
                      reserved = List.empty
                    )
                  )
                ),
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "HelloReply",
                      List(MessageElement.FieldElement(Field(Type.String, "message", 1, optional = false, comment = None))),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("streaming rpcs") {
        val input = """syntax = "proto3";
                      |
                      |service StreamService {
                      |    rpc ClientStream (stream Request) returns (Response) {}
                      |    rpc ServerStream (Request) returns (stream Response) {}
                      |    rpc BidiStream (stream Request) returns (stream Response) {}
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.ServiceDef(
                    Service(
                      "StreamService",
                      List(
                        Rpc(
                          "ClientStream",
                          RpcMessage(Fqn(None, "Request")),
                          RpcMessage(Fqn(None, "Response")),
                          streamingRequest = true,
                          streamingResponse = false,
                          comment = None,
                          options = List.empty
                        ),
                        Rpc(
                          "ServerStream",
                          RpcMessage(Fqn(None, "Request")),
                          RpcMessage(Fqn(None, "Response")),
                          streamingRequest = false,
                          streamingResponse = true,
                          comment = None,
                          options = List.empty
                        ),
                        Rpc(
                          "BidiStream",
                          RpcMessage(Fqn(None, "Request")),
                          RpcMessage(Fqn(None, "Response")),
                          streamingRequest = true,
                          streamingResponse = true,
                          comment = None,
                          options = List.empty
                        )
                      )
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("rpc options") {
        val input = """syntax = "proto3";
                      |
                      |service Greeter {
                      |    rpc SayHello (HelloRequest) returns (HelloReply) {
                      |        option deprecated = true;
                      |        option (spec.analytics) = {
                      |            track: {}
                      |        };
                      |    }
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.ServiceDef(
                    Service(
                      "Greeter",
                      List(
                        Rpc(
                          "SayHello",
                          RpcMessage(Fqn(None, "HelloRequest")),
                          RpcMessage(Fqn(None, "HelloReply")),
                          streamingRequest = false,
                          streamingResponse = false,
                          comment = None,
                          options = List(
                            OptionValue(OptionName.BuiltIn("deprecated"), OptionVal.BoolLit(true)),
                            OptionValue(
                              OptionName.Extension("spec.analytics"),
                              OptionVal.MessageValue(List("track" -> OptionVal.MessageValue()))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("leading dot in rpc message type") {
        val input = """syntax = "proto3";
                      |service Svc {
                      |    rpc Foo (.pkg.Request) returns (.pkg.Response) {}
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.ServiceDef(
                    Service(
                      "Svc",
                      List(
                        Rpc(
                          "Foo",
                          RpcMessage(Fqn(Some(List("", "pkg")), "Request")),
                          RpcMessage(Fqn(Some(List("", "pkg")), "Response")),
                          streamingRequest = false,
                          streamingResponse = false,
                          comment = None,
                          options = List.empty
                        )
                      )
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("service-level options") {
        val input = """syntax = "proto3";
                      |service Svc {
                      |    option deprecated = true;
                      |    rpc Foo (Req) returns (Resp) {}
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.ServiceDef(
                    Service(
                      "Svc",
                      List(
                        Rpc(
                          "Foo",
                          RpcMessage(Fqn(None, "Req")),
                          RpcMessage(Fqn(None, "Resp")),
                          streamingRequest = false,
                          streamingResponse = false,
                          comment = None,
                          options = List.empty
                        )
                      ),
                      options = List(
                        OptionValue(OptionName.BuiltIn("deprecated"), OptionVal.BoolLit(true))
                      )
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      }
    ),
    suite("Comments")(
      test("field and oneof comments") {
        val input = """syntax = "proto3";
                      |
                      |message ContactMessage {
                      |    int32 id = 1; // User identifier
                      |    // Preferred contact method
                      |    oneof contact {
                      |        // Email address
                      |        Email email = 2;
                      |        Phone phone = 3; // Mobile number
                      |    }
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "ContactMessage",
                      List(
                        MessageElement.FieldElement(
                          Field(Type.Int32, "id", 1, optional = false, comment = Some("User identifier"))
                        ),
                        MessageElement.OneOfElement(
                          OneOf(
                            "contact",
                            List(
                              Field(Type.RefType("Email"), "email", 2, optional = false, comment = Some("Email address")),
                              Field(Type.RefType("Phone"), "phone", 3, optional = false, comment = Some("Mobile number"))
                            ),
                            comment = Some("Preferred contact method")
                          )
                        )
                      ),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("single-line block comment before message") {
        val input = """syntax = "proto3";
                      |/* A user record */
                      |message User {
                      |    string name = 1;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "User",
                      List(MessageElement.FieldElement(Field(Type.String, "name", 1, optional = false, comment = None))),
                      reserved = List.empty,
                      comment = Some("A user record")
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("comments before package and import statements") {
        val input = """syntax = "proto3";
                      |// Package docs
                      |package test;
                      |
                      |/* shared types */
                      |import "shared.proto";
                      |
                      |message User {}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              Some("test"),
              List(
                Statement.ImportStatement("shared.proto"),
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message("User", List.empty, reserved = List.empty)
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("comments before top-level and message options") {
        val input = """syntax = "proto3";
                      |// file option
                      |option optimize_for = SPEED;
                      |
                      |message User {
                      |    // field settings
                      |    option deprecated = true;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "User",
                      List.empty,
                      reserved = List.empty,
                      options = List(
                        OptionValue(OptionName.BuiltIn("deprecated"), OptionVal.BoolLit(true))
                      )
                    )
                  )
                )
              ),
              options = List(
                TopLevelOption(OptionName.BuiltIn("optimize_for"), OptionVal.Identifier("SPEED"))
              )
            )
          )
        )
      },
      test("comments before service options") {
        val input = """syntax = "proto3";
                      |service Svc {
                      |    // service settings
                      |    option deprecated = true;
                      |    rpc Foo (Req) returns (Resp) {}
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.ServiceDef(
                    Service(
                      "Svc",
                      List(
                        Rpc(
                          "Foo",
                          RpcMessage(Fqn(None, "Req")),
                          RpcMessage(Fqn(None, "Resp")),
                          streamingRequest = false,
                          streamingResponse = false,
                          comment = None,
                          options = List.empty
                        )
                      ),
                      options = List(
                        OptionValue(OptionName.BuiltIn("deprecated"), OptionVal.BoolLit(true))
                      )
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("multi-line block comment before field") {
        val input = """syntax = "proto3";
                      |message User {
                      |    /**
                      |     * The user's full name.
                      |     * Must not be empty.
                      |     */
                      |    string name = 1;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.MessageDef(
                    Message(
                      "User",
                      List(
                        MessageElement.FieldElement(
                          Field(Type.String, "name", 1, optional = false, comment = Some("The user's full name.\nMust not be empty."))
                        )
                      ),
                      reserved = List.empty
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      },
      test("block comment before enum") {
        val input = """syntax = "proto3";
                      |/* Status codes */
                      |enum Status {
                      |    ACTIVE = 0;
                      |}
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result == Right(
            CompilationUnit(
              None,
              List(
                Statement.TopLevelStatement(
                  TopLevelDef.EnumDef(
                    Enum(
                      "Status",
                      List(EnumValue("ACTIVE", 0)),
                      reserved = List.empty,
                      comment = Some("Status codes")
                    )
                  )
                )
              ),
              options = List.empty
            )
          )
        )
      }
    ),
    suite("Keyword boundaries")(
      test("type named doubleValue") {
        val input = """syntax = "proto3";
                      |message Foo { doubleValue value = 1; }
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result.toOption.get.statements.collect { case Statement.TopLevelStatement(TopLevelDef.MessageDef(m)) =>
            m.elements.head.asInstanceOf[MessageElement.FieldElement].field.ty
          }.head == Type.RefType("doubleValue")
        )
      },
      test("type named optionalValue") {
        val input = """syntax = "proto3";
                      |message Foo { optionalValue value = 1; }
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result.toOption.get.statements.collect { case Statement.TopLevelStatement(TopLevelDef.MessageDef(m)) =>
            m.elements.head.asInstanceOf[MessageElement.FieldElement].field
          }.head == Field(Type.RefType("optionalValue"), "value", 1, optional = false, comment = None)
        )
      },
      test("type named streamData in rpc context") {
        val input = """syntax = "proto3";
                      |message Foo { streamData value = 1; }
                      |""".stripMargin

        val result = ProtoParser.parse(input)
        assertTrue(
          result.toOption.get.statements.collect { case Statement.TopLevelStatement(TopLevelDef.MessageDef(m)) =>
            m.elements.head.asInstanceOf[MessageElement.FieldElement].field.ty
          }.head == Type.RefType("streamData")
        )
      }
    ),
    suite("Round-trip")(
      test("simple message") {
        val input = """syntax = "proto3";

package test;

message SimpleMessage {
    int32 id = 1;
    string name = 2;
    bool active = 3;
}
"""
        assertRoundTrip(input)
      },
      test("enum") {
        val input = """syntax = "proto3";

package test;

enum Status {
    ACTIVE = 0;
    INACTIVE = 1;
    PENDING = 2;
}
"""
        assertRoundTrip(input)
      },
      test("service") {
        val input = """syntax = "proto3";

package examples;

service Greeter {
    rpc SayHello (HelloRequest) returns (HelloReply) {}
}

message HelloRequest {
    string name = 1;
}

message HelloReply {
    string message = 1;
}
"""
        assertRoundTrip(input)
      },
      test("comments and import modifiers") {
        val input = """syntax = "proto3";

package test;

import public "other.proto";

message ContactMessage {
    int32 id = 1; // User identifier
    // Preferred contact method
    oneof contact {
        Email email = 2;
        Phone phone = 3; // Mobile number
    }
}
"""
        assertRoundTrip(input)
      },
      test("rich options") {
        val input = """syntax = "proto3";

package test;

option java_multiple_files = true;
option optimize_for = CODE_SIZE;
option (spec.analytics) = {
    track: {}
};

message ContactMessage {
    oneof contact {
        option (spec.type) = FIELD_TYPE_STRING;
        Email email = 1;
    }
}

service Greeter {
    rpc SayHello (HelloRequest) returns (HelloReply) {
        option deprecated = true;
    }
}
"""
        assertRoundTrip(input)
      }
    )
  )

  private def assertRoundTrip(input: String) = {
    val result = ProtoParser.parse(input)
    assertTrue(
      result.isRight,
      Renderer.render(result.toOption.get) == input
    )
  }
}
