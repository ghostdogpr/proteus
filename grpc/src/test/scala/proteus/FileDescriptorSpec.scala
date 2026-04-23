package proteus

import scala.jdk.CollectionConverters.*

import zio.blocks.schema.Schema
import zio.test.*

/**
  * Exercises `Service.fileDescriptor` and `Dependency.fileDescriptor` — the runtime descriptors used for
  * gRPC reflection. `render` has its own pipeline that relocates nested types and qualifies references;
  * the descriptor path was previously skipping that, which surfaced as
  * `com.google.protobuf.Descriptors$DescriptorValidationException` at service startup.
  */
object FileDescriptorSpec extends ZIOSpecDefault {

  def spec = suite("FileDescriptorSpec")(
    test("Service.fileDescriptor builds a valid descriptor with flat top-level messages") {
      case class FlatRequest(id: Int) derives Schema, ProtobufCodec
      case class FlatResponse(ok: Boolean) derives Schema, ProtobufCodec

      val service = Service("test.pkg", "FlatService").rpc(Rpc.unary[FlatRequest, FlatResponse]("Flat"))
      val fd      = service.fileDescriptor
      assertTrue(fd.findMessageTypeByName("FlatRequest") != null) &&
        assertTrue(fd.findMessageTypeByName("FlatResponse") != null) &&
        assertTrue(fd.findServiceByName("FlatService") != null)
    },
    test("Service.fileDescriptor nests a type inside its parent when both are present") {
      case class Inner(v: String) derives Schema
      case class Parent(inner: Inner) derives Schema
      case class NestedReq(parent: Parent) derives Schema, ProtobufCodec
      case class NestedResp(ok: Boolean) derives Schema, ProtobufCodec

      given ProtobufDeriver           = ProtobufDeriver.modifier[Inner](Modifiers.nestedIn[Parent])
      given ProtobufCodec[NestedReq]  = Schema[NestedReq].derive(summon[ProtobufDeriver])
      given ProtobufCodec[NestedResp] = Schema[NestedResp].derive(summon[ProtobufDeriver])

      val service = Service("test.pkg", "NestService").rpc(Rpc.unary[NestedReq, NestedResp]("Nest"))
      val fd      = service.fileDescriptor
      val parent  = fd.findMessageTypeByName("Parent")
      assertTrue(parent != null) &&
        assertTrue(parent.findNestedTypeByName("Inner") != null) &&
        assertTrue(fd.findMessageTypeByName("NestedReq").findFieldByName("parent").getMessageType.getFullName == "test.pkg.Parent") &&
        assertTrue(parent.findFieldByName("inner").getMessageType.getFullName == "test.pkg.Parent.Inner")
    },
    test("Service.fileDescriptor: two nested types sharing a short name don't collide (ck-server Trade case)") {
      // Mirrors ck-server's EventExtension.Trade vs EventExtension.CommissionSlot.Trade pattern.
      // Neither `Trade` is referenced from outside its parent, so no ambiguity on field refs.
      case class TradeA(amount: Int) derives Schema
      case class TradeB(other: Int) derives Schema
      case class CommissionSlot(id: Int, t: TradeB) derives Schema
      case class EventExtension(slot: CommissionSlot, a: TradeA) derives Schema
      case class DualReq(ext: EventExtension) derives Schema, ProtobufCodec
      case class DualResp(ok: Boolean) derives Schema, ProtobufCodec

      given ProtobufDeriver =
        ProtobufDeriver
          .modifier[TradeA](Modifiers.rename("Trade"))
          .modifier[TradeA](Modifiers.nestedIn[EventExtension])
          .modifier[TradeB](Modifiers.rename("Trade"))
          .modifier[TradeB](Modifiers.nestedIn[CommissionSlot])
          .modifier[CommissionSlot](Modifiers.nestedIn[EventExtension])
      given ProtobufCodec[DualReq]  = Schema[DualReq].derive(summon[ProtobufDeriver])
      given ProtobufCodec[DualResp] = Schema[DualResp].derive(summon[ProtobufDeriver])

      val service = Service("test.pkg", "DualService").rpc(Rpc.unary[DualReq, DualResp]("Dual"))
      val fd      = service.fileDescriptor
      val ext     = fd.findMessageTypeByName("EventExtension")
      val slot    = ext.findNestedTypeByName("CommissionSlot")
      assertTrue(ext.findNestedTypeByName("Trade") != null) &&
        assertTrue(slot.findNestedTypeByName("Trade") != null) &&
        assertTrue(fd.findMessageTypeByName("Trade") == null) &&
        // Field ref from EventExtension resolves to its own nested Trade
        assertTrue(ext.findFieldByName("a").getMessageType.getFullName == "test.pkg.EventExtension.Trade") &&
        // Field ref from CommissionSlot resolves to its own nested Trade
        assertTrue(slot.findFieldByName("t").getMessageType.getFullName == "test.pkg.EventExtension.CommissionSlot.Trade")
    },
    test("Dependency.fileDescriptor nests a type inside its parent (derived transitively)") {
      case class DepInner(v: String) derives Schema
      case class DepParent(inner: DepInner) derives Schema
      case class DepReq(parent: DepParent) derives Schema, ProtobufCodec
      case class DepResp(ok: Boolean) derives Schema, ProtobufCodec

      given ProtobufDeriver          = ProtobufDeriver.modifier[DepInner](Modifiers.nestedIn[DepParent])
      given ProtobufCodec[DepParent] = Schema[DepParent].derive(summon[ProtobufDeriver])
      given ProtobufCodec[DepReq]    = Schema[DepReq].derive(summon[ProtobufDeriver])
      given ProtobufCodec[DepResp]   = Schema[DepResp].derive(summon[ProtobufDeriver])

      val dep     = Dependency("dep_entities", "test.pkg").add[DepParent]
      val service = Service("test.pkg", "DepSvc").rpc(Rpc.unary[DepReq, DepResp]("DepRpc")).dependsOn(dep)

      val fd       = service.fileDescriptor
      val depFd    = fd.getDependencies.asScala.find(_.getName.endsWith("dep_entities.proto")).getOrElse(throw new AssertionError("dep not imported"))
      val depParent = depFd.findMessageTypeByName("DepParent")
      assertTrue(depParent != null) &&
        assertTrue(depParent.findNestedTypeByName("DepInner") != null) &&
        assertTrue(depFd.findMessageTypeByName("DepInner") == null) &&
        assertTrue(fd.findMessageTypeByName("DepReq").findFieldByName("parent").getMessageType.getFullName == "test.pkg.DepParent")
    },
    test("Cross-file reference: service map<K, V> where V is nested inside a dep's parent (ck-server BingoMission case)") {
      // Reproduces the ck-server KingdomAdditionalEvents → ChangedBingoMissions → BingoMission case.
      case class BingoMission(id: Int) derives Schema
      case class Bingo(x: Int) derives Schema
      case class CrossReq(missions: Map[Int, BingoMission]) derives Schema, ProtobufCodec
      case class CrossResp(ok: Boolean) derives Schema, ProtobufCodec

      given ProtobufDeriver             = ProtobufDeriver.modifier[BingoMission](Modifiers.nestedIn[Bingo])
      given ProtobufCodec[Bingo]        = Schema[Bingo].derive(summon[ProtobufDeriver])
      given ProtobufCodec[BingoMission] = Schema[BingoMission].derive(summon[ProtobufDeriver])
      given ProtobufCodec[CrossReq]     = Schema[CrossReq].derive(summon[ProtobufDeriver])
      given ProtobufCodec[CrossResp]    = Schema[CrossResp].derive(summon[ProtobufDeriver])

      val dep     = Dependency("dep_entities", "test.pkg").add[Bingo].add[BingoMission]
      val service = Service("test.pkg", "CrossSvc").rpc(Rpc.unary[CrossReq, CrossResp]("Cross")).dependsOn(dep)

      val fd       = service.fileDescriptor
      val req      = fd.findMessageTypeByName("CrossReq")
      val mapField = req.findFieldByName("missions")
      assertTrue(mapField.isMapField) &&
        assertTrue(mapField.getMessageType.findFieldByName("value").getMessageType.getFullName == "test.pkg.Bingo.BingoMission")
    },
    test("Service.fileDescriptor: a nestedIn chain deeper than 1 level resolves") {
      case class Leaf(v: String) derives Schema
      case class Mid(leaf: Leaf) derives Schema
      case class Top(mid: Mid) derives Schema
      case class DeepReq(top: Top) derives Schema, ProtobufCodec
      case class DeepResp(ok: Boolean) derives Schema, ProtobufCodec

      given ProtobufDeriver         =
        ProtobufDeriver
          .modifier[Leaf](Modifiers.nestedIn[Mid])
          .modifier[Mid](Modifiers.nestedIn[Top])
      given ProtobufCodec[DeepReq]  = Schema[DeepReq].derive(summon[ProtobufDeriver])
      given ProtobufCodec[DeepResp] = Schema[DeepResp].derive(summon[ProtobufDeriver])

      val service = Service("test.pkg", "DeepSvc").rpc(Rpc.unary[DeepReq, DeepResp]("Deep"))
      val fd      = service.fileDescriptor
      val top     = fd.findMessageTypeByName("Top")
      val mid     = top.findNestedTypeByName("Mid")
      assertTrue(top != null) &&
        assertTrue(mid != null) &&
        assertTrue(mid.findNestedTypeByName("Leaf") != null) &&
        assertTrue(mid.findFieldByName("leaf").getMessageType.getFullName == "test.pkg.Top.Mid.Leaf")
    },
    test("Service.fileDescriptor: two nested types sharing a short name but referenced from outside resolve via typeId") {
      // Both TradeA and TradeB render as `Trade` after rename and live under different parents.
      // The request message has fields of BOTH — if FQN resolution only keys off the short name,
      // one field would resolve to the wrong nested type and the crossLink step would fail.
      case class TradeA(amount: Int) derives Schema
      case class TradeB(other: Int) derives Schema
      case class EvExt(a: TradeA) derives Schema
      case class ComSlot(t: TradeB) derives Schema
      case class AmbigReq(ev: EvExt, cs: ComSlot, tradeA: TradeA, tradeB: TradeB) derives Schema, ProtobufCodec
      case class AmbigResp(ok: Boolean) derives Schema, ProtobufCodec

      given ProtobufDeriver =
        ProtobufDeriver
          .modifier[TradeA](Modifiers.rename("Trade"))
          .modifier[TradeA](Modifiers.nestedIn[EvExt])
          .modifier[TradeB](Modifiers.rename("Trade"))
          .modifier[TradeB](Modifiers.nestedIn[ComSlot])
      given ProtobufCodec[AmbigReq]  = Schema[AmbigReq].derive(summon[ProtobufDeriver])
      given ProtobufCodec[AmbigResp] = Schema[AmbigResp].derive(summon[ProtobufDeriver])

      val service = Service("test.pkg", "AmbigService").rpc(Rpc.unary[AmbigReq, AmbigResp]("Ambig"))
      val fd      = service.fileDescriptor
      val req     = fd.findMessageTypeByName("AmbigReq")
      assertTrue(req.findFieldByName("trade_a").getMessageType.getFullName == "test.pkg.EvExt.Trade") &&
        assertTrue(req.findFieldByName("trade_b").getMessageType.getFullName == "test.pkg.ComSlot.Trade") &&
        assertTrue(fd.findMessageTypeByName("Trade") == null)
    },
    test("Sealed trait with variants nested in the parent (ck-server Payment sealed trait case)") {
      // Payment is a sealed trait with variants. Each variant has `nestedIn[Payment]` so the oneOf
      // field refs resolve to `Payment.Variant`.
      sealed trait Payment derives Schema
      object Payment {
        case class Coin(amount: Int)                     extends Payment derives Schema
        case class EventItem(dataId: Int, amount: Int)   extends Payment derives Schema
      }
      case class Purchase(payment: Payment)    derives Schema, ProtobufCodec
      case class PayReq(p: Purchase)           derives Schema, ProtobufCodec
      case class PayResp(ok: Boolean)          derives Schema, ProtobufCodec

      given ProtobufDeriver =
        ProtobufDeriver
          .modifier[Payment.Coin](Modifiers.nestedIn[Payment])
          .modifier[Payment.EventItem](Modifiers.nestedIn[Payment])
      given ProtobufCodec[PayReq]  = Schema[PayReq].derive(summon[ProtobufDeriver])
      given ProtobufCodec[PayResp] = Schema[PayResp].derive(summon[ProtobufDeriver])

      val service = Service("test.pkg", "PaySvc").rpc(Rpc.unary[PayReq, PayResp]("Pay"))
      val fd      = service.fileDescriptor
      val payment = fd.findMessageTypeByName("Payment")
      assertTrue(payment != null) &&
        assertTrue(payment.findNestedTypeByName("Coin") != null) &&
        assertTrue(payment.findNestedTypeByName("EventItem") != null) &&
        assertTrue(fd.findMessageTypeByName("EventItem") == null) &&
        // Verify the oneof field ref resolves to the nested EventItem, not a ghost top-level one.
        assertTrue(payment.findFieldByName("event_item").getMessageType.getFullName == "test.pkg.Payment.EventItem")
    },
    test("Sealed trait with top-level variants (no nestedIn) in a fromServices dep (ck-server DefenseGame case)") {
      // Reproduces ck-server: Payment is a sealed trait, variants are NOT marked nestedIn, the
      // service uses Payment in a response, the dep is built via `Dependency.fromServices`, and
      // the service then `.dependsOn(gameDep)`. The generated game file descriptor must resolve
      // Payment's oneof refs to the top-level variants.
      sealed trait Payment derives Schema
      object Payment {
        case class Coin(amount: Int)                 extends Payment derives Schema
        case class EventItem(dataId: Int, amount: Int) extends Payment derives Schema
      }
      case class Purchase(payment: Payment) derives Schema, ProtobufCodec
      case class PayReq()                   derives Schema, ProtobufCodec
      case class PayResp(p: Purchase)       derives Schema, ProtobufCodec

      given ProtobufDeriver        = ProtobufDeriver
      given ProtobufCodec[PayReq]  = Schema[PayReq].derive(summon[ProtobufDeriver])
      given ProtobufCodec[PayResp] = Schema[PayResp].derive(summon[ProtobufDeriver])

      val paySvc  = Service("test.game", "PaySvc").rpc(Rpc.unary[PayReq, PayResp]("Pay"))
      val gameDep = Dependency.fromServices("game_entities", "test.game", "game", paySvc)
      val service = Service("test.game", "PaySvc").rpc(Rpc.unary[PayReq, PayResp]("Pay")).dependsOn(gameDep)

      val fd     = service.fileDescriptor
      val gameFd = fd.getDependencies.asScala.find(_.getName.endsWith("game_entities.proto")).getOrElse(throw new AssertionError("game dep missing"))
      val paymentMsg = gameFd.findMessageTypeByName("Payment")
      // Payment.event_item oneof field should resolve to top-level EventItem
      assertTrue(paymentMsg != null) &&
        assertTrue(gameFd.findMessageTypeByName("EventItem") != null) &&
        assertTrue(paymentMsg.findFieldByName("event_item").getMessageType.getFullName == "test.game.EventItem")
    },
    test("Service's own type with a short name that collides with a sub-dep type (different typeIds) stays in the service") {
      // The service defines its own `Item` type. A dep also defines a completely unrelated `Item`.
      // Short-name filtering would drop the service's `Item` and try to resolve its refs through the
      // dep, which points at a different Scala type.
      case class Item(v: String) derives Schema, ProtobufCodec                  // distinct from DepItem
      case class DepItem(other: Int) derives Schema, ProtobufCodec
      case class SvcReq(item: Item) derives Schema, ProtobufCodec
      case class SvcResp(ok: Boolean) derives Schema, ProtobufCodec

      // Rename `DepItem` to `Item` in the dep so the short names collide.
      given ProtobufDeriver                     = ProtobufDeriver.modifier[DepItem](Modifiers.rename("Item"))
      given ProtobufCodec[DepItem]              = Schema[DepItem].derive(summon[ProtobufDeriver])

      val dep     = Dependency("dep_entities", "test.pkg").add[DepItem]
      val service = Service("test.pkg", "SvcSvc").rpc(Rpc.unary[SvcReq, SvcResp]("Svc")).dependsOn(dep)

      val fd = service.fileDescriptor
      // Service's own `Item` (typeId of `Item`, one field `v`) must remain at top-level in the service file.
      val item = fd.findMessageTypeByName("Item")
      assertTrue(item != null) &&
        assertTrue(item.findFieldByName("v") != null) &&
        assertTrue(fd.findMessageTypeByName("SvcReq").findFieldByName("item").getMessageType.getFullName == "test.pkg.Item")
    },
    test("Structurally identical variants in two sealed traits share a short name (ck-server Payment.EventItem vs RewardElement.EventItem)") {
      // ck-server: `Payment` and `RewardElement` are separate sealed traits that each have an
      // `EventItem` variant with identical fields (dataId, amount). Both variants are top-level in
      // the generated proto. `distinctBy(dedupKey)` strips typeIds and dedupes them into one, so
      // the winner's typeId is what's registered — the loser's refs can no longer be resolved.
      sealed trait Payment derives Schema
      object Payment {
        case class EventItem(dataId: Int, amount: Int) extends Payment derives Schema
      }
      sealed trait RewardElement derives Schema
      object RewardElement {
        case class EventItem(dataId: Int, amount: Int) extends RewardElement derives Schema
      }
      case class Bag(payments: List[Payment], rewards: List[RewardElement]) derives Schema, ProtobufCodec
      case class BagReq()          derives Schema, ProtobufCodec
      case class BagResp(b: Bag)   derives Schema, ProtobufCodec

      given ProtobufDeriver        = ProtobufDeriver
      given ProtobufCodec[BagReq]  = Schema[BagReq].derive(summon[ProtobufDeriver])
      given ProtobufCodec[BagResp] = Schema[BagResp].derive(summon[ProtobufDeriver])

      val bagSvc  = Service("test.game", "BagSvc").rpc(Rpc.unary[BagReq, BagResp]("Bag"))
      val gameDep = Dependency.fromServices("game_entities", "test.game", "game", bagSvc)
      val service = Service("test.game", "BagSvc").rpc(Rpc.unary[BagReq, BagResp]("Bag")).dependsOn(gameDep)

      service.fileDescriptor // must build without DescriptorValidationException
      assertCompletes
    },
    test("Two distinct types share a short name: one nested in a parent dep, one top-level in a sub-dep (ck-server Payment.EventItem case)") {
      // `EventItem` here is a variant of the sealed trait `Payment` (nested under Payment in `game`).
      // A completely unrelated `EventItem` lives in `common`. The name collision must not drop the
      // Payment variant from `game` — that would leave Payment's oneOf ref dangling.
      case class EventItem(dataId: Int, amount: Int) derives Schema, ProtobufCodec // top-level, in common

      sealed trait Payment derives Schema
      object Payment {
        case class Coin(amount: Int)                   extends Payment derives Schema
        // Same short name as the common dep's EventItem, but an entirely different Scala type.
        case class EventItem(dataId: Int, amount: Int) extends Payment derives Schema
      }
      case class Purchase(payment: Payment) derives Schema, ProtobufCodec
      case class PayReq(p: Purchase)        derives Schema, ProtobufCodec
      case class PayResp(ok: Boolean)       derives Schema, ProtobufCodec

      given ProtobufDeriver =
        ProtobufDeriver
          .modifier[Payment.Coin](Modifiers.nestedIn[Payment])
          .modifier[Payment.EventItem](Modifiers.nestedIn[Payment])
      given ProtobufCodec[PayReq]  = Schema[PayReq].derive(summon[ProtobufDeriver])
      given ProtobufCodec[PayResp] = Schema[PayResp].derive(summon[ProtobufDeriver])

      val commonDep = Dependency("common_entities", "test.common", "common").add[EventItem]
      val gameDep   = Dependency("game_entities", "test.game", "game").add[Purchase].dependsOn(commonDep)
      val service   = Service("test.game", "PaySvc").rpc(Rpc.unary[PayReq, PayResp]("Pay")).dependsOn(gameDep)

      val fd      = service.fileDescriptor
      val gameFd  = fd.getDependencies.asScala.find(_.getName.endsWith("game_entities.proto")).getOrElse(throw new AssertionError("game dep missing"))
      val payment = gameFd.findMessageTypeByName("Payment")
      assertTrue(payment != null) &&
        assertTrue(payment.findNestedTypeByName("EventItem") != null) &&
        assertTrue(payment.findFieldByName("event_item").getMessageType.getFullName == "test.game.Payment.EventItem")
    },
    test("A type in a dep references a type in a sub-dep (different package, ck-server Payment→EventItem case)") {
      // Payment lives in `game` entities; EventItem lives in `common` entities; game depends on common.
      // The service only sees game via its dependency chain but the resulting file descriptor for
      // `game` must resolve EventItem refs to their `common`-package FQN.
      case class EventItem(id: Int) derives Schema, ProtobufCodec
      case class Payment(item: EventItem) derives Schema, ProtobufCodec
      case class PayReq(payment: Payment) derives Schema, ProtobufCodec
      case class PayResp(ok: Boolean) derives Schema, ProtobufCodec

      val commonDep = Dependency("common_entities", "test.common", "common").add[EventItem]
      val gameDep   = Dependency("game_entities", "test.game", "game").add[Payment].dependsOn(commonDep)
      val service   = Service("test.game", "PaySvc").rpc(Rpc.unary[PayReq, PayResp]("Pay")).dependsOn(gameDep)

      val fd         = service.fileDescriptor
      val gameFd     = fd.getDependencies.asScala.find(_.getName.endsWith("game_entities.proto")).getOrElse(throw new AssertionError("game dep missing"))
      val paymentMsg = gameFd.findMessageTypeByName("Payment")
      // The `item` field on Payment must point at EventItem's true FQN in the common package.
      assertTrue(paymentMsg.findFieldByName("item").getMessageType.getFullName == "test.common.EventItem")
    },
    test("relocateNestedIn is idempotent: adding a child both as a dep entry and via its parent doesn't duplicate") {
      case class DepInner(v: String) derives Schema
      case class DepParent(inner: DepInner) derives Schema
      case class DepReq(parent: DepParent, inner: DepInner) derives Schema, ProtobufCodec
      case class DepResp(ok: Boolean) derives Schema, ProtobufCodec

      given ProtobufDeriver          = ProtobufDeriver.modifier[DepInner](Modifiers.nestedIn[DepParent])
      given ProtobufCodec[DepParent] = Schema[DepParent].derive(summon[ProtobufDeriver])
      given ProtobufCodec[DepInner]  = Schema[DepInner].derive(summon[ProtobufDeriver])
      given ProtobufCodec[DepReq]    = Schema[DepReq].derive(summon[ProtobufDeriver])
      given ProtobufCodec[DepResp]   = Schema[DepResp].derive(summon[ProtobufDeriver])

      // Add both explicitly — DepInner is pulled in by both .add calls (DepParent transitively + DepInner directly).
      val dep     = Dependency("dep_entities", "test.pkg").add[DepParent].add[DepInner]
      val service = Service("test.pkg", "DepSvc").rpc(Rpc.unary[DepReq, DepResp]("DepRpc")).dependsOn(dep)

      val fd       = service.fileDescriptor
      val depFd    = fd.getDependencies.asScala.find(_.getName.endsWith("dep_entities.proto")).getOrElse(throw new AssertionError("dep not imported"))
      val depParent = depFd.findMessageTypeByName("DepParent")
      assertTrue(depParent != null) &&
        assertTrue(depParent.findNestedTypeByName("DepInner") != null) &&
        // exactly one DepInner nested, not two
        assertTrue(depParent.getNestedTypes.asScala.count(_.getName == "DepInner") == 1)
    }
  )
}
