package proteus

import scala.jdk.CollectionConverters.*

import zio.blocks.schema.Schema
import zio.test.*

/**
  * Covers runtime `FileDescriptor` generation used by gRPC reflection — paths that bypass the
  * `render` pipeline and need their own handling of nested types, cross-file refs, and dedup.
  */
object FileDescriptorSpec extends ZIOSpecDefault {

  private def depFd(fd: com.google.protobuf.Descriptors.FileDescriptor, name: String) =
    fd.getDependencies.asScala.find(_.getName.endsWith(s"$name.proto")).getOrElse(throw new AssertionError(s"dep $name missing"))

  def spec = suite("FileDescriptorSpec")(
    test("flat top-level messages") {
      case class Req(id: Int) derives Schema, ProtobufCodec
      case class Resp(ok: Boolean) derives Schema, ProtobufCodec

      val fd = Service("test.pkg", "Svc").rpc(Rpc.unary[Req, Resp]("Do")).fileDescriptor
      assertTrue(fd.findMessageTypeByName("Req") != null) &&
        assertTrue(fd.findMessageTypeByName("Resp") != null) &&
        assertTrue(fd.findServiceByName("Svc") != null)
    },
    test("nestedIn: child lives under its parent and refs resolve to the nested FQN") {
      case class Inner(v: String) derives Schema
      case class Parent(inner: Inner) derives Schema
      case class Req(parent: Parent) derives Schema, ProtobufCodec
      case class Resp(ok: Boolean) derives Schema, ProtobufCodec

      given ProtobufDeriver     = ProtobufDeriver.modifier[Inner](Modifiers.nestedIn[Parent])
      given ProtobufCodec[Req]  = Schema[Req].derive(summon[ProtobufDeriver])
      given ProtobufCodec[Resp] = Schema[Resp].derive(summon[ProtobufDeriver])

      val fd     = Service("test.pkg", "Svc").rpc(Rpc.unary[Req, Resp]("Do")).fileDescriptor
      val parent = fd.findMessageTypeByName("Parent")
      assertTrue(parent.findNestedTypeByName("Inner") != null) &&
        assertTrue(parent.findFieldByName("inner").getMessageType.getFullName == "test.pkg.Parent.Inner")
    },
    test("nestedIn chain deeper than one level") {
      case class Leaf(v: String) derives Schema
      case class Mid(leaf: Leaf) derives Schema
      case class Top(mid: Mid) derives Schema
      case class Req(top: Top) derives Schema, ProtobufCodec
      case class Resp(ok: Boolean) derives Schema, ProtobufCodec

      given ProtobufDeriver     =
        ProtobufDeriver.modifier[Leaf](Modifiers.nestedIn[Mid]).modifier[Mid](Modifiers.nestedIn[Top])
      given ProtobufCodec[Req]  = Schema[Req].derive(summon[ProtobufDeriver])
      given ProtobufCodec[Resp] = Schema[Resp].derive(summon[ProtobufDeriver])

      val fd  = Service("test.pkg", "Svc").rpc(Rpc.unary[Req, Resp]("Do")).fileDescriptor
      val mid = fd.findMessageTypeByName("Top").findNestedTypeByName("Mid")
      assertTrue(mid.findNestedTypeByName("Leaf") != null) &&
        assertTrue(mid.findFieldByName("leaf").getMessageType.getFullName == "test.pkg.Top.Mid.Leaf")
    },
    test("two nested types sharing a short name don't collide when each is referenced only in its parent's scope") {
      case class TradeA(amount: Int) derives Schema
      case class TradeB(other: Int) derives Schema
      case class Inner(id: Int, t: TradeB) derives Schema
      case class Outer(inner: Inner, a: TradeA) derives Schema
      case class Req(o: Outer) derives Schema, ProtobufCodec
      case class Resp(ok: Boolean) derives Schema, ProtobufCodec

      given ProtobufDeriver     =
        ProtobufDeriver
          .modifier[TradeA](Modifiers.rename("Trade"))
          .modifier[TradeA](Modifiers.nestedIn[Outer])
          .modifier[TradeB](Modifiers.rename("Trade"))
          .modifier[TradeB](Modifiers.nestedIn[Inner])
          .modifier[Inner](Modifiers.nestedIn[Outer])
      given ProtobufCodec[Req]  = Schema[Req].derive(summon[ProtobufDeriver])
      given ProtobufCodec[Resp] = Schema[Resp].derive(summon[ProtobufDeriver])

      val fd    = Service("test.pkg", "Svc").rpc(Rpc.unary[Req, Resp]("Do")).fileDescriptor
      val outer = fd.findMessageTypeByName("Outer")
      val inner = outer.findNestedTypeByName("Inner")
      assertTrue(fd.findMessageTypeByName("Trade") == null) &&
        assertTrue(outer.findFieldByName("a").getMessageType.getFullName == "test.pkg.Outer.Trade") &&
        assertTrue(inner.findFieldByName("t").getMessageType.getFullName == "test.pkg.Outer.Inner.Trade")
    },
    test("two nested types sharing a short name, referenced from outside, resolve via typeId") {
      case class TradeA(amount: Int) derives Schema
      case class TradeB(other: Int) derives Schema
      case class A(a: TradeA) derives Schema
      case class B(b: TradeB) derives Schema
      case class Req(a: A, b: B, trA: TradeA, trB: TradeB) derives Schema, ProtobufCodec
      case class Resp(ok: Boolean) derives Schema, ProtobufCodec

      given ProtobufDeriver     =
        ProtobufDeriver
          .modifier[TradeA](Modifiers.rename("Trade"))
          .modifier[TradeA](Modifiers.nestedIn[A])
          .modifier[TradeB](Modifiers.rename("Trade"))
          .modifier[TradeB](Modifiers.nestedIn[B])
      given ProtobufCodec[Req]  = Schema[Req].derive(summon[ProtobufDeriver])
      given ProtobufCodec[Resp] = Schema[Resp].derive(summon[ProtobufDeriver])

      val req = Service("test.pkg", "Svc").rpc(Rpc.unary[Req, Resp]("Do")).fileDescriptor.findMessageTypeByName("Req")
      assertTrue(req.findFieldByName("tr_a").getMessageType.getFullName == "test.pkg.A.Trade") &&
        assertTrue(req.findFieldByName("tr_b").getMessageType.getFullName == "test.pkg.B.Trade")
    },
    test("Dependency.fileDescriptor nests transitively-pulled children under their parent") {
      case class Inner(v: String) derives Schema
      case class Parent(inner: Inner) derives Schema
      case class Req(p: Parent) derives Schema, ProtobufCodec
      case class Resp(ok: Boolean) derives Schema, ProtobufCodec

      given ProtobufDeriver       = ProtobufDeriver.modifier[Inner](Modifiers.nestedIn[Parent])
      given ProtobufCodec[Parent] = Schema[Parent].derive(summon[ProtobufDeriver])
      given ProtobufCodec[Req]    = Schema[Req].derive(summon[ProtobufDeriver])
      given ProtobufCodec[Resp]   = Schema[Resp].derive(summon[ProtobufDeriver])

      val dep = Dependency("entities", "test.pkg").add[Parent]
      val fd  = Service("test.pkg", "Svc").rpc(Rpc.unary[Req, Resp]("Do")).dependsOn(dep).fileDescriptor

      val parent = depFd(fd, "entities").findMessageTypeByName("Parent")
      assertTrue(parent.findNestedTypeByName("Inner") != null) &&
        assertTrue(depFd(fd, "entities").findMessageTypeByName("Inner") == null)
    },
    test("map<K, V> where V lives nested inside a dep's parent") {
      case class Leaf(id: Int) derives Schema
      case class Parent(x: Int) derives Schema
      case class Req(m: Map[Int, Leaf]) derives Schema, ProtobufCodec
      case class Resp(ok: Boolean) derives Schema, ProtobufCodec

      given ProtobufDeriver       = ProtobufDeriver.modifier[Leaf](Modifiers.nestedIn[Parent])
      given ProtobufCodec[Parent] = Schema[Parent].derive(summon[ProtobufDeriver])
      given ProtobufCodec[Leaf]   = Schema[Leaf].derive(summon[ProtobufDeriver])
      given ProtobufCodec[Req]    = Schema[Req].derive(summon[ProtobufDeriver])
      given ProtobufCodec[Resp]   = Schema[Resp].derive(summon[ProtobufDeriver])

      val dep = Dependency("entities", "test.pkg").add[Parent].add[Leaf]
      val fd  = Service("test.pkg", "Svc").rpc(Rpc.unary[Req, Resp]("Do")).dependsOn(dep).fileDescriptor

      val mapField = fd.findMessageTypeByName("Req").findFieldByName("m")
      assertTrue(mapField.isMapField) &&
        assertTrue(mapField.getMessageType.findFieldByName("value").getMessageType.getFullName == "test.pkg.Parent.Leaf")
    },
    test("sealed trait variants nested under the parent") {
      sealed trait Payment derives Schema
      object Payment {
        case class Coin(amount: Int) extends Payment derives Schema
        case class Cash(bills: Int)  extends Payment derives Schema
      }
      case class Req(p: Payment) derives Schema, ProtobufCodec
      case class Resp(ok: Boolean) derives Schema, ProtobufCodec

      given ProtobufDeriver     =
        ProtobufDeriver
          .modifier[Payment.Coin](Modifiers.nestedIn[Payment])
          .modifier[Payment.Cash](Modifiers.nestedIn[Payment])
      given ProtobufCodec[Req]  = Schema[Req].derive(summon[ProtobufDeriver])
      given ProtobufCodec[Resp] = Schema[Resp].derive(summon[ProtobufDeriver])

      val payment = Service("test.pkg", "Svc").rpc(Rpc.unary[Req, Resp]("Do")).fileDescriptor.findMessageTypeByName("Payment")
      assertTrue(payment.findNestedTypeByName("Coin") != null) &&
        assertTrue(payment.findNestedTypeByName("Cash") != null) &&
        assertTrue(payment.findFieldByName("cash").getMessageType.getFullName == "test.pkg.Payment.Cash")
    },
    test("sealed trait with top-level variants, dep built via fromServices") {
      sealed trait Payment derives Schema
      object Payment {
        case class Coin(amount: Int) extends Payment derives Schema
        case class Cash(bills: Int)  extends Payment derives Schema
      }
      case class Wrap(p: Payment) derives Schema, ProtobufCodec
      case class Req() derives Schema, ProtobufCodec
      case class Resp(w: Wrap) derives Schema, ProtobufCodec

      given ProtobufDeriver     = ProtobufDeriver
      given ProtobufCodec[Req]  = Schema[Req].derive(summon[ProtobufDeriver])
      given ProtobufCodec[Resp] = Schema[Resp].derive(summon[ProtobufDeriver])

      val svc = Service("test.pkg", "Svc").rpc(Rpc.unary[Req, Resp]("Do"))
      val dep = Dependency.fromServices("entities", "test.pkg", "entities", svc)
      val fd  = svc.dependsOn(dep).fileDescriptor

      val gameFd = depFd(fd, "entities")
      assertTrue(gameFd.findMessageTypeByName("Coin") != null) &&
        assertTrue(gameFd.findMessageTypeByName("Payment").findFieldByName("coin").getMessageType.getFullName == "test.pkg.Coin")
    },
    test("structurally identical variants in two sealed traits both resolve after dedup") {
      // `dedupKey` collapses the two `Leaf` variants into one top-level `Leaf`; both parents' oneof
      // refs (with distinct typeIds) must still resolve to that shared FQN.
      sealed trait A derives Schema
      object A { case class Leaf(x: Int, y: Int) extends A derives Schema }
      sealed trait B derives Schema
      object B { case class Leaf(x: Int, y: Int) extends B derives Schema }
      case class Bag(as: List[A], bs: List[B]) derives Schema, ProtobufCodec
      case class Req() derives Schema, ProtobufCodec
      case class Resp(b: Bag) derives Schema, ProtobufCodec

      given ProtobufDeriver     = ProtobufDeriver
      given ProtobufCodec[Req]  = Schema[Req].derive(summon[ProtobufDeriver])
      given ProtobufCodec[Resp] = Schema[Resp].derive(summon[ProtobufDeriver])

      val svc = Service("test.pkg", "Svc").rpc(Rpc.unary[Req, Resp]("Do"))
      val dep = Dependency.fromServices("entities", "test.pkg", "entities", svc)
      svc.dependsOn(dep).fileDescriptor
      assertCompletes
    },
    test("service's own type survives a short-name collision with an unrelated sub-dep type") {
      case class Item(v: String) derives Schema, ProtobufCodec
      case class DepItem(other: Int) derives Schema, ProtobufCodec
      case class Req(item: Item) derives Schema, ProtobufCodec
      case class Resp(ok: Boolean) derives Schema, ProtobufCodec

      // Rename `DepItem` to `Item` in the dep so short names collide but typeIds stay distinct.
      given ProtobufDeriver        = ProtobufDeriver.modifier[DepItem](Modifiers.rename("Item"))
      given ProtobufCodec[DepItem] = Schema[DepItem].derive(summon[ProtobufDeriver])

      val dep = Dependency("entities", "test.pkg").add[DepItem]
      val fd  = Service("test.pkg", "Svc").rpc(Rpc.unary[Req, Resp]("Do")).dependsOn(dep).fileDescriptor

      val item = fd.findMessageTypeByName("Item")
      assertTrue(item != null) &&
        assertTrue(item.findFieldByName("v") != null) &&
        assertTrue(fd.findMessageTypeByName("Req").findFieldByName("item").getMessageType.getFullName == "test.pkg.Item")
    },
    test("a nested variant survives when a sub-dep has an unrelated same-named top-level type") {
      case class SharedName(dataId: Int, amount: Int) derives Schema, ProtobufCodec

      sealed trait Payment derives Schema
      object Payment {
        case class Coin(amount: Int)                    extends Payment derives Schema
        case class SharedName(dataId: Int, amount: Int) extends Payment derives Schema
      }
      case class Wrap(p: Payment) derives Schema, ProtobufCodec
      case class Req(w: Wrap) derives Schema, ProtobufCodec
      case class Resp(ok: Boolean) derives Schema, ProtobufCodec

      given ProtobufDeriver     =
        ProtobufDeriver
          .modifier[Payment.Coin](Modifiers.nestedIn[Payment])
          .modifier[Payment.SharedName](Modifiers.nestedIn[Payment])
      given ProtobufCodec[Req]  = Schema[Req].derive(summon[ProtobufDeriver])
      given ProtobufCodec[Resp] = Schema[Resp].derive(summon[ProtobufDeriver])

      val subDep = Dependency("common", "test.common", "common").add[SharedName]
      val dep    = Dependency("game", "test.game", "game").add[Wrap].dependsOn(subDep)
      val fd     = Service("test.game", "Svc").rpc(Rpc.unary[Req, Resp]("Do")).dependsOn(dep).fileDescriptor

      val payment = depFd(fd, "game").findMessageTypeByName("Payment")
      assertTrue(payment.findNestedTypeByName("SharedName") != null) &&
        assertTrue(payment.findFieldByName("shared_name").getMessageType.getFullName == "test.game.Payment.SharedName")
    },
    test("a type in a dep references a type in a sub-dep with a different package") {
      case class Leaf(id: Int) derives Schema, ProtobufCodec
      case class Parent(leaf: Leaf) derives Schema, ProtobufCodec
      case class Req(p: Parent) derives Schema, ProtobufCodec
      case class Resp(ok: Boolean) derives Schema, ProtobufCodec

      val subDep = Dependency("common", "test.common", "common").add[Leaf]
      val dep    = Dependency("game", "test.game", "game").add[Parent].dependsOn(subDep)
      val fd     = Service("test.game", "Svc").rpc(Rpc.unary[Req, Resp]("Do")).dependsOn(dep).fileDescriptor

      val parent = depFd(fd, "game").findMessageTypeByName("Parent")
      assertTrue(parent.findFieldByName("leaf").getMessageType.getFullName == "test.common.Leaf")
    },
    test("relocateNestedIn is idempotent: a child added both directly and via its parent isn't duplicated") {
      case class Inner(v: String) derives Schema
      case class Parent(inner: Inner) derives Schema
      case class Req(p: Parent, i: Inner) derives Schema, ProtobufCodec
      case class Resp(ok: Boolean) derives Schema, ProtobufCodec

      given ProtobufDeriver       = ProtobufDeriver.modifier[Inner](Modifiers.nestedIn[Parent])
      given ProtobufCodec[Parent] = Schema[Parent].derive(summon[ProtobufDeriver])
      given ProtobufCodec[Inner]  = Schema[Inner].derive(summon[ProtobufDeriver])
      given ProtobufCodec[Req]    = Schema[Req].derive(summon[ProtobufDeriver])
      given ProtobufCodec[Resp]   = Schema[Resp].derive(summon[ProtobufDeriver])

      val dep = Dependency("entities", "test.pkg").add[Parent].add[Inner]
      val fd  = Service("test.pkg", "Svc").rpc(Rpc.unary[Req, Resp]("Do")).dependsOn(dep).fileDescriptor

      val parent = depFd(fd, "entities").findMessageTypeByName("Parent")
      assertTrue(parent.getNestedTypes.asScala.count(_.getName == "Inner") == 1)
    }
  )
}
