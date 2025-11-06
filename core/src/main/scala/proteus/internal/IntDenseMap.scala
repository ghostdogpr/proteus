package proteus
package internal

import scala.collection.immutable.HashMap

final private[proteus] class IntDenseMap[V] private (
  private val array: Array[Any],
  private val map: HashMap[Int, V],
  val arraySize: Int,
  val indexes: List[Int]
) {
  def apply(id: Int): V =
    if (id >= 0 && id < arraySize) {
      val value = array(id)
      if (value != null) value.asInstanceOf[V] else map.getOrElse(id, null.asInstanceOf[V])
    } else map.getOrElse(id, null.asInstanceOf[V])
}

object IntDenseMap {
  def apply[V](fields: HashMap[Int, V]): IntDenseMap[V] = {
    // Find the highest field ID that's less than 1000 (typical protobuf field numbers are small)
    val maxId     = fields.keysIterator.filter(_ < 1000).maxOption.getOrElse(0)
    val arraySize = maxId + 1
    val array     = new Array[Any](arraySize)
    val builder   = List.newBuilder[Int]

    // Fill array with fields that have small IDs
    fields.foreach { case (id, value) =>
      if (id < arraySize) array(id) = value
      builder += id
    }

    // Keep only fields with large IDs in the HashMap
    val largeFields = fields.filter { case (id, _) => id >= arraySize }

    new IntDenseMap(array, largeFields, arraySize, builder.result())
  }

  def from[V](pairs: Iterable[(Int, V)]): IntDenseMap[V] =
    apply(HashMap.from(pairs))
}
