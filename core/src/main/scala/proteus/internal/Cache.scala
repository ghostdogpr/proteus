package proteus
package internal

/**
  * A cache for storing computed sizes and values during the first pass of encoding.
  */
final private[proteus] class Cache {
  private var sizes: Array[Int] = new Array[Int](32)
  private var currentIndex: Int = 0

  private var values: Array[AnyRef] = new Array[AnyRef](32)
  private var valueIndex: Int       = 0

  def recordSize(size: Int): Unit = {
    if (currentIndex >= sizes.length) {
      sizes = java.util.Arrays.copyOf(sizes, sizes.length * 2)
    }
    sizes(currentIndex) = size
    currentIndex += 1
  }

  def recordValue(value: AnyRef): Unit = {
    if (valueIndex >= values.length) {
      values = java.util.Arrays.copyOf(values, values.length * 2)
    }
    values(valueIndex) = value
    valueIndex += 1
  }

  def reserveSize(): Int = {
    if (currentIndex >= sizes.length) {
      sizes = java.util.Arrays.copyOf(sizes, sizes.length * 2)
    }
    val index = currentIndex
    currentIndex += 1
    index
  }

  def fillSize(index: Int, size: Int): Unit =
    sizes(index) = size

  def nextSize(): Int = {
    val size = sizes(currentIndex)
    currentIndex += 1
    size
  }

  def nextValue(): Object = {
    val value = values(valueIndex)
    valueIndex += 1
    value
  }

  def reset(): Unit = {
    currentIndex = 0
    valueIndex = 0
  }
}
