package proteus
package internal

/**
  * A cache for storing computed sizes during the first pass of encoding.
  * Sizes use a FIFO queue (both passes traverse in same order).
  */
final private[proteus] class SizeCache {
  private var sizes: Array[Int] = new Array[Int](32)
  private var currentIndex: Int = 0

  /**
    * Records a size during the size computation pass.
    */
  def record(size: Int): Unit = {
    if (currentIndex >= sizes.length) {
      sizes = java.util.Arrays.copyOf(sizes, sizes.length * 2)
    }
    sizes(currentIndex) = size
    currentIndex += 1
  }

  /**
    * Reserves a slot for a size and returns the index.
    */
  def reserve(): Int = {
    if (currentIndex >= sizes.length) {
      sizes = java.util.Arrays.copyOf(sizes, sizes.length * 2)
    }
    val index = currentIndex
    currentIndex += 1
    index
  }

  /**
    * Fills a reserved slot with the actual size.
    */
  def fill(index: Int, size: Int): Unit =
    sizes(index) = size

  /**
    * Retrieves the next size during the write pass (FIFO order).
    */
  def next(): Int = {
    val size = sizes(currentIndex)
    currentIndex += 1
    size
  }

  /**
    * Resets the current index between passes. Called after size computation, before writing.
    */
  def reset(): Unit =
    currentIndex = 0
}
