package mbhashmap

import com.sun.org.apache.bcel.internal.generic.LoadClass

object MbHashmap {
  def empty[@miniboxed K, @miniboxed V](initialCapacity: Int = 16, loadFactor: Float = 0.75f) =
    new MbHashmap[K, V](initialCapacity, loadFactor)

  class MbHashmap[@miniboxed K, @miniboxed V](private var _capacity: Int, private val _loadFactor: Float) {
    private var _buckets = new Array[Entry[K, V]](_capacity)
    private var _size = 0

    def size = _size

    def apply(key: K): Option[V] = {
      var entry = _buckets(computeIndex(key))

      while (entry != null) {
        if (entry.key == key) {
          return Some(entry.value)
        }
        entry = entry.next
      }

      None
    }

    def update(key: K, value: V): Unit = {
      if (updateEntry(key, value)) {
        _size += 1
        rehashIfNeeded
      }
    }

    private def updateEntry(key: K, value: V): Boolean = {
      val i = computeIndex(key)
      var entry = _buckets(i)

      if (entry == null) {
        _buckets(i) = new Entry(key, value)
      } else {
        var lastEntry = entry

        while (entry != null) {
          if (entry.key == key) {
            entry.value = value
            return false
          }
          lastEntry = entry
          entry = entry.next
        }

        lastEntry.next = new Entry(key, value)
      }
      true
    }

    private def rehashIfNeeded = {
      if (_size > _capacity * _loadFactor) {
        val oldCapacity = _capacity
        val oldBuckets = _buckets

        _capacity = _capacity * 2
        _buckets = new Array[Entry[K, V]](_capacity)

        var i = 0
        while (i < oldCapacity) {
          var entry = oldBuckets(i)
          while (entry != null) {
            updateEntry(entry.key, entry.value)
            entry = entry.next
          }
          i += 1
        }

        /*
        println("Rehashing")
        println("\tSize : " + _size)
        println("\tOld capacity : " + oldCapacity)
        println("\tNew capacity : " + _capacity)
        */
      }
    }

    private def computeIndex(k: K) = k.hashCode() % _capacity
  }

  class Entry[@miniboxed K, @miniboxed V](val key: K, var value: V) {
    var next: Entry[K, V] = null
  }
}

object Main {
  def main(args: Array[String]) = {
    val hm = MbHashmap.empty[Int, Int]()
    for (i <- 0 to 50) {
      hm(i) = i
    }
    for (i <- 0 to 50) {
      hm(i).foreach { x => assert(x == i) }
    }
  }
}