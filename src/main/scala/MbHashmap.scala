package mbhashmap

import com.sun.org.apache.bcel.internal.generic.LoadClass

object MbHashmap {
  def empty[@miniboxed K, @miniboxed V](initialCapacity: Int = 16, loadFactor: Float = 0.75f) =
    new MbHashmap[K, V](initialCapacity, loadFactor)

  class MbHashmap[@miniboxed K, @miniboxed V](var _capacity: Int, val _loadFactor: Float) {
    var _buckets = new Array[Entry[K, V]](_capacity)
    var _size = 0
    
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
      val i = computeIndex(key)
      var entry = _buckets(i)

      if (entry == null) {
        _size += 1
        _buckets(i) = new Entry(key, value)
      } else {
        var lastEntry = entry
        
        while (entry != null) {
          if (entry.key == key) {
            entry.value = value
            return
          }
          lastEntry = entry
          entry = entry.next
        }

        _size += 1
        lastEntry.next = new Entry(key, value)
      }
      
      rehashIfNeeded
    }

    private def rehashIfNeeded = {
      if (_size > _capacity * _loadFactor ) {
        // perform rehashing
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

  }
}