package mbhashmap

trait Buildable[@miniboxed T, Container[_]] extends Iterable[T] {
  def builder[@miniboxed K]: Builder[K, Container]

  def map[@miniboxed U](f: T => U) = {
    val bd = builder[U]
    val it = iterator
    while (it.hasNext) bd.append(f(it.next))
    bd.finalise
  }

  def filter(f: T => Boolean) = {
    val bd = builder[T]
    val it = iterator
    while (it.hasNext) {
      val elem = it.next
      if (f(elem)) {
        bd.append(elem)
      }
    }
    bd.finalise
  }
}

trait Buildable2[@miniboxed A, @miniboxed B, Container[_, _]] extends Iterable[(A, B)] {
  def builder[@miniboxed U, @miniboxed V]: Builder2[U, V, Container]
  
  def map[@miniboxed U, @miniboxed V](f: ((A, B)) => (U, V)) = {
    val bd = builder[U, V]
    val it = iterator
    while (it.hasNext) bd.append(f(it.next))
    bd.finalise
  }

  def filter(f: ((A, B)) => Boolean) = {
    val bd = builder[A, B]
    val it = iterator
    while (it.hasNext) {
      val elem = it.next
      if (f(elem)) {
        bd.append(elem)
      }
    }
    bd.finalise
  }
}

abstract class Builder[@miniboxed T, Container[_]] {
  def append(x: T)
  def finalise: Container[T]
}

abstract class Builder2[@miniboxed A, @miniboxed B, Container[_, _]] {
  def append(x: (A, B))
  def finalise: Container[A, B]
}