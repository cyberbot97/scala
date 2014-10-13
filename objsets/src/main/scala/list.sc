
object list {

  trait List[T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
  }
  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty = false
  }
  class Nil[T] extends List[T] {
    def isEmpty = true
    def head = throw new NoSuchElementException()
    def tail = throw new NoSuchElementException()
  }


  def getNth[T](n: Int, list: List[T]): T = {
    if (list.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) list.head
    else getNth(n-1, list.tail)
  }

  val testlist = new Cons(1, new Cons(2, new Cons(3, new Nil)))

  getNth(1, testlist)
}
