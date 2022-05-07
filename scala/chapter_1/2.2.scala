// check if a Array[T] is sorted

object Test extends App {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def helper(head: A, tail: Array[A]): Boolean = {
      if (tail.isEmpty) true
      else if (ordered(head, tail.head)) helper(tail.head, tail.tail)
      else false
    }

    if (as.nonEmpty) helper(as.head, as.tail)
    else true
  }

  def ordered(a: Int, b: Int) = b >= a

  assert(isSorted(Array(1, 2, 3), ordered) == true)
  assert(isSorted(Array(1), ordered) == true)
  assert(isSorted(Array[Int](), ordered) == true)
  assert(isSorted(Array(1, 4, 3), ordered) == false)
  assert(isSorted(Array(5, 4, 3), ordered) == false)
  assert(isSorted(Array(1, 1, 2, 3, 3), ordered) == true)
  assert(isSorted(Array(3, 3, 1), ordered) == false)

}
