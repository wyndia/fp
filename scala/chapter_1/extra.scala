// Re-define List! with sum, product and apply()

object Test extends App {

  sealed trait List[+A]
  // Have to specify an existing Type (Nothing) because you can not write `object Nil[A]`
  // case object Nil extends List[Nothing]
  // You can also use this...
  case class Nil[A]() extends List[A]
  // Here the A in `extends List[A]` is reffering to A in `Cons[A]` (called type parameter)
  case class Cons[A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(list: List[Int]): Int =
      list match {
        // If you define trait as List[A] then this will fail
        case Nil()            => 0
        case Cons(head, tail) => head + sum(tail)
      }

    def product(list: List[Double]): Double =
      list match {
        case Nil() => 0
        // TODO quit when 0
        case Cons(head, tail) => head * product(tail)
      }

    def apply[A](as: A*): List[A] = {
      // Why can't we use case match here?
      if (as.isEmpty) Nil()
      else Cons(as.head, List(as.tail: _*))
    }
  }

  println(List(1, 2, 3), List.product(List(1, 2, 3)))
  println(List[Int](), List.sum(List[Int]()))
  println(List(1), List.sum(List(1)))

  println(List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  })

  println("List[Int]() == List[String](): " + List[Int]() == List[String]())
  println("↑↑↑↑ should be false if not using case object Nil")

}
