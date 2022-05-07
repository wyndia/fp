object Test extends App {
  println(List.dropWhile(List(1, 2, 3))(_ < 2))
  println(List.dropWhile(List(2, 1, 3))(_ < 2))
  println(List.dropWhile(List(2, 2, 2))(_ < 2))
  println(List.dropWhile(List[Int]())(_ < 2))
}

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(list: List[Int]): Int =
    list match {
      case Nil              => 0
      case Cons(head, tail) => head + sum(tail)
    }

  def product(list: List[Double]): Double =
    list match {
      case Nil              => 0
      case Cons(head, tail) => head * product(tail)
    }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, List(as.tail: _*))
  }

  // 3.2 Add tail() method to List which takes O(1) time.
  def tail[A](list: List[A]): List[A] =
    list match {
      case Cons(_, tail) => tail
      case Nil           => throw new Exception("Empty List has no tail.")
    }

  // 3.3 Add setHead() method to List which takes O(1) time.
  def setHead[A](head: A, list: List[A]): List[A] =
    list match {
      case Cons(_, tail) => Cons(head, tail)
      case Nil           => Nil
    }

  // 3.4 add dropWhile with signature of:
  // def dropWhile[A](l: List[A], f: A => Boolean): List[A]
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) => if (f(h)) dropWhile(t)(f) else Cons(h, t)
      case Nil        => Nil
    }

}
