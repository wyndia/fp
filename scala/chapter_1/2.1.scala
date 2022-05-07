// def fib recursively and use @tailrec

object Test extends App {

  def fib(n: Int): Int = {
    require(n > 0)

    @annotation.tailrec
    def helper(prev: Int, curr: Int, cnt: Int): Int = {
      if (cnt < n) helper(curr, prev + curr, cnt + 1)
      else curr
    }

    if (n < 3) n - 1
    else helper(0, 1, 2)
  }

  println(fib(1))
  println(fib(2))
  println(fib(3))
  println(fib(4))
  println(fib(5))
  println(fib(6))
  println(fib(7))

}
