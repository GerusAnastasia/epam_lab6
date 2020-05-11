import scala.annotation.tailrec

object Main {

  @tailrec
  def foldl[A, B](f: (B, A) => B, a: B, c: List[A]): B = c match {
    case Nil => a
    case r :: rest => foldl(f, f(a, r), rest)
  }

  def size[A](in: List[A]): Int = in match {
    case _ :: rest => size(rest) + 1
    case Nil => 0
  }

  def mean[A, B](identity: A, add: (A, A) => A, div: (A, Int) => B)(in: List[A]): B =
    div(foldl[A, A](add, identity, in),size(in))

  def main(args: Array[String]): Unit = {
    val int_mean: List[Int] => Double = mean[Int, Double](0, (x, y) => x + y, (x, y) => x.toDouble / y)
    println(int_mean(List(1, 2, 3, 4, 5, 6, 20)))
  }
}
