package common
import scala.reflect.ClassTag

object ArrayUtils {
  implicit class ImplicitArray2D[A](arr: Array[Array[A]]) {
    def map2D[B](f: A => B)(implicit ct: ClassTag[B]): Array[Array[B]] = arr.map(_.map(f))

    def count2D(f: A => Boolean): Int = arr.map(_.count(f)).sum

    def zipWithIndex2D: Array[Array[(A, Int, Int)]] =
      arr.zipWithIndex.map {
        case (ai, i) => ai.zipWithIndex.map {
          case (a, j) => (a, i, j)
        }
      }

    def lift2D(i: Int, j: Int): Option[A] = arr.lift(i).flatMap(_.lift(j))

    def sameElements2D(other: Array[Array[A]]): Boolean =
      arr.zipWithIndex.forall {
        case (ai, i) => ai sameElements other(i)
      }
  }
}
