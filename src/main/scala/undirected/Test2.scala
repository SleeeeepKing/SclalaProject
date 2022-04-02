package undirected

import scala.math.Numeric.DoubleIsFractional

object Test2 {
  // Main method
  val vertices: Set[Int] = Set(1, 2, 3, 4, 5, 6, 7,0)
  val vertices2: Seq[Int] = Seq(1, 2, 3, 4, 5, 6, 7,0)
  val vertices3: Seq[Int] = Seq(9,99)
  val edges: Set[Edge[Int]] = Set(Edge(1, 4), Edge(1, 2), Edge(2, 4), Edge(2, 3), Edge(2, 5), Edge(4, 6), Edge(6, 7), Edge(5, 7))
  val g2 = new SimpleGraphDefaultImpl[Int](vertices, edges)

  def main(args: Array[String]): Unit = {
//    println(g2.vertices.max( _: Ordering[Int]))
    println(g2.calVexDegree(g2))
//println(vertices2.concat(vertices3))
//    println(vertices4)
  }
}
