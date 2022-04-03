package undirected

import scala.math.Numeric.DoubleIsFractional

object Test2 {
  // Main method
  val vertices: Set[Int] = Set(1, 2, 3, 4, 5, 6, 7)
  val vertices2: Set[Int] = Set(1)
  val edges: Set[Edge[Int]] = Set(Edge(1, 4), Edge(1, 2), Edge(2, 4), Edge(2, 3), Edge(2, 5), Edge(4, 6), Edge(6, 7), Edge(5, 7))
  val edges2: Set[Edge[Int]] = Set(Edge(1, 4), Edge(1, 2), Edge(2, 5), Edge(2, 3))
  val edges3: Set[Edge[Int]] = Set()
  val g = new SimpleGraphDefaultImpl[Int](vertices, edges)
  val g2 = new SimpleGraphDefaultImpl[Int](vertices, edges2)
  val weight = g.getMap(g.edges.toList.sortBy(x => x._1), List(4, 2, 5, 6, 7, 3, 8, 9))


  def main(args: Array[String]): Unit = {
//    println(g2.vertices.max( _: Ordering[Int]))
//    println(g2.calVexDegree(g2))
//println(vertices2.concat(vertices3))
//    println(vertices4)
    println(g.neighborsOf(1).get.--(Set(1)))
  }
}
