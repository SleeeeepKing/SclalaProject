package undirected

object Test3 {
  val vertices: Set[Int] = Set(1,2,3,4,5,6,7)
  val edges: Set[Edge[Int]] = Set(Edge(1,4),Edge(1,2),Edge(2,4),Edge(2,3),Edge(2,5),Edge(4,6),Edge(6,7),Edge(5,7))
  val edges2: Set[Edge[Int]] = Set(Edge(1,4),Edge(1,2),Edge(2,5),Edge(2,3))
  val edges3: Set[Edge[Int]] = Set()
  val g = new SimpleGraphDefaultImpl[Int](vertices,edges)
  val g2 = new SimpleGraphDefaultImpl[Int](vertices,edges2)
  def main(args:Array[String]) {

//    println(g.isAcyclic)

//    println(g.Hascycle(g.vertices))
//    println(g)
//    println(g2.hasPath(g2.vertices.head,g2.vertices.last) )
//    println(g2.vertices.head,g2.vertices.last )
        println(g.minimumSpanningTree(g.getMap(g.edges.toList.sortBy(x=>x._1),List(4,2,5,6,7,3,8,9))))
//    g.withoutEdge
//    println(g)
    Set.empty(edges)
//    println(g.withoutEdge)
  }
}
