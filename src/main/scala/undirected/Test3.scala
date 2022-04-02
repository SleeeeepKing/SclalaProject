package undirected

object Test3 {
  val vertices: Set[Int] = Set(1, 2, 3, 4, 5, 6, 7)
  val vertices2: Set[Int] = Set(1)
  val edges: Set[Edge[Int]] = Set(Edge(1, 4), Edge(1, 2), Edge(2, 4), Edge(2, 3), Edge(2, 5), Edge(4, 6), Edge(6, 7), Edge(5, 7))
  val edges2: Set[Edge[Int]] = Set(Edge(1, 4), Edge(1, 2), Edge(2, 5), Edge(2, 3))
  val edges3: Set[Edge[Int]] = Set()
  val g = new SimpleGraphDefaultImpl[Int](vertices, edges)
  val g2 = new SimpleGraphDefaultImpl[Int](vertices, edges2)

  def main(args: Array[String]) {

    /** hasPath */
    println("*****Fonction hasPath*****")
    println(g.hasPath(1,4))
    /** isAcyclic */
    println("*****Fonction isAcyclic*****")
    println(g.isAcyclic)
    /** mst */
    println("*****Fonction mst*****")
    println("Before:  "+g)
    println("After :  "+g.minimumSpanningTree(g.getMap(g.edges.toList.sortBy(x => x._1), List(4, 2, 5, 6, 7, 3, 8, 9))))
    /** color sorted */
    println("*****Fonction sortedVertices*****")
    println("Before:  "+g.vertices)
    println("After :  "+g.sortedVertices)


    //    println(g.isAcyclic)
    /* val nv1 = g2.neighborsOf(1)
     //    println(nv1)
     val e = g2.edges
     val l1 = scala.collection.mutable.Map[Int,Int]((1 , 2), (3 , 4))
     val ll = Map[Int,Int]((11 , 22), (33 , 44))
     val l2 = l1 +=((5,6))
      l1 +=((5,6))
     val l3 = l2 ++ ll*/
    //    g.sortedVertices

    //    println(vertices2.-(1) != Set())
    //    println(l3)
    //    println(l1)
    //    println(l2)
    //    val l3 = l2
    //    println(l3)
    //        g.calVexDegree(g)
    //    println(g.+(99))
    //        println(g.withoutVertices)
    vertices.size
    //    println(g.neighborsOf(1).get.size)

    //    println(g2.edges.filter(_._1 == 1))
    //    println(g2.edges)
    //    println(g.Hascycle(g.vertices))
    //    println(g)
    //    println(g2.hasPath(g2.vertices.head,g2.vertices.last) )
    //    println(g2.vertices.head,g2.vertices.last )
    //    g.withoutEdge
    //    println(g)
    //    Set.empty(edges)
    //    println(g.withoutEdge)
  }
}
