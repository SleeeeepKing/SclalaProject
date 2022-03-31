package undirected

class test extends SimpleGraph[Int] {
  /** The set of all vertices of the graph */
  override val vertices: Set[Int] = Set(1,2,3,4,5,6,7)
  /** The set of all    edges of the graph */
  override val edges: Set[Edge[Int]] = Set(Edge(1,4),Edge(1,2),Edge(2,4),Edge(2,3),Edge(2,5),Edge(4,6),Edge(6,7),Edge(5,7))

  /** The set of all vertices adjacent to input vertex
   * 与输入顶点相邻的所有顶点的集合
   *
   * @param v vertex
   * @return [[None]] if `v` is not an actual vertex, the set of all neighbors of `v` otherwise (may be empty)
   */
  override def neighborsOf(v: Int): Option[Set[Int]] = ???

  /** Add vertex to graph
   *
   * @param v new vertex
   * @return the graph with new vertex `v`
   *         if `v` is an actual vertex of graph, return input graph
   */
  override def +(v: Int): SimpleGraph[Int] = ???

  /** Remove vertex from graph
   *
   * @param v new vertex
   * @return the graph without vertex `v`
   *         if `v` is not an actual vertex of graph, return input graph
   */
  override def -(v: Int): SimpleGraph[Int] = ???

  /** Add edge to graph (also add edge ends as new vertices if necessary)
   *
   * @param e new edge
   * @return the graph with new edge `e`
   *         if `e` is an actual edge of graph, return input graph
   */
  override def +|(e: Edge[Int]): SimpleGraph[Int] = ???

  /** Remove edge from graph (does NOT remove ends)
   *
   * @param e new edge
   * @return the graph without edge `e`
   *         if `e` is not an actual edge of graph, return input graph
   */
  override def -|(e: Edge[Int]): SimpleGraph[Int] = ???

  /** Remove all edges from graph but keep same vertices
   *
   * @return graph with same vertices without any edge
   */
  override def withoutEdge: SimpleGraph[Int] = {
    SimpleGraph(vertices, Set.empty[Edge[V]])
  }

  /** Add all possible edge with same vertices
   *
   * @return graph with same vertices and all possible edges
   */
  override def withAllEdges: SimpleGraph[Int] = ???
}
