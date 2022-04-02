package undirected

import jdk.nashorn.internal.objects.NativeDebug.map

import scala.collection.immutable.Nil.++
import scala.math.Numeric.DoubleIsFractional
import scala.collection.mutable
import scala.math.Ordering.Double.{IeeeOrdering, TotalOrdering}

/** Trait for an undirected and ''simple'' graph, that is without loop nor parallel edges
 *
 * @tparam V type for vertices
 */
trait SimpleGraph[V] {
  /* QUERY METHODS */

  /** The set of all vertices of the graph */
  val vertices: Set[V] //vertices 所有的顶点


  /** The set of all    edges of the graph */
  val edges: Set[Edge[V]] //edges 所有的边


  val map = mutable.Map.empty[Edge[V], Double]

  def getMap(arc: Seq[Edge[V]], weight: List[Double]): Map[Edge[V], Double] = {
    if (arc.nonEmpty) {
      map(arc.head) = weight.head
      (map ++ getMap(arc.tail, weight.tail)).toMap
    }
    else
      Map.empty[Edge[V], Double]

  }


  /** The set of all vertices adjacent to input vertex
   * 与输入顶点相邻的所有顶点的集合
   *
   * @param v vertex
   * @return [[None]] if `v` is not an actual vertex, the set of all neighbors of `v` otherwise (may be empty)
   */
  def neighborsOf(v: V): Option[Set[V]] //与输入顶点相邻的所有顶点的集合

  /** The number of adjacent vertices to input vertex
   * 输入顶点的相邻顶点数
   *
   * @param v vertex
   * @return [[None]] if `v` is not an actual vertex, the degree of `v` otherwise
   */
  def degreeOf(v: V): Option[Int] = neighborsOf(v) map {
    _.size
  } //输入顶点的相邻顶点数

  /** Checks if there exists a path between two vertices
   *
   * @param v1 one end of path to search
   * @param v2 other end of path to search
   * @return `true` if `v1` and `v2` are equal or if a path exists between `v1` and `v2`, `false` otherwise
   */

  def hasPath(v1: V, v2: V): Boolean = {
    if (degreeOf(v1).get == 0 || degreeOf(v2).get == 0) return false;
    if (v1 == v2 || neighborsOf(v1).get.contains(v2)) return true; //condition d'arret true
    return (neighborsOf(v1).get.map(v => hasPathAux(v, v2, Set(v1))).contains(true))
    //return false;
  }

  def hasPathAux(v1: V, v2: V, v3: Set[V]): Boolean = {
    if (v1 == v2 || neighborsOf(v1).get.contains(v2)) return true;
    val s = neighborsOf(v1).get.--(v3)
    if (s.nonEmpty) {
      return s.map(x => hasPathAux(x, v2, v3.+(v1))).contains(true)
    }

    return false;
  }

  /** Checks if graph is connected */
  lazy val isConnected: Boolean = !vertices.map(v1 => vertices.-(v1).map(v2 => hasPath(v1, v2))).flatten.contains(false);


  /** Checks if graph is acyclic */
  lazy val isAcyclic: Boolean = if (edges.size == 1) {
    true
      } else !vertices.map(v1 => hasPath(v1, v1)).contains(true)


  /** Checks if graph is a tree */
  lazy val isTree: Boolean = isConnected && isAcyclic

  /* VERTEX OPERATIONS */

  /** Add vertex to graph
   *
   * @param v new vertex
   * @return the graph with new vertex `v`
   *         if `v` is an actual vertex of graph, return input graph
   */
  def +(v: V): SimpleGraph[V]

  /** Remove vertex from graph
   *
   * @param v new vertex
   * @return the graph without vertex `v`
   *         if `v` is not an actual vertex of graph, return input graph
   */
  def -(v: V): SimpleGraph[V]

  /* EDGE OPERATIONS */

  /** Add edge to graph (also add edge ends as new vertices if necessary)
   *
   * @param e new edge
   * @return the graph with new edge `e`
   *         if `e` is an actual edge of graph, return input graph
   */
  def +|(e: Edge[V]): SimpleGraph[V]

  /** Remove edge from graph (does NOT remove ends)
   *
   * @param e new edge
   * @return the graph without edge `e`
   *         if `e` is not an actual edge of graph, return input graph
   */
  def -|(e: Edge[V]): SimpleGraph[V]

  /** Remove all edges from graph but keep same vertices
   *
   * @return graph with same vertices without any edge
   */
  def withoutEdge: SimpleGraph[V]

  /** Add all possible edge with same vertices
   *
   * @return graph with same vertices and all possible edges
   */
  def withAllEdges: SimpleGraph[V]

  /* VALUATED GRAPH METHODS */

  /** Total value of the graph
   *
   * @param valuation valuation used
   * @return total value of the graph, i.e. sum of values of all edges
   */
  def value(valuation: Map[Edge[V], Double]): Double = (edges map {
    valuation(_)
  }).sum

  /** Minimum spanning tree
   *
   * @param valuation valuation used
   * @return a spanning tree whose value is minimal
   */


  def mstAide(g: SimpleGraph[V], valuation: Map[Edge[V], Double]): SimpleGraph[V] = {
    val minE = valuation.minBy(_._2)(DoubleIsFractional)._1
    val g2 = g.+|(minE)
    val valuation2 = valuation - (minE)
    println(g2)
    if (!g2.isAcyclic) {
      val g3 = g2.-|(minE)
      println("成环,删除 " + minE)
      if (!g3.isConnected)
        mstAide(g3, valuation2)
      else
        g3
    } else {
      if (!g2.isConnected)
        mstAide(g2, valuation2)
      else
        g2
    }
  }

  def minimumSpanningTree(valuation: Map[Edge[V], Double]): SimpleGraph[V] = {
    val g = this.withoutEdge
    mstAide(g, valuation)
  }

  /* COLORING METHODS */
  def calVexDegree(g: SimpleGraph[V]): Seq[V] = {
    //    val v1 = g.vertices
    ???


  }

  /** Sequence of vertices sorted by decreasing degree */
  lazy val sortedVertices: Seq[V] = {
    //    val s1 = Seq[this.vertices]
    ???
  }

  /** Proper coloring using greedy algorithm (a.k.a WELSH-POWELL) */
  lazy val greedyColoring: Map[V, Int] = {
    ???
  }

  /** Proper coloring using DSATUR algorithm */
  lazy val coloringDSATUR: Map[V, Int] = {
    ???
  }

  /* toString-LIKE METHODS */

  /** @inheritdoc */
  override lazy val toString: String = s"({${
    vertices mkString ", "
  }}, {${
    edges mkString ", "
  }})"

  /** Graph representation in DOT language */
  lazy val toDOTString: String = {
    "strict graph {\n" +
      "    // Edges\n" +
      (edges foldLeft "    ") {
        _ + _.toDOTString + "\n    "
      } + "\n" +
      "    // Vertices\n" +
      vertices.mkString("    ", "\n    ", "\n") +
      "  }\n"
  }

}
