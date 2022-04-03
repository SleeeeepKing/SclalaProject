package undirected

import jdk.nashorn.internal.objects.NativeDebug.map
import jdk.nashorn.internal.runtime.ScriptObject.{GETPROTO, NO_SUCH_METHOD_NAME, getCount}

import scala.:+
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

  def hasPath(v1 : V, v2 : V) : Boolean = {
    if(degreeOf(v1).get== 0 || degreeOf(v2).get == 0) return false;
    if(v1==v2 || neighborsOf(v1).get.contains(v2)) return true;
    return (neighborsOf(v1).get.map(v=>hasPathAide(v,v2,Set(v1))).contains(true))
    //false;
  }


  def hasPathAide(v1 : V, v2 : V,v3:Set[V]) : Boolean ={
    if(v1==v2 || neighborsOf(v1).get.contains(v2)) return true;
    val s=neighborsOf(v1).get.--(v3)
    if(s.nonEmpty){return s.map(x =>hasPathAide(x,v2,v3.+(v1))).contains(true)}
    return false;
  }


  /*  def hasPathAide(v1:V,v2:V,edge: Set[Edge[V]]): Boolean = {
      val nv1 = neighborsOf(v1)
      if (nv1 == v2) {
        return true
      }else{
        edge.find(_._2 == nv1)
      }
      false
    }

    def hasPathAide2(v: V, edge: Set[Edge[V]]): Boolean = {
      false
    }*/


  /** Checks if graph is connected */
  lazy val isConnected: Boolean = !vertices.map(v1 => vertices.-(v1).map(v2 => hasPath(v1, v2))).flatten.contains(false);


  /** Checks if graph is acyclic */
  lazy val isAcyclic: Boolean = this.NoCycle(this)


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

  def NoCycle(g: SimpleGraph[V]): Boolean = {

    if (g.edges.isEmpty)
      true
    else {
      val e1 = g.edges
      val e2 = g.edges.drop(1)
      checkcycle(e1.head, e2)
    }
  }

  def checkcycle(e1: Edge[V], e2: Set[Edge[V]]): Boolean = {

    if (e2 != Set()) {
      e2.foreach(x =>
        if (x._1 == e1._1) {
          //          println("进入foreach的if")
          val res1 = e2.find(_ == Edge(e1._2, x._2))
          val res2 = e2.find(_ == Edge(x._2, e1._2))
          if (res1 != None || res2 != None) {
            //            println("hasCycle")
            return false
          }
        }
      )
      //noCycle
      //      println(e2)
      val ee1 = e2.head
      val ee2 = e2.drop(1)
      checkcycle(ee1, ee2)
    }
    else
      true
  }

  def mstAide(g: SimpleGraph[V], valuation: Map[Edge[V], Double]): SimpleGraph[V] = {
    val minE = valuation.minBy(_._2)(DoubleIsFractional)._1
    val g2 = g.+|(minE)
    val valuation2 = valuation - (minE)
//        println(g2)
    if (!g2.isAcyclic) {
      val g3 = g2.-|(minE)
//            println("成环,删除 " + minE)
      if (!g3.isConnected)
        mstAide(g3, valuation2)
      else
        g3
    } else {
      if (!g2.isConnected)
        mstAide(g2, valuation2)
      else {
        //        println("成功")
        g2
      }
    }
  }

  def minimumSpanningTree(valuation: Map[Edge[V], Double]): SimpleGraph[V] = {
    val g = this.withoutEdge
    mstAide(g, valuation)
  }

  /* COLORING METHODS */
  /*
    def addDegree(d1: Map[V, Int], d2: Map[V, Int]): Map[V, Int] = {
      val newD = d1 ++ d2
      newD
    }
  */


  /*  def deleteV(g: SimpleGraph[V], v1: V, v: Set[V]): SimpleGraph[V] = {
      val G = g.-(v1)
      val V = v.-(v1)
      if (V != Set()) {

        val V1 = V.head
        //      println(G)
        deleteV(G, V1, V)
      } else
        G
    }

    def withoutVertices: SimpleGraph[V] = {

      val g1 = deleteV(this, this.vertices.head, this.vertices)
      g1.edges.foreach(x =>
        g1.+|(x)
      )
      null
    }*/
  def calVexDegree(g: SimpleGraph[V]): mutable.Map[V, Int] = {
    val degree = scala.collection.mutable.Map[V, Int]()
    g.vertices.foreach(x => {
      //      println(x+" 的度是： "+neighborsOf(x).get.size)
//      degree += ((x, neighborsOf(x).get.size))
      degree += ((x, degreeOf(x).get))

    }
    )
    degree

  }

  def addV(V: Seq[V], v1: V, v: Seq[(V, Int)]): Seq[V] = {
    val V2 = V :+ v1
    val v2 = v.drop(1)
    if (v2 != List()) {
      //      println(v2)
      val v12 = v2.head._1
      addV(V2, v12, v2)
    } else
      V2
  }

  /** Sequence of vertices sorted by decreasing degree 按度降序排列顶点 */
  lazy val sortedVertices: Seq[V] = {
    //        val s1 = Seq[this.vertices]

    val V = calVexDegree(this).toSeq.sortWith(_._2 > _._2)
    val v1 = V.head._1
    val v = Seq[V]()
    //    println(V)
    addV(v, v1, V)

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
