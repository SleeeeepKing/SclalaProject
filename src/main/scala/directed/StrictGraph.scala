package directed

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.{ClassManifest, ClassTag}
import scala.runtime.Nothing$

/** Trait for a directed ''and strict'' graph, i.e. without loop nor parallel arcs */
trait StrictGraph[V] {
    /* QUERY METHODS */

    /** The set of all vertices of the graph */
    val VtoPos:Map[V,Int]
    val vertices : Set[V]
    val map = mutable.Map.empty[Arc[V], Double]

    /** The set of all     arcs of the graph */
    val arcs : Set[Arc[V]]

    /** The set of all vertices with arcs incoming from input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the set of all successors of `v` otherwise
      */
    def successorsOf(v : V) : Option[Set[V]]

    /** The number of incoming arcs to input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the inner degree of `v` otherwise
      */
        def countNumber(v : V,set: Set[Arc[V]],Type:Int):Int={
            if(set.isEmpty)
                0
            else {
                if(Type==1){
                val n=countNumber(v,set.tail,Type)
                if (set.head._1==v)
                    n + 1
                 else
                 n}
                else {
                    val n=countNumber(v,set.tail,Type)
                    if (set.head._2==v)
                        n+1
                    else
                        n
                }
            }

        }
    def deleteArc(arc:Set[Arc[V]],v:V): Set[Arc[V]]={

      if(arc.isEmpty)
        arc
        else {
        if (arc.head._1 == v)
          deleteArc(arc.tail, v)
        else
          Set(arc.head)++deleteArc(arc.tail, v)
      }
    }
    def topological(s:Set[V],arc:Set[Arc[V]],relist:List[V]):List[V]={
      if(s.nonEmpty) {
        val ind=aide_top(s,arc)
        val res=deleteArc(arc,ind)
        return topological(s-ind,res,List(ind)++relist)
      }
      relist
    }
    def aide_top(s:Set[V],arc:Set[Arc[V]]):V= {
      if (arc.nonEmpty) {
        if (inDegreeOf(s.head, s, arc).toList.head == 0 || s.tail.isEmpty) {

          s.head
        } else
          aide_top(s.tail, arc)
      }
      else {

        s.head

    }
    }
    def Hascycle(ves:Set[V],res:Boolean):Boolean={
      if(ves.isEmpty)
        res
        else
        Hascycle(ves.tail,checkcycle(ves.head,ves.head)||res)
    }
  def checkcycle(parent:V,v:V):Boolean={

      if(arcs.exists(y=>y._1==v)){
        arcs.filter(y=>y._1==v).map(x=>(parent==x._2) ||checkcycle(parent,x._2)).exists(w=>w==true)
      }
      else
      false
  }


    def inDegreeOf(v : V,s:Set[V],arc:Set[Arc[V]]) : Option[Int] = {
        if(s.contains(v))
      Some(countNumber(v,arc,2))
      else
            None
    }

    /** The number of outcoming arcs to input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the outer degree of `v` otherwise
      */
    def outDegreeOf(v : V) : Option[Int] = {
        if(vertices.contains(v))
        Some(countNumber(v,arcs,1))
        else
            None
    }
    /** The number of adjacent vertices to input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the degree of `v` otherwise
      */
    def degreeOf(v : V) : Option[Int] = {
        if(vertices.contains(v))
            Some(countNumber(v,arcs,1)+countNumber(v,arcs,2))
        else
            None
    }
    def getMap(arc:Seq[Arc[V]],weight:List[Double]):Map[Arc[V],Double]={
      if(arc.nonEmpty){
      map(arc.head)=weight.head
        (map++getMap(arc.tail,weight.tail)).toMap}
      else
        Map.empty[Arc[V],Double]

    }
    /* VERTEX OPERATIONS */

    /** Add vertex to graph
      * @param v new vertex
      * @return the graph with new vertex `v`
      *         if `v` is an actual vertex of graph, return input graph
      */
    def + (v : V) : StrictGraph[V]

    /** Remove vertex from graph
      * @param v new vertex
      * @return the graph without vertex `v`
      *         if `v` is not an actual vertex of graph, return input graph
      */
    def - (v : V) : StrictGraph[V]

    /* ARC OPERATIONS */

    /** Add arc to graph (also add arc ends as new vertices if necessary)
      * @param a new arc
      * @return the graph with new arc `e`
      *         if `e` is an actual arc of graph, return input graph
      */
    def +| (a : Arc[V]) : StrictGraph[V]

    /** Remove arc from graph (does NOT remove ends)
      * @param a new arc
      * @return the graph without arc `e`
      *         if `e` is not an actual arc of graph, return input graph
      */
    def -| (a : Arc[V]) : StrictGraph[V]

    /** Remove all arcs from graph but keep same vertices
      * @return graph with same vertices without any arc
      */
    def withoutArc : StrictGraph[V]

    /** Add all possible arc with same vertices
      * @return graph with same vertices and all possible arcs
      */
    def withAllArcs : StrictGraph[V]

    /* SEARCH METHODS */

    /** A topological order of the vertex set (if exists) */
    lazy val topologicalOrder : Option[Seq[V]] = {
      if(Hascycle(vertices,false))
        None
      else
        Some(topological(vertices,arcs,List.empty[V]).to(Seq).reverse)
    }

    /* VALUATED GRAPH METHODS */

    /** Computes a shortest path between two vertices
      * @param valuation valuation of graph
      * @param start origin      of path
      * @param end   destination of path
      * @return [[None]] if there is no path from `start` to `end`, the shortest path and its valuation otherwise
      */
    def shortestPath(valuation : Map[Arc[V], Double])(start : V, end : V) : Option[(Seq[V], Double)] ={
      val edgeTo = mutable.ArrayBuffer.fill(vertices.size)(-1)
      val disTo=mutable.ArrayBuffer.fill(vertices.size)(Double.PositiveInfinity)

      disTo(VtoPos(start))=0.0
      edgeTo(VtoPos(start))=(-1)
      val sourceDist=(start,disTo(VtoPos(start)))
      val sortByWeight:Ordering[(V,Double)]=(a,b)=>a._2.compareTo(b._2)
      val queue=mutable.PriorityQueue[(V,Double)](sourceDist)(sortByWeight)
      val res=aideQueue(queue, valuation, edgeTo,disTo) match {
        case Some(x)=>(x._2,x._3)
        case None=>(disTo,edgeTo)
      }

      if(res._1(VtoPos(end))!=Double.PositiveInfinity) {

        val path=getPath(res._2,end).reverse
        Some((path,res._1(VtoPos(end))))
      } else
        None
    }
  def getPath(gPath:mutable.ArrayBuffer[Int],end:V):Seq[V]={

    if(gPath(VtoPos(end))>=0){
    val next=vertices.toList(gPath(VtoPos(end)))

      Seq(end)++getPath(gPath,next)}
    else
      Seq(end)
  }
  def aideQueue(queue: mutable.PriorityQueue[(V,Double)],valuation:Map[Arc[V], Double],edgeTo:mutable.ArrayBuffer[Int],disTo:mutable.ArrayBuffer[Double]):Option[(mutable.PriorityQueue[(V,Double)],mutable.ArrayBuffer[Double],mutable.ArrayBuffer[Int])]={
     if(queue.nonEmpty){
      val (minDestV, _) = queue.dequeue()
      val s=arcs.filter(y=>y._1==minDestV)
      s.foreach{e=>
        if(disTo(VtoPos(e._2))>disTo(VtoPos(e._1))+valuation(e)) {
          disTo(VtoPos(e._2))=disTo(VtoPos(e._1))+valuation(e)
          edgeTo(VtoPos(e._2))=VtoPos(e._1)
          if (!queue.exists(_._1 == e._2)) queue.enqueue((e._2, disTo(VtoPos(e._2))))
        }
        }
       aideQueue(queue, valuation, edgeTo,disTo)
      }
    else
       None
  }



    /* toString-LIKE METHODS */

    /** @inheritdoc */
    override lazy val toString : String = s"({${vertices mkString ", "}}, {${arcs mkString ", "}})"

    /** Graph representation in DOT language */
    lazy val toDOTString : String = {
        "strict graph {\n" +
        "    // Edges\n" +
        (arcs foldLeft "    ") { _ + _.toDOTString + "\n    " } + "\n" +
        "    // Vertices\n" +
        vertices.mkString("    ", "\n    ", "\n") +
        "  }\n"
      }

  }
