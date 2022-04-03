package undirected

/** Implementation of [[SimpleGraph]] using list of neighbors for each vertex
  * @param neighbors associative map providing set of neighbors for each vertex
  *                  Key must be defined for any vertex in graph : should an actual vertex have no neighbor, value is defined and is an empty set
  * @tparam V type for vertices
  */
case class SimpleGraphNeighborsImpl[V](neighbors : Map[V, Set[V]]) extends SimpleGraph[V] {

    /** @inheritdoc */
    val vertices : Set[V] = neighbors.map(x=>x._2).toSet.flatten

    /** @inheritdoc */
    val edges : Set[Edge[V]] = neighbors.map(x=>x._2.map(a=>Set(Edge(x._1,a)))).toSet.flatten.flatten

    /** @inheritdoc */
    def neighborsOf(v: V) : Option[Set[V]] = {
        Some(neighbors(v))
    }

    /** @inheritdoc */
    def + (v : V) : SimpleGraphNeighborsImpl[V] = {
        SimpleGraphNeighborsImpl(neighbors + (v->Set()))
    }

    /** @inheritdoc */
    def - (v : V) : SimpleGraphNeighborsImpl[V] = {
        SimpleGraphNeighborsImpl(neighbors - v)
    }

    /** @inheritdoc */
    def +| (e: Edge[V]) : SimpleGraphNeighborsImpl[V] = {
        SimpleGraphNeighborsImpl(neighbors + (e._1->Set(e._2))+(e._2->Set(e._1)))
    }

    /** @inheritdoc */
    def -| (e: Edge[V]) : SimpleGraphNeighborsImpl[V] = {
        SimpleGraphNeighborsImpl(neighbors - e._1-e._2)
    }

    /** @inheritdoc */
    def withoutEdge : SimpleGraphNeighborsImpl[V] = {
        SimpleGraphNeighborsImpl(neighbors.map(x=>(x._1->Set.empty[V])))
    }
    /** @inheritdoc */
    def withAllEdges : SimpleGraphNeighborsImpl[V] = {
        var set : Set[V] = Set()
        neighbors.map(x=>set += x._1)
        SimpleGraphNeighborsImpl(neighbors.map(x=>(x._1->set)))
    }
}

