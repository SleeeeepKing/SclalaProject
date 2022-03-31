package undirected

object Test2 {
  // Main method
  val g = new test()
  def main(args:Array[String])
  {

    println(g.minimumSpanningTree(g.getMap(g.edges.toList.sortBy(x=>x._1),List(4,2,5,6,7,3,8,9))))
  }

}
