import scala.annotation.tailrec

object GraphBFS {

  def bfs[V](nbrs: V => Set[V], src: V): (Map[V, V], Map[V, Int]) = {
    def expand(frontier: Set[V], parent: Map[V, V]): (Set[V], Map[V, V]) = {
      val newFrontier = frontier.map(i => nbrs(i)).toList
      val frontier_ = newFrontier.flatten.filterNot(i => parent.contains(i)).toSet

      def helper(parentFrontier: List[V], childFrontier: List[Set[V]], index: Int): Map[V, V] = {
        if childFrontier.isEmpty then Map()
        else {
          val h = childFrontier.head
          h.filterNot(i => parent.contains(i))
          helper(parentFrontier, childFrontier.tail, index+1) ++ h.map(i => i -> parentFrontier(index)).toMap
        }
      }
      (frontier_, parent ++ helper(frontier.toList, newFrontier, 0))
    }
    @tailrec
    def iterate(frontier: Set[V], parent: Map[V, V], distance: Map[V, Int], d: Int):
    (Map[V, V], Map[V, Int]) =
      if frontier.isEmpty then
        (parent, distance)
      else {
        val (frontier_, parent_) = expand(frontier, parent)
        val distance_ = distance ++ frontier.map(i => i -> (d+1) )// derive new distance map
        iterate(frontier_, parent ++ parent_.filterNot((k, _) => parent.contains(k)), distance_, d + 1)
      }
    iterate(Set(src), Map(src -> src), Map(), 0)
  }

}
