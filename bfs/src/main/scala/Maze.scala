import scala.annotation.tailrec

object Maze extends App{

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

  def load(maze: Vector[String], rowPos: Int): Map[(Int, Int), Set[(Int, Int)]] = {
    if rowPos == maze.size then Map()
    else {
      @tailrec
      def parseRow(accA: Map[(Int, Int), Set[(Int, Int)]], colPos: Int): Map[(Int, Int), Set[(Int, Int)]] = {
        if colPos == maze(0).length then accA
        else {
          @tailrec
          def checkNeighbours(coordinate: (Int, Int), acc: Set[(Int, Int)], dir: Int): Set[(Int, Int)] = {
            if maze(coordinate._1-1)(coordinate._2) != 'x' && rowPos != 0  && dir == 0
              then checkNeighbours(coordinate, acc + ((coordinate._1-1, coordinate._2)), dir+1) // CHECK FOR UP
            else if maze(coordinate._1+1)(coordinate._2) != 'x' && rowPos != maze.size-1 && dir == 1
              then checkNeighbours(coordinate, acc + ((coordinate._1+1, coordinate._2)), dir+1) // CHECK FOR DOWN
            else if maze(coordinate._1)(coordinate._2-1) != 'x' && colPos != 0 && dir == 2
              then checkNeighbours(coordinate, acc + ((coordinate._1, coordinate._2-1)), dir+1) // CHECK FOR LEFT
            else if  maze(coordinate._1)(coordinate._2+1) != 'x' && colPos != maze(0).length-1 && dir == 3
              then checkNeighbours(coordinate, acc + ((coordinate._1, coordinate._2+1)), dir+1) // CHECK FOR RIGHT
            else if dir > 3
              then acc
            else checkNeighbours(coordinate, acc, dir+1)
          }

          if maze(rowPos)(colPos) != 'x' then
            parseRow(accA + ((rowPos, colPos) -> checkNeighbours((rowPos, colPos), Set(), 0)), colPos+1)
          else parseRow(accA, colPos+1)
        }
      }

      parseRow(Map(), 0) ++ load(maze, rowPos+1)
    }
  }

  def solveMaze(maze: Vector[String]): Option[String] = {
    val A = load(maze, 0)
    def neighbours(node: (Int, Int)): Set[(Int, Int)] = {
      A(node)
    }
    val source = (maze.indexWhere(s => s.contains('s')), maze.filter(_.contains('s'))(0).indexOf('s')) // starting point
    val toSource = bfs(neighbours, source)._1
    @tailrec
    def walkingPath(pos: (Int, Int), direction: String): String = {
      if pos == source then direction
      else {
        val nextPos = toSource(pos)
        if nextPos._1 < pos._1 then walkingPath(nextPos, direction + 'd') // Go down
        else if nextPos._1 > pos._1 then walkingPath(nextPos, direction + 'u') // Go up
        else if nextPos._2 < pos._2 then walkingPath(nextPos, direction + 'r') // Go right
        else walkingPath(nextPos, direction + 'l') // Go left
      }
    }
    val destination = (maze.indexWhere(s => s.contains('e')), maze.filter(_.contains('e'))(0).indexOf('e')) // end point
    Some(walkingPath(destination, "").reverse)
  }

//  val maze = Vector(
//    "xxxxxxxxxxxxxxxxxx",
//    "x    x       x   x",
//    "x    x   x   x xex",
//    "x        x   x   x",
//    "xs   x   x       x",
//    "xxxxxxxxxxxxxxxxxx"
//  )
//  println(solveMaze(maze))
}
