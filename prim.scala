object Main {
  def main(args: Array[String]): Unit = {
    val input = io.Source.stdin.getLines.toArray // for each line...
    val iter = input.flatMap(
      _.split("""\s+""") // split it into words
        .map(_.toInt) // parse each word into an Int
    )
    val iter_ = input.flatMap(
      _.split("""\s+""") // split it into words
        .map(_.toInt) // parse each word into an Int
    )
    var count = 0
    var n = 0
    var m = 0
    for (i <- iter) {
      while (count < 2) {
        if (count == 0) n = i
        if (count == 1) m = i
        count += 1
      }
    }
    val adjMatrix = Array.ofDim[Int](n, n)
    val weights = iter_.toBuffer
    weights.remove(0)
    weights.remove(0)
    weights.toArray
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        adjMatrix(i)(j) = 0
      }
    }
    count = 1
    var h = 0
    var k = 0
    for (i <- 0 until weights.length) {
      if (count == 1) {
        h = weights(i)
      }
      if (count == 2) {
        k = weights(i)
      }
      if (count == 3) {
        adjMatrix(h-1)(k-1) = weights(i)
        adjMatrix(k-1)(h-1) = weights(i)
      }
      if (count < 3) count += 1
      else count = 1
    }
    //print adj matrix
    /*
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        println(adjMatrix(i)(j))
      }
    }
    */
    //now the adjacency matrix has been created and I will implement Prims Algorithm
    var vertices = (1 to n).toSet
    var Gamma: Set[Int] = Set(1)
    var MST_weight = 0
    val F = scala.collection.mutable.ArrayBuffer[(Int, Int)]()
    var tmp_edge = (0, 0)
    while (!(Gamma.size == n)) {
      //println("here") This prints quickly so I know that I am not getting stuck
      //on the previous steps where I initialized the Adj Matrix
      var tmp: Double = 10/0.0 //initially assigned infinity
      for (u <- Gamma) {
        for (v <- vertices) {
          //if Gamma does not contain edge v, and the edge (u,v) exists, and the edge has a weight less than tmp
          if (!Gamma.contains(v) && adjMatrix(u - 1)(v - 1) != 0 && adjMatrix(u - 1)(v - 1) < tmp) {
              tmp = adjMatrix(u - 1)(v - 1)
              tmp_edge = (u, v)
          }
        }
      }
      vertices -= tmp_edge._2
      Gamma += tmp_edge._2
      MST_weight += adjMatrix(tmp_edge._1 - 1)(tmp_edge._2 - 1)
      F += tmp_edge
    }
    println(MST_weight)
    for (i <- F) println(i)
  }
}