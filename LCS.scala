object Main {
  def main(args: Array[String]): Unit = {
    val input = io.Source.stdin.getLines.toArray // for each line...
    val iter = input.flatMap(
      _.split("""\s+""") // split it into words
        .map(_.toString) // parse each word into an Int
    )
    var s1 = ""
    var s2 = ""
    var count = 0
    for (i <- iter) {
      if (count == 0) s1 = i
      else s2 = i
      count += 1
    }
    val A = scala.collection.mutable.ArrayBuffer[Char]()
    val B = scala.collection.mutable.ArrayBuffer[Char]()
    for (char <- s1) A += char
    for (char <- s2) B += char
    //println(A.toList)
    //println(B.toList)
    val n = A.length
    val m = B.length
    val Opt_i_j = Array.ofDim[Int](n+1, m+1)
    val Pi_i_j = Array.ofDim[String](n+1, m+1)
    //implementing algorithm
    for (j <- 0 to m) Opt_i_j(0)(j) = 0
    for (i <- 1 to n) {
      Opt_i_j(i)(0) = 0
      for (j <- 1 to m) { // Dont I want to start at i = n-1 and j = m-1????
        if (A(i-1) == B(j-1)) {  // and start with the last char for each string
          Opt_i_j(i)(j) = Opt_i_j(i - 1)(j - 1) + 1 //but then if I do that this wont be the same as lecture
          Pi_i_j(i)(j) = "diagonal"  //struggling to visualize what is happening....
        }else if (Opt_i_j(i)(j -1) >= Opt_i_j(i-1)(j)) {
          Opt_i_j(i)(j) = Opt_i_j(i)(j-1)
          Pi_i_j(i)(j) = "left"
        }else {
          Opt_i_j(i)(j) = Opt_i_j(i-1)(j)
          Pi_i_j(i)(j) = "up"
        }
      }
    }
    var p = n
    var q = m
    var LCS = ""
    while (p > 0 && q > 0) {
      if (Pi_i_j(p)(q) == "diagonal") {
        p -= 1
        q -= 1
        LCS += A(p)
      }else if (Pi_i_j(p)(q) == "left") {
        q -= 1
      }else p -= 1
    }
    LCS = LCS.reverse
    println(LCS.length)
    println(LCS)
  }
}