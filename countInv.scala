object Main {
  def main(args: Array[String]): Unit = {
    val input = io.Source.stdin.getLines.toArray // for each line...
    val iter = input.flatMap(
      _.split("""\s+""") // split it into words
        .map(_.toInt) // parse each word into an Int
    )
    var n: Int = 0
    val input_Array = scala.collection.mutable.ArrayBuffer[Int]()
    var counter = 0
    for (i <- iter) {
      if (counter == 0) n = i
      else input_Array += i
      counter += 1
    }
    val _input_Array = input_Array.toArray

    def merge_And_Count(B:Array[Int], C:Array[Int], n1:Int, n2:Int): (Array[Int], Long) = {
      var count: Long = 0
      val A = scala.collection.mutable.ArrayBuffer[Int]()
      var i = 0
      var j = 0
      while (i < n1 || j < n2) {
        if (j >= n2) {
          A.append(B(i))
          i += 1
          count += (j)
        }else if (i < n1 && B(i) <= C(j)){
          A.append(B(i))
          i += 1
          count += (j)
        }else {
          A.append(C(j))
          j += 1
        }
      }
      (A.toArray, count)
    }
    def sort_And_Count(A:Array[Int], n:Int): (Array[Int], Long) = {
      if (n == 1) return (A, 0)
      val q = scala.math.floor(A.length.toDouble/2)
      val p = scala.math.ceil(A.length.toDouble/2)
      val B = A.slice(0, q.toInt)
      val C = A.slice(q.toInt, A.length)
      val (b:Array[Int], m1:Long) = sort_And_Count(B, q.toInt)
      val (c:Array[Int], m2:Long) = sort_And_Count(C, p.toInt)
      val (a:Array[Int], m:Long) = merge_And_Count(b, c, q.toInt, p.toInt)
      (a, m + m1 + m2)
    }
    val retval = sort_And_Count(_input_Array, _input_Array.length)
    println(retval._2)
  }
}