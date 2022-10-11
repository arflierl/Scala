

object problem8 extends App {
  def problem8(limit: Int) = {
    for (n <- 0 to limit) {
      if ((n > 1) && !(2 to scala.math.sqrt(n).toInt).exists(x => n % x == 0))
        println(n)
      }
  }  
  problem8(100)
}