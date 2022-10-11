

object day2Problem6 extends App {
  def day2Problem6(limit: Int): Float = {   
    val sum = (1 to limit).foldLeft(0)(_+_)
    val avg = sum/(limit.toFloat)
    avg   
  }
  println(day2Problem6(34))
  println(day2Problem6(10))
  println(day2Problem6(100))
}