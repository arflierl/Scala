

object problem2 extends App {
  
  def problem2(x: Int, y: Int): Int = {
    if (x > y) 
      x
    else 
      y
  }
  println(problem2(7, 10))
}