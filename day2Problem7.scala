

object day2Problem7 extends App {
  def day2Problem7(limit: Int) = {
    val numbers = 1 to limit
    for (num <- numbers) {
      val numList = num.toString.map(_.asDigit)
      if (numList.forall(x => ((x % 2) == 0))) 
        println(num)     
    }
  }
  day2Problem7(100)
}