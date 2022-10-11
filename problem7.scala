

object problem7 extends App {
  /*Write and test a function that takes a numeric argument and returns 
  true if the argument is a prime number (and false if not).
  */
  def problem7(num:Int) =
    //use square root to make more efficient, dont double count
    //ex: 16 - 1,2,4,8 -> no need to go past 4 we already "checked"
    if ((num > 1) && !(2 to scala.math.sqrt(num).toInt).exists(x => num % x == 0)) {
      println(num + " is a prime number")
    }
    else {
      println(num + " is not a prime number")
    }
  problem7(7)
  problem7(49)
  problem7(97)
  problem7(-7)
  problem7(0)
  problem7(1)
  problem7(2)
}