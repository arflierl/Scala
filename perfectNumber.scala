

object perfectNumber extends App {
  def perfectNumber(x: Int) = {
    var sum = 0
    for (i <- 1 until x) {   
      if (x % i == 0) {
        sum += i 
      }  
    }      
    if (sum == x) {
      println(x + " is a Perfect Number")
    }
    if (sum != x) {
      println(x + " is not a Perfect Number")
    }
  }
  perfectNumber(20)
  perfectNumber(28)
  perfectNumber(272)
  perfectNumber(496)
  perfectNumber(8128)
  perfectNumber(-20)
  perfectNumber(-28)
}