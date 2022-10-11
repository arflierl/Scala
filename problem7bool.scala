


  object problem7bool extends App {
    /*Write and test a function that takes a numeric argument and returns 
    true if the argument is a prime number (and false if not).
    */
    def problem7bool(num:Int):Boolean = {
      //use square root to make more efficient, don't double count
      //ex: 16 - 1,2,4,8,16 -> no need to go past 4, we already "checked"
      //Again, took advantage of !
      (num > 1) && !(2 to scala.math.sqrt(num).toInt).exists(x => num % x == 0)
    }  
    println(problem7bool(7))
    println(problem7bool(49))
    println(problem7bool(97))
    println(problem7bool(-7))
    println(problem7bool(0))
    println(problem7bool(1))
    println(problem7bool(2))
  }