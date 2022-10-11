

object oddEven extends App {
  
  def oddEven(limit: Int) = {
    
    /*Write and test a function called oddEven that takes a parameter called limit. 
    It should print all the numbers between 0 and limit with a label to identify 
    the even and odd numbers. 
    */
    
    //a little "functional"
    def even(limit: Int) = limit % 2 == 0
    def odd(limit: Int) = !even(limit)
    
    val nums = List.range(0, 8)
    
    for (n <- nums) {      
      print(n)      
      if (even(n)) {       
        println(" -> EVEN")
      }     
      if (odd(n)) {
        println(" -> ODD")
      }
    }
  }
  oddEven(7)
}