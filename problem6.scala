

object problem6 extends App {
  
    /*Write and test a function that returns the sum of multiples of 3 and 5 
    between 0 and a given integer.
    */
  
    // || is called Logical OR Operator. If any of the two operands is non zero then condition becomes true
    def problem6(range: Range): Int = {
        range.filter(n => n % 3 == 0 || n % 5 == 0).sum
    }

    val strLimit = scala.io.StdIn.readLine("Enter 0 to quit, or" + 
    " enter an integer to calculate the sum of multiples of 3 and 5 up " + 
    "to and including the entered integer: ")
    
    val limit = strLimit.toInt
    
    if (limit != 0) {
      //string interpolation (use s)
      //https://www.oreilly.com/library/view/scala-cookbook/9781449340292/ch01s05.html
      //define range while calling the function!
      println(s"The sum of multiples of 3 or 5 between 0 and ${limit} is " + problem6(1 to limit))
    }
    
    if (limit == 0) {
      println("Exiting program...")
    }  
}