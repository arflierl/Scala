

object day2Problem3 extends App{
  /*Write and test a function that accepts a string argument. This will
  be a long string containing multiple words. Print the string but 
  with the words (not the characters) reversed.
  */
  def day2Problem3(x: String)= {
    //x.reverse -> Obvious
    //x.foldLeft("")((Char, nextChar) => nextChar + Char)
    //(x.+:() yields individual chars in Vector
    //x.foldLeft("")(_.+:(_)) 
    //x.foldLeft("")(_.:+(_)) returns original string
    val xList = x.split(" ")
    for (s <- xList.reverse) {
      print(s + " ")
      
    }
    
  }
  day2Problem3("hello old friend")
  
}