

object problem1 extends App{
  
  def problem1(name: String, age: Int) = {
    
    val year = (65 - age) + 2019
    
    println("Hello, " + name + ".  You will turn 65 during the year " + year)
    
  }
  
  val name = scala.io.StdIn.readLine("What is your name? ")
  
  val age = scala.io.StdIn.readLine("How old are you? ").toInt
  
  problem1(name: String, age: Int)  
    
}