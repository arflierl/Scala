

object day2Problem4 extends App {
  /*Write and test a function that asks for the names of all the members 
  in a club. However, we don't know how many members are actually in 
  the club. Use a “while-loop” which will simply repeat until all the 
  member's names have been entered. How will you communicate to the 
  program that there are no more names to enter?
  */
  def day2Problem4() = {
    import scala.collection.mutable.MutableList    
    var end = false
    val clubMembers = MutableList[String]()
    while (!end) {
      val member = scala.io.StdIn.readLine("Enter a club members name " 
          + "or enter 'end' to exit and print a list of club members: ")
      if (member != "end") {
          clubMembers += member
      }    
      if (member == "end") {
        println(clubMembers)
        end = true
        sys.exit
      }
    }
  }  
  day2Problem4
}