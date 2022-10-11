
object day2Problem1 extends App {
  /*Write and test a function that accepts a string as an argument 
  and prints out whether this string is a palindrome or not. 
	A palindrome is a string that reads the same forwards and backwards. 
	In testing your string you should ignore blanks, punctuation and case 
	differences (be case-insensitive).*/
  def day2Problem1(x: String) = {
    
    val xReady = x.toLowerCase.replaceAll("""[\p{Punct}]""", "").replaceAll("\\s", "")
    
    val y = xReady.reverse
    
    if (xReady == y) {
      println(x + " is a palindrome")
    }
    else 
    {
      println(x + " is not a palindrome")
    }
  }
  
  day2Problem1("sit on a Potato Pan, Otis.")
  day2Problem1("Radar")
  day2Problem1("Ra.d!a.r")
  day2Problem1("sit on a Potato Pan, Otis.")
}
