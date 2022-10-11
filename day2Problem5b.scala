

object day2Problem5b extends App {
  def day2Problem5b(password: String) = {
    var check = true
    while (check == true) {
      if (password.length < 6) {
        println("Password must contain at least 6 acceptable characters")
        check = false     
      }
      if (password.length > 16) {
        println("Password must contain no more than 16 acceptable characters")
        check = false
      }
      if ((password.exists(x => (x.isUpper))) == false) {
        println("Password must contain one uppercase letter")
        check = false
      }    
      if ((password.exists(x => (x.isLower))) == false) {
        println("Password must contain one lowercase letter")
        check = false
      }
      if ((password.exists(x => (x.isDigit))) == false) {
        println("Password must contain at least one number")
        check = false      
      }
      if ((password.exists(_==('$') || password.exists(_==('@')) 
          || password.exists(_==('#')))) == false) {
        println("Password must contain one of the following characters: " +
            "'@', '$', or '#'. ")
            check = false
      }
      if (check == true) {
        println(password + " is acceptable")
        check = false
      }
    }
  }
 day2Problem5b("Aa1#")
 println()
 day2Problem5b("A1@ccccccccccccccc")
 println()
 day2Problem5b("aaaaaa")
 println()
 day2Problem5b("AAAAAAA")
 println()
 day2Problem5b("AAAAaaA")
 println()
 day2Problem5b("TexasRangers2")
 println()
 day2Problem5b("TexasRangers#1")
 println()
 println("No wonder its so hard to remember all of your Passwords")
 sys.exit
}