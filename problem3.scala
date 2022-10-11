

object summer_winter extends App {
  
  def summer_winter (limit: Int) = {
    if (limit % 3 == 0 && limit % 5 != 0) {
      println("summer")
    }  
    if (limit % 5 == 0 && limit % 3 != 0) {
      println("winter")      
    }
    if (limit % 3 == 0 && limit % 5 == 0) {
      println("summer_winter")
    }
    if (limit % 3 !=0 && limit % 5 != 0) {
      println(limit)
    }
  }
  summer_winter(15)
  summer_winter(33)
  summer_winter(100)
  summer_winter(97)
}