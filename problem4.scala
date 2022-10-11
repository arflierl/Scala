

object speeding_points extends App{
  
  def speeding_points(speed: Int) = {
    
    if (speed <= 55) {
      println("ok")
    }
    
    if (speed > 55 && speed < 120) {
      println("Points: " + ((speed - 55) / 5))
    }
    
    if (speed >= 120) {
      println("License Suspended!  Watch out for kids and pets!!!!")
    }
    
  }
  
  speeding_points(55)
  speeding_points(60)
  speeding_points(90)
  speeding_points(119)
  speeding_points(120)
}