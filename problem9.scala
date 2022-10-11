

object problem9 extends App {
  
  def problem9(x: Int, y: Int, z: Int) = {
    
    if (x > y && y > z) {
      println(z,y,x)
    }
    if (x > z && z > y) {
      println(y,z,x)
    }
    if (y > x && x > z) {
      println(z,x,y)
    }
    if (y > z && z > x) {
      println(x,z,y)
    }
    if (z > x && x > y) {
      println(y,x,z)
    }
    if (z > y && y > x) {
      println(x,y,z)
    }
  }
  problem9(1,2,3)
  problem9(1,3,2)
  problem9(2,1,3)
  problem9(2,3,1)
  problem9(3,1,2)
  problem9(3,2,1)
}

  
