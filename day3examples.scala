object examples {
  def main(args:Array[String])
  {
    var index : Int = 99
    while(index != 0)
    {
      index = (scala.io.StdIn.readLine("Please input 1..7  (0 to quit )  ").toInt)
      index match
      {
        case 0 => zero()
        case 1 => one()
        case 2 => two()
        case 3 => three()
        case 4 => four()
        case 5 => five()
        case 6 => six()
        case 7 => seven()
      }
      println()
    }
  }

  // zero
  def zero() : Unit =
  {
    println("\nterminating Testing!")
    System.exit(0)
  }
  
  // one
   def one() : Unit = 
   {
     println("#1")
     print("Please enter your first name: ")
     var name = scala.io.StdIn.readLine()
     println("First name = " + name)
     println()
     name = scala.io.StdIn.readLine("Now type your last name")
     println("Last name = " + name + "\n")
   }

// two
   def two () : Unit =
   {
     System.out.println("#2")
     println("First we write a file")
//     val fileName : String = "/eng/home/scandale/Desktop/textFile.txt"
     val fileName : String = "C:\\Users\\jim\\Desktop\\textFile.txt"
     val fw = new java.io.BufferedWriter(new java.io.FileWriter(fileName))
     for (i <- 1 to 10)
     {
       fw.write("Line #" + i + "\n")
       println("Line #" + i + "  written to file.")
     }
     fw.close()
     
     println("Now we read the file back and list it.")
     val buffSrc = scala.io.Source.fromFile(fileName)
     for (line <- buffSrc.getLines)
        println(line)
     buffSrc.close()
   }
   
// three
     class coord (ix : Double, iy : Double, iz : Double)
     {
       private var x : Double = ix
       private var y : Double = iy
       private var z : Double = iz
       
       def get_x() : Double =
       {
         return x
       }
       def get_y() : Double =
       {
         return y
       }
       def get_z() : Double =
       {
         return z
       }
       def distFrom(other : coord) : Double =
       {
         val xs = scala.math.pow((x - other.get_x()), 2.0)
         val ys = scala.math.pow((y - other.get_y()), 2.0)
         val zs = scala.math.pow((z - other.get_z()), 2.0)
         return scala.math.pow((xs+ys+zs), 0.5)
       }
       // other functions possible & useful
     }
   def three () : Unit =
   {
     println("#3")
     
     println("define a point - enter x, y and z")
     val x = scala.io.StdIn.readDouble()
     val y = scala.io.StdIn.readDouble()
     val z = scala.io.StdIn.readDouble()
     val point1 = new coord(x, y, z)
     val origin = new coord(0.0, 0.0, 0.0)
     println("Radius of origin-centered sphere is: " + point1.distFrom(origin))
   }
   
   // four
   def four() : Unit =
   {
     println("#4")
     while(true)
     {
       var n = 0
       try
       {
         print("Please enter a grade value: ")
         n = scala.io.StdIn.readInt() 
         // do something with the value - sum, avg, whatever
       }
       catch
       {
         case e : java.lang.NumberFormatException =>
           {
             println("Illegal input - Integer needed")
           }
       }
     }
   }
   
   // five
   def five() : Unit =
   {
     println("#5")
     print("Please enter value for calculation: ")
     var x = scala.io.StdIn.readDouble()
     var y : Double = 0.0
     try
     {
       y = sqrt(x)
     }
     catch
     {
       case e : java.lang.IllegalArgumentException =>
       {
         println("Can't take square root of this value")
         return
       }
     }
     // do something with sqrt of x
     println(y + " = square root of " + x)
     return
   }
   def sqrt(arg : Double) : Double =
   {
     if (arg < 0.0)
     {
       throw new java.lang.IllegalArgumentException()
     }
     // do square root calculation
     return scala.math.pow(arg, 0.5)
   }

   // six
   def func(x : Int) : Int =
   {
     val y = 2 * x
     return y
   }
   def six() : Unit = 
   {
     var x : Int = 1
     x = func(x+1) + func(x+2)
     println("Current value of x is " + x)
   }
   
   // seven
   import scala.collection.mutable
   def seven() : Unit = 
   {
     var l1 = List(1, 2, 3)        // default - immutable
     l1 = 7 :: l1
     println(l1)
//     var l2 = mutable.List(4, 5, 6)// designated mutable
     var m1 = Map[Int, String]()    // default - immutable
     m1 = m1 + (1 -> "one")
     m1 = m1 + (2 -> "two")
//     m1(3) = "three"              // can't update in-place
     println(m1)
     var m2 = mutable.Map[Int, String]()
     m2(1) = "100"
     m2(2) = "200"
     m2 += (3-> "300")
     println(m2)
   }

}