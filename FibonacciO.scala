
object Fibonacci extends App{
    /*
    import annotation.tailrec 
    //val FibNum: Stream[Int] = 0 #:: FibNum.scanLeft(1)(_ + _)
    //FibNum take 11 toList  
    http://www.luigip.com/?p=200
  
   Define a Function that prints the Fibonacci Sequence
   from 1 to limit
   */
  
   def Fibonacci(a: Int, b: Int, limit: Int) {
     if (limit < 1) println("There are no Fibonacci numbers less " +
                    "than 1, although sometimes 0 is considered.")
     if (a == 0) println(1)               
     val c = a + b
     if (c > 100) System.exit(0)
     println(c)
     Fibonacci(b, c, limit)//a little recursive
   }
   Fibonacci(0,1,100)
}

