// コメント
/* 別のコメント */
/** 説明のコメント*/

object MyModule {
  def abs (n: Int) :Int = 
    if (n < 0) -n
    else n
    
  private def formatAbs (x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args:Array[String]): Unit =
    //println(formatAbs(-42))
    println(fib(6))

  def fib(n: Int): Int = {
    def go(n: Int, a: Int, b: Int): Int =
      if (n <= 0) 0
      else if (n == 1) b
      else go(n-1, b, a+b)
    go(n, 0, 1)
  }

  def isSorted[A](as :Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = 
      if(as.length <= n) true
      else if (!ordered(as(n-1), as(n))) false
      else loop(n+1)
    loop(1)
  }
  
  def curry[A,B,C](f: (A, B) => C) : A => (B => C) = a => b => f(a,b)
  def uncurry[A,B,C](f: A => B => C) : (A, B) => C = (a,b) => f(a)(b)
  def compose[A,B,C](f: B => C, g: A => B) : A => C = a => f(g(a))
}


