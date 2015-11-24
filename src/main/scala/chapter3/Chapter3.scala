// コメント
/* 別のコメント */
/** 説明のコメント*/
/*EXERCISE 3.8
Listが出来る。
*/

import fpinscala.datastructures._
import fpinscala.datastructures.List._

object Chapter3 {
  def main(args:Array[String]): Unit = {
    val x = List(1,2,3,4,5) match {
      case Cons (x, Cons (2, Cons (4, _))) => x
      case Nil => 42
      case Cons (x, Cons (y, Cons (3, Cons (4, _)))) => x + y //コレっぽい
      case Cons (h, t) => h + sum(t)
      case _ => 101
    }
    println(x)
  }
}

