// コメント
/* 別のコメント */
/** 説明のコメント*/
/*EXERCISE 3.8
Listが出来る。
*/

//import fpinscala.datastructures._
//import fpinscala.datastructures.List._
import fpinscala.errorhandling._
import fpinscala.errorhandling.Option._

object Chapter3 {
  def main(args:Array[String]): Unit = {
    //val x = List(1,2,3,4,5) match {
    //  case Cons (x, Cons (2, Cons (4, _))) => x
    //  case Nil => 42
    //  case Cons (x, Cons (y, Cons (3, Cons (4, _)))) => x + y //コレっぽい
    //  case Cons (h, t) => h + sum(t)
    //  case _ => 101
    //}
    //println(x)

    //println(toDString(List(1.1,2.2,3.3,4.4,5.5)))
    //println(filter2(List(1,2,3,4,5,6,7,8,9))(_ <= 5))

    println(map2(Some(1), Some(1))(_ + _))
    println(sequence(List(Some(1), Some(1), Some(2))))
    println(traverse(List(3, 6, 9))(x => if (x % 3 == 0) Some(x) else None))
  }
}

