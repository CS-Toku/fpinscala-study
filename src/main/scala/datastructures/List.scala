package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons [+A] (head: A, tail: List[A]) extends List[A]

object List {
  def sum (ints: List[Int]) : Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]) : Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  
  def apply[A] (as: A*) : List[A] = 
    if (as.isEmpty) Nil
    else Cons (as.head, apply(as.tail: _*))

  def head[A] (l: List[A]) : A = l match {
    case Nil => throw new Exception
    case Cons(x, _) => x
  }
  def tail[A] (l: List[A]) : List[A] = l match {
    case Nil => throw new Exception
    case Cons(x, xs) => xs
  }

  def setHead[A] (n: A, l: List[A]) : List[A] = l match {
    case Nil => throw new Exception
    case Cons(_, xs) => Cons(n, xs)
  }

  def drop[A] (l: List[A], n: Int): List[A] = 
    if (n <= 0) l
    else drop(tail(l), n-1)

  def dropWhile[A] (l: List[A], f: A => Boolean): List[A] = 
    if ( !f(head(l)) ) l
    else dropWhile(tail(l), f)

  def init[A] (l: List[A]): List[A] = l match {
    case Nil => throw new Exception
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }
  
  def append[A] (a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons (h, append(t, a2))
  }

  //0.0を検出した際に直ちに再帰を中止することはできない
  def foldRight[A,B] (as: List[A], z: B) (f: (A, B) => B):B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Int]) = 
    foldRight(ns, 1)(_ * _)

  def length2(ns: List[Int]) = 
    foldRight(ns, 0)((_, y) => 1 + y)
  

  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B):B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]) = 
    foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Int]) = 
    foldLeft(ns, 1)(_ * _)

  def length3(ns: List[Int]) = 
    foldLeft(ns, 0)((x, _) => x + 1)


  def reverse[A] (l: List[A]):List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => append(reverse(xs), Cons(x, Nil))
  }
    
  def reverse2[A] (l: List[A]):List[A] =
    foldLeft(l, Nil: List[A])((xs: List[A], n:A) => Cons(n, xs))

  def append2[A] (a1: List[A], a2: List[A]): List[A] = 
    foldRight(a1, a2)(Cons(_,_))

  def foldLeft2[A,B] (as: List[A], z: B) (f: (B, A) => B):B = 
    foldRight(reverse2(as), z)((x, y) => f(y, x))
  def foldRight2[A,B] (as: List[A], z: B) (f: (A, B) => B):B = 
    foldLeft(reverse2(as), z)((x, y) => f(y, x))
  

  def flatlist[A] (a1: List[List[A]]): List[A] = a1 match {
    case Nil => Nil
    case Cons(xs, xxs) => append(xs, flatlist(xxs))
  }

  def add_one (l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x+1, add_one(xs))
  }

  def toDString (l: List[Double]): String = 
    foldLeft(l, "")((z, x) => z+","+x.toString).tail

  def map[A,B] (as :List[A])(f:A=>B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x),map(xs)(f))
  }
  
  def filter[A] (as: List[A])(f:A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => 
      if (f(x)) Cons(x, filter(xs)(f))
      else filter(xs)(f)

  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x),flatMap(xs)(f))
  }

  def filter2[A] (as: List[A])(f:A=>Boolean): List[A] = 
    flatMap(as)((x) => if(f(x)) List(x) else Nil)

  def add_pair(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, add_pair(xs, ys))
  }

  def zipWith[A] (l1: List[A], l2: List[A])(f: (A, A) => A) : List[A] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  def isPrefix[A] (sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil, _) => false
    case (_, Nil) => true
    case (Cons(x, xs), Cons(y, ys)) =>
      if(x != y) false
      else isPrefix(xs, ys)
  }

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case Cons(x, xs) =>
      if(isPrefix(sup, sub)) true
      else hasSubsequence(xs, sub)
  }


}


