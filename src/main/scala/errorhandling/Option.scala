package fpinscala.errorhandling

sealed trait Option[+A] {
  def map[B] (f: A => B): Option[B] = flatMap(a => Some(f(a)))
  def flatMap[B] (f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(x) => f(x)
  }
  def getOrElse[B >: A] (default: => B): B = this match {
    case None => default
    case Some(x) => x
  }
  def orElse[B >: A] (ob: => Option[B]): Option[B] = 
    map(Some(_)).getOrElse(ob)
  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if(f(a)) Some(a) else None)
}

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean (xs: Seq[Double]): Option[Double] = 
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = 
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))
    
  def map2[A,B,C] (a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a.flatMap(x => b.map(y => f(x, y)))

  def sequence[A] (a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]])((a, b) => a.flatMap(x => b.map(list => x::list)))
  
  def traverse[A, B] (a: List[A]) (f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((x, y) => map2(f(x), y)(_ :: _))

}


