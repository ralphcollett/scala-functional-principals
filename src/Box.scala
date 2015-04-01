
case class Box[T](value: T)

object Box {

  // functor
  def map[A, B](f: A => B) = (a: Box[A]) => Box(f(a.value))

  // monad
  def flatMap[A, B](f: A => Box[B]) = (a: Box[A]) => f(a.value)

  // applicative
  def _apply[A, B](f: Box[A => B]) = (a: Box[A]) => Box(f.value(a.value))
}

trait Monoid[T] {
  def zero: T
  def op: (T, T) => T
}

object Monoid {
  
  def zeroRule[T](t: T, monoid: Monoid[T]) = monoid.op(t, monoid.zero) == t

  def associativeRule[T](t1: T, t2: T,  t3: T, monoid: Monoid[T]) =
    monoid.op(monoid.op(t1, t2), t3) == monoid.op(t1, monoid.op(t2, t3))
}

class Addition extends Monoid[Int] {
  override def zero: Int = 0

  override def op: (Int, Int) => Int = _ + _
}

class StringConcatenate extends Monoid[String] {

  override def zero: String = ""

  override def op: (String, String) => String = _ + _
}

class Multiplication extends Monoid[Int] {

  override def zero: Int = 1

  override def op: (Int, Int) => Int = _ * _
}

