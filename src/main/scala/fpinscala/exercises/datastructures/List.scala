package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match
    case Nil => Nil
    case Cons(x, xs) => xs


  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil => Nil
    case Cons(x, xs) => Cons(h, xs)


  def drop[A](l: List[A], n: Int): List[A] =
    if n <= 0 then l
    else l match
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Nil => Nil
    case Cons(x, xs) =>
      if f(x) then dropWhile(xs, f)
      else l

  def init[A](l: List[A]): List[A] = l match
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case Nil => Nil

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, acc) => acc + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil => acc
    case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _+_)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 0.0, _*_)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A], (acc, x) => Cons(x, acc))

  def foldLeftByFoldRight[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    foldRight(reverse(l), acc, (a: A, b: B) => f(b,a))

  def foldRightByFoldLeft[A,B](l: List[A], acc: B, f: (A, B) => B): B =
    foldRight(reverse(l), acc, (a: A, b: B) => f(a,b))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, (x, xs) => Cons(x, xs))

  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, List[A](), append(_, _))

  def incrementEach(l: List[Int]): List[Int] =
    foldLeft(reverse(l), List[Int](), (acc, x) => Cons(x+1, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldLeft(reverse(l), List[String](), (acc, x) => Cons(x.toString, acc))

  def map[A,B](l: List[A], f: A => B): List[B] =
    foldRight(l, List[B](), (x, acc) => Cons(f(x), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, List[A](), (x, acc) => if(f(x)) then Cons(x, acc) else acc)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
    foldLeft(as, List[B](), (acc, x) => append(acc, f(x)))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, (a: A) => if f(a) then List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match
    case (a, Nil) => Nil
    case (Nil, b) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(a+b, addPairwise(as, bs))

  // def zipWith - TODO determine signature

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(x, xs), Cons(y, ys)) => if (x==y) {
      hasSubsequence(xs, ys)
    } else {
      hasSubsequence(xs, sub)
    }
