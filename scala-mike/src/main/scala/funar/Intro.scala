package funar

import scala.annotation.tailrec

// note: there are two Scala versions right now
// Scala 2 and Scala 3

val s: String = "Mike"

// A pet is one of the following:
// - a dog - OR -
// - a cat - OR -
// - a snake
// mixed data
// trait == interface
// enumeration
/*
sealed trait Pet
/*
case class Dog() extends Pet
case class Cat() extends Pet 
case class Snake() extends Pet 
*/
case object Dog extends Pet 
case object Cat extends Pet  
case object Snake extends Pet 
*/

enum Pet {
  case Dog
  case Cat
  case Snake
}

val p0 = Pet.Snake

import Pet._

val p1: Pet = Dog
val p2: Pet = Cat

// Is a pet cute?
// def defines a function
/*
def isCute(pet: Pet): Boolean =
  // template / "Schablone"
  pet match { // pattern matching
    case Cat => ???
    case Dog => ???
    case Snake => ???
  }
*/ 
def isCute(pet: Pet): Boolean =
  // template / "Schablone"
  pet match { // pattern matching
    case Cat => true
    case Dog => true
    case Snake => false
  }

// Animals on the Texas Highway

// An armadillo has the following properties:
// - dead - OR - alive
// - weight

enum Liveness {
  case Dead
  case Alive
}

import Liveness._

type Weight = Double

// An animal is one the following:
// - armadillo
// - parrot
/*
sealed trait Animal {
  def runOver(): Animal =
    this match {
      case Dillo(_, w) => Dillo(Dead, w)
      case Parrot(_, w) => Parrot("", w)
    }
}
case class Dillo(liveness: Liveness, weight: Weight) extends Animal
case class Parrot(sentence: String, weight: Weight) extends Animal
*/
enum Animal {
  case Dillo(liveness: Liveness, weight: Weight)
  case Parrot(sentence: String, weight: Weight)
  def runOver: Animal =
    this match {
      case Dillo(_, w) => Dillo(Dead, w)
      case Parrot(_, w) => Parrot("", w)
    }

  def feed(amount: Weight): Animal =
    this match {
      // case Dillo(Dead, weight) => this
      // case Dillo(Alive, weight) = Dillo(Alive, weight + amount)
      case Dillo(liveness, weight) =>
        liveness match {
          case Dead => this
          case Alive => Dillo(Alive, weight + amount)
        }
      case Parrot(sentence, weight) =>
        Parrot(sentence, weight + amount)
    }
}

import Animal._

val dillo1 = Animal.Dillo(Liveness.Alive, 10)
val dillo2 = Dillo(Liveness.Dead, 8)

// run over an armadillo
def runOverDillo(dillo: Dillo): Dillo = {
  import Liveness._
  Dillo(Dead, dillo.weight)
}


def silly(x: Int, y: Int) = {
  val sum = x + y
  sum * 2
}

// A parrot has the following properties:
// - a sentence
// - weight

// run over an animal
def runOverAnimal(animal: Animal): Animal =
  animal match {
    case Dillo(l, w) => Dillo(Dead, w)
    case Parrot(s, w) => Parrot("", w)
  }

// A list is one of the following:
// - the empty list
// - a cons list consisting of the first element and the rest list
//                                                       ^^^^^^^^^
//                                                   self-reference

// Scala:
// empty list: Nil
// cons:       ::, can write in infix
val list1: List[Int] = 5 :: Nil // Java: List<Int>
val list2 = 5 :: 7 :: Nil
val list3 = List(5, 4, 7)
val list4 = 2 :: list3


// add the elements of a list
// template:
/*
def listSum(list: List[Int]): Int =
  list match {
    case Nil => ???
    case f :: r => 
      f ... listSum(r) 
  }
  */

def listSum(list: List[Int]): Int =
  list match {
    case Nil => 0
    case f :: r => 
      f + listSum(r) 
  }
def listProduct(list: List[Int]): Int =
  list match {
    case Nil => 1
    case f :: r =>
      f * listProduct(r)
  }

def plus(x: Int, y: Int): Int = x + y

// "right fold"
def fold[A, B](e: B, op: (A, B) => B, list: List[A]): B =
  list match {
    case Nil => e
    case f :: r =>
      op(f, fold(e, op, r))
  }

// Scala always infers types within one pair of parantheses
// simultaneously
val s1 = fold(0, {(x: Int, y: Int) => x + y}, list4)
// it infers the first parentheses first,
// then the second
// here: list4 says A must be Int
// 0 says B must be an Int
val s2 = list4.foldRight(0)(_+_)
val s2_a = list4.foldRight(0)({(x: Int, y: Int) => x +y})
val s2_b = list4.foldRight(0)({(x, y) => x +y})
val s2_c = list4.foldRight(0) {(x, y) => x +y}
val s3_d = list4.foldRight(0)(_+_)


// exercise: abstract over listSum and listProduct

val parrot1 = Parrot("Hello", 1)
val parrot2 = Parrot("Goodbye!", 2)
val highway = List(dillo1, dillo2, parrot1, parrot2)

// run over all animals in a list
def runOverAnimals(list: List[Animal]): List[Animal] =
  list match {
    case Nil => Nil
    case first :: rest =>
      runOverAnimal(first) :: runOverAnimals(rest)
  }

// "extension method"
extension (list: List[Animal])
  def runOver =
    runOverAnimals(list)

/*
sealed trait MyList[+A] {
  def runOver: MyList[Animal] =
    this match {
      case Empty => Empty
      case Cons(first, rest) =>
        Cons(runOverAnimal(first), rest.runOver)
    }
}

case object Empty extends MyList[Nothing]
case class Cons[A](first: A, rest: MyList[A]) extends MyList[A]
*/

val deadHighway = highway.runOver

def inc(x: Int): Int = x + 1

// increment all elements of a list
def listInc(list: List[Int]): List[Int] =
  list match {
    case Nil => Nil
    case first :: rest =>
      inc(first) :: listInc(rest)
  }

def map[A, B]              (f: A => B, list:  List[A]):    List[B] =
  list match {
    case Nil => Nil
    case first :: rest =>
      f(first) :: map(f, rest)
  }
def optionMap[A, B]        (f: A => B, option: Option[A]): Option[B] =
  option match {
    case None => None
    case Some(thing) => Some(f(thing))  
  }

  

// reverse the elements of list
def rev[A](list: List[A]): List[A] =
  list match {
    case Nil => Nil
    case first :: rest =>
      // list = 1 2 3 => first = 1, rest = 2 3 => rev(rest) = 3 2
      // 1 recursive call for each element of list
      // + 1 call to addElement for each element of list
      addElement(rev(rest), first)
      // recursive call has context: after the recursive call,
      // addElements needs to be called
      // needs to be stored at run time
  }

// JVM: context stored on a "stack"
// - stack is separate from heap
// - constrained in size
// => StackOverflow exception at about 10.000 open calls

// add an element to the end of the list
def addElement[A](list: List[A], element: A): List[A] =
  list match {
    case Nil => element :: Nil
    case first :: rest => 
      first :: addElement(rest, element)
  }

// run time of rev, if list has length n:
// n + (n-1) + (n-2) + ... + 3 + 2 + 1
// Gauß formula: n*(n+1)/2 = O(n^2)

// acc is the reversed list of elements we've already seen
@tailrec // compile into a loop
def rev[A](list: List[A], acc: List[A]): List[A] =
  list match {
    case Nil => acc
    case first :: rest =>
      // call to rev has *no* context: tail call
      // doesn't need to store anything on the stack
      // bug in the JVM: stores something on the stack anyway
      rev(rest, first :: acc)
  }


def revLoop[A](list0: List[A]): List[A] = {
  var list = list0
  var acc: List[A] = Nil
  while (list != Nil) {
    acc = list.head :: acc
    list = list.tail
  } 
  return acc
}

// built-in type:
/*
enum Option[A] {
  case None extends Option[A]
  case Some(value: A) extends Option[A]
}
*/

// determine the index of a list element
// listIndex(List(10,20,30,40,50), 40) => 3
def listIndex[A](list: List[A], element: A): Option[Int] =
  list match {
    case Nil => None
    case first :: rest => 
      (first == element) match {
        case true => Some(0) 
        case false => {
          /*
          val res = listIndex(rest, element)
          if (res == None)
            None
          else
            Some(res.get)
            */
            /*
          listIndex(rest, element) match {
            case None => None
            case Some(index) => Some(index+1)
          }*/
          // optionMap(inc, listIndex(rest, element))
          listIndex(rest, element).map(_+1)
        }
      }
  }

// Assoziativität
// op : (A, A) => A
// (a op b) op c = a op (b op c)
// Zutaten:
// Typ A, op : (A, A) => A
// Assoziativität
// Halbgruppe / Semigroup
// (: overlay (image image -> image))

// Haben Halbgruppe
// neutrales Element
// n: A
// n op x = x op n = x für alle x : A
// Monoid

type Price = Double

case class ShoppingItem(name: String, price: Price)

// Pattern: Typklasse
trait Semigroup[S] {
  // combine muss assoziativ sein
  extension (x: S)
     def combine(y: S): S
}

object IntAddSemigroup extends Semigroup[Int] {
  extension (x: Int) def combine(y: Int): Int = x + y
}

object IntMultSemigroup extends Semigroup[Int] {
  extension (x: Int) def combine(y: Int): Int = x * y
}

import IntAddSemigroup._
val n1 = 1.combine(5)

// Instanz der Typklasse
given [A]: Semigroup[List[A]] with {
  extension (x: List[A])
    def combine(y: List[A]): List[A] = x ++ y
}

val list7 = list4.combine(list3)

case class ShoppingCart(items: List[ShoppingItem]) {
  def combine(cart2: ShoppingCart): ShoppingCart =
    // Listen bilden auch eine Halbgruppe
    ShoppingCart(this.items ++ cart2.items) // ++ konkateniert zwei Listen
}

def combineShoppingCarts(cart1: ShoppingCart, cart2: ShoppingCart): ShoppingCart =
  ShoppingCart(cart1.items ++ cart2.items)

given Semigroup[ShoppingCart] with {
  extension (x: ShoppingCart)
    def combine(y: ShoppingCart): ShoppingCart = combineShoppingCarts(x, y)
}

val sh1 = ShoppingCart(List(ShoppingItem("Seife", 2)))
val sh2 = ShoppingCart(List(ShoppingItem("Klopapier", 2)))
val sh3 = sh1.combine(sh2)

trait Monoid[S] extends Semigroup[S] {
  // neutral.combine(x) == x.combine(neutral) == x
  def neutral: S

  def combineAll(list: List[S]): S =
    list.fold(neutral)(_.combine(_))
}

given [A]: Monoid[List[A]] with {
  def neutral = List.empty
  extension (x: List[A])
    def combine(y: List[A]): List[A] = x ++ y
}

given [S1: Monoid, S2: Monoid]: Monoid[(S1, S2)] with {
  extension (x: (S1, S2))
    def combine(y: (S1, S2)): (S1, S2) = {
      val (x1, x2) = x
      val (y1, y2) = y
      (x1.combine(y1), x2.combine(y2))
    }

  def neutral: (S1, S2) = (summon[Monoid[S1]].neutral,
                           summon[Monoid[S2]].neutral)
}


given shoppingCartMonoid: Monoid[ShoppingCart] with {
  def neutral = ShoppingCart(List.empty)
  extension (x: ShoppingCart)
    def combine(y: ShoppingCart): ShoppingCart = combineShoppingCarts(x, y)
}

val shs = List(sh1, sh2, sh3)

val shss = shoppingCartMonoid.combineAll(shs)

import scala.collection.mutable 

class ShoppingCartV1 private (val items: mutable.IndexedBuffer[ShoppingItem]) {
  def this() = this(mutable.ArrayBuffer.empty)

  def addItem(item: ShoppingItem) =
    this.items += item

  def addCart(cart: ShoppingCartV1) =
    this.items ++= cart.items

  def getItems() = this.items.clone()

  def clear() = this.items.clear()
}

// Ziel: Effekte / Interaktion mit der Außenwelt
// um Spieler:innen-Strategien zu beschreiben
// in Java: void executeStrategy(...)
// im Zusammenhang mit Testbarkeit / Dependency Injection
// FP: Monade, Spezialfall von einem Funktor "etwas mit map"
// in Scala: mit Typklassen