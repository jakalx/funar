package funar

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

val dillo1 = Dillo(Liveness.Alive, 10)
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

def fold[A, B](e: B, op: (A, B) => B, list: List[A]): B =
  list match {
    case Nil => e
    case f :: r =>
      op(f, fold(e, op, r))
  }

val s1 = fold(0, {(x: Int, y: Int) => x + y}, list4)

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

extension (list: List[Animal])
  def runOver =
    runOverAnimals(list)

def inc(x: Int): Int = x + 1

// increment all elements of a list
def listInc(list: List[Int]): List[Int] =
  list match {
    case Nil => Nil
    case first :: rest =>
      inc(first) :: listInc(rest)
  }

def map[A, B](f: A => B, list: List[A]): List[B] =
  list match {
    case Nil => Nil
    case first :: rest =>
      f(first) :: map(f, rest)
  }

