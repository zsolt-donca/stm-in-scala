package org.fpmeetup.examples

object BrewingCoffee {

  // Recipe: How To Make Coffee, in just three easy steps
  //
  // step 1. Take some coffee beans out of the jar
  // step 2. Grind the beans
  // step 3. Brew the ground coffee using a Moka pot
  //
  // Watch out for cases when:
  //   - you run ouf of coffee beans
  //   - your grinder is stuck
  //   - your Moka pot explodes
 
  case class Coffee()
  case class CoffeeBeans()
  case class GroundCoffee()

  sealed trait CoffeeMakingError
  case class NoMoreBeans() extends CoffeeMakingError
  case class GrinderStuck() extends CoffeeMakingError
  case class MokaExploded() extends CoffeeMakingError

  def takeCoffeeBeans: Either[NoMoreBeans, CoffeeBeans] = ???

  def grindBeans(coffeeBeans: CoffeeBeans): Either[GrinderStuck, GroundCoffee] = ???

  def useMokaPot(groundCoffee: GroundCoffee): Either[MokaExploded, Coffee] = ???

  // combines all of the above
  def makeCoffee: Either[CoffeeMakingError, Coffee] = ???

  // simplest solution: just use pattern matching
  def makeCoffee1: Either[CoffeeMakingError, Coffee] = {
    takeCoffeeBeans match {
      case Right(coffeeBeans) =>
        grindBeans(coffeeBeans) match {
          case Right(groundCoffee) =>
            useMokaPot(groundCoffee) match {
              case Right(coffee) => Right(coffee)
              case Left(reason)  => Left(reason)
            }
          case Left(reason) => Left(reason)
        }
      case Left(reason) => Left(reason)
    }
  }

  // let's introduce flatMap to eliminate redundancy above
  def flatMap[E, A, B](either: Either[E, A], f: A => Either[E, B]): Either[E, B] = {
    either match {
      case Right(a) => f(a)
      case Left(e)  => Left(e)
    }
  }

  // making coffee using the flatMap above - note the type parameters of the lambdas
  def makeCoffee2: Either[CoffeeMakingError, Coffee] = {
    flatMap(takeCoffeeBeans, (coffeeBeans: CoffeeBeans) =>
      flatMap(grindBeans(coffeeBeans), (groundCoffee: GroundCoffee) =>
        useMokaPot(groundCoffee)
      )
    )
  }

  // making coffee using the built-in flatMap (works with Scala 2.12, or with previous versions using cats implicits)
  // the structure is nested: "pyramid of doom", or "callback hell"
  def makeCoffee3: Either[CoffeeMakingError, Coffee] = {
    takeCoffeeBeans.flatMap(coffeeBeans =>
      grindBeans(coffeeBeans).flatMap(groundCoffee =>
        useMokaPot(groundCoffee)
      )
    )
  }

  // making coffee using a for comprehension - the structure is flat instead of nested
  def makeCoffee4: Either[CoffeeMakingError, Coffee] = {
    for {
      coffeeBeans  <- takeCoffeeBeans          // either returns with NoMoreBeans error or continues
      groundCoffee <- grindBeans(coffeeBeans)  // either returns with GrinderStuck error or continues
      coffee       <- useMokaPot(groundCoffee) // either returns with MokaExploded error or continues
    } yield coffee                             // if reached, just returns the coffee
  }
}
