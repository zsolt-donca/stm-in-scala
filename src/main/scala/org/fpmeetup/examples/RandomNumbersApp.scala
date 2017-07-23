package org.fpmeetup.examples

import cats.data.State

object RandomNumbersApp extends App {

  case class SState[S, A](f: S => (S, A))

  case class Rand(long: Long) {
    // Knuth’s 64-bit linear congruential generator
    def next = Rand(long * 6364136223846793005L + 1442695040888963407L)
  }

  def nextBoolean: State[Rand, Boolean] =
    State(rand => (rand.next, rand.long >= 0L))

  def nextInt(max: Int): State[Rand, Int] =
    State(rand => (rand.next, Math.abs(rand.long.toInt) % max))

  def nextElem[A](list: List[A]): State[Rand, A] = {
    nextInt(list.size).map(index => list(index))
  }


  type Grade = Int // from 1 to 5
  case class ConferenceFeedback
  (
    name: String,
    email: String,

    organization: Grade,
    content: Grade,
    pricing: Grade,

    comeNextYear: Boolean
  )

  val names = List("Alan", "Bob", "Cecil", "Dan", "Emily")
  val emailHosts = List("gmail.com", "yahoo.com", "outlook.com")

  def genFeedback: State[Rand, ConferenceFeedback] = {
    val nextGrade = nextInt(5).map(i => i + 1)
    for {
      name <- nextElem(names)
      emailHost <- nextElem(emailHosts)
      email = name.toLowerCase + "@" + emailHost
      organization <- nextGrade
      content <- nextGrade
      pricing <- nextGrade
      comeNextYear <- nextBoolean
    } yield ConferenceFeedback(name, email, organization, content, pricing, comeNextYear)
  }

  val rand = Rand(7)
  val (newRand, feedback) = genFeedback.run(rand).value
  println(feedback) // ConferenceFeedback(Cecil,cecil@yahoo.com,5,1,5,true))
}
