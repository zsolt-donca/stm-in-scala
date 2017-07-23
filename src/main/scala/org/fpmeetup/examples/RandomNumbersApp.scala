package org.fpmeetup.examples

import scala.util.Random

object RandomNumbersApp extends App {

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

  def genFeedback(rnd: Random): ConferenceFeedback = {
    def nextElem[A](list: List[A]): A = list(rnd.nextInt(list.size))
    val grade = rnd.nextInt(5) + 1

    val name = nextElem(names)
    val email = name.toLowerCase + "@" + nextElem(emailHosts)
    val organization = grade
    val content = grade
    val pricing = grade
    val comeNextYear = rnd.nextBoolean()

    ConferenceFeedback(name, email, organization, content, pricing, comeNextYear)
  }

  val rnd = new Random(42)
  println(genFeedback(rnd)) // ConferenceFeedback(Dan,dan@gmail.com,1,1,1,false)
}
