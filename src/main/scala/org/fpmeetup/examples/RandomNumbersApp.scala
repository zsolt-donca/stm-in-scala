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
    val name = names(rnd.nextInt(names.size))
    val email = name.toLowerCase + "@" + emailHosts(rnd.nextInt(emailHosts.size))
    val organization = rnd.nextInt(5) + 1
    val content = rnd.nextInt(5) + 1
    val pricing = rnd.nextInt(5) + 1
    val comeNextYear = rnd.nextBoolean()

    ConferenceFeedback(name, email, organization, content, pricing, comeNextYear)
  }

  val rnd = new Random(42)
  println(genFeedback(rnd))
}
