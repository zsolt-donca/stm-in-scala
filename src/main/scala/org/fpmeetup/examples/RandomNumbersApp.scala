package org.fpmeetup.examples

object RandomNumbersApp extends App {

  case class Rand(long: Long) {
    // Knuth’s 64-bit linear congruential generator
    def next = Rand(long * 6364136223846793005L + 1442695040888963407L)
  }

  def nextBoolean(rand: Rand): (Rand, Boolean) =
    (rand.next, rand.long >= 0L)

  def nextInt(rand: Rand, max: Int): (Rand, Int) =
    // not perfectly uniform, but will do the trick
    (rand.next, Math.abs(rand.long.toInt) % max)

  def nextElem[A](rand: Rand, list: List[A]): (Rand, A) = {
    val (newRand, index) = nextInt(rand, list.size)
    (newRand, list(index))
  }

  def nextGrade(rand: Rand): (Rand, Grade) = {
    val (newRand, grade) = nextInt(rand, 5)
    (newRand, grade + 1)
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

  def genFeedback(rand: Rand): (Rand, ConferenceFeedback) = {
    val (rand1, name) = nextElem(rand, names)
    val (rand2, emailHost) = nextElem(rand1, emailHosts)
    val email = name.toLowerCase + "@" + emailHost
    val (rand3, organization) = nextGrade(rand2)
    val (rand4, content) = nextGrade(rand3)
    val (rand5, pricing) = nextGrade(rand4)

    val (rand6, comeNextYear) = nextBoolean(rand5)

    (rand6, ConferenceFeedback(name, email, organization, content, pricing, comeNextYear))
  }

  val rand = Rand(7)
  val (newRand, feedback) = genFeedback(rand)
  println(feedback) // ConferenceFeedback(Cecil,cecil@yahoo.com,5,1,5,true))
}
