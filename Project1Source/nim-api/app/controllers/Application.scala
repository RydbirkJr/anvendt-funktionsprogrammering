package controllers

import play.api.mvc._

import scala.util.Random

class Application extends Controller {

  val random = new Random()

  def index = Action {
    val numberOfHeaps = Math.max(random.nextInt(11), 2)
    val result = (0 to numberOfHeaps).foldLeft("") { (s, _) =>
      val matches = Math.max(random.nextInt(51), 1)
      s"$s$matches,"
    }.dropRight(1)

    Ok(result)
  }

}
