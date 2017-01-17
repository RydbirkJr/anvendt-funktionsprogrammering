package controllers

import play.api.mvc._

import scala.util.Random

class Application extends Controller {

  val random = new Random()

  def index = Action {
    Thread.sleep(2000)
    val numberOfHeaps = Math.max(random.nextInt(10), 2)
    val result = (0 to numberOfHeaps).foldLeft("") { (s, _) =>
      val matches = Math.max(random.nextInt(15), 1)
      s"$s$matches,"
    }.dropRight(1)

    Ok(result)
  }

}
