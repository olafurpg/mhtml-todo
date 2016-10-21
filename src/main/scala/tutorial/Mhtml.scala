package tutorial

import scala.scalajs.js.JSApp
import mhtml._

object Mhtml extends JSApp {
  def main(): Unit = {
    val x = Var(1)
    val y = Var(1)
    val z = for {
      xx <- x
      yy <- y.map(_ + xx)
    } yield xx + yy
    z.foreach(println)
    println(z)
    x := 7
    println(z)
    z.foreach(println)
  }
}
