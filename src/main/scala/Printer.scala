import akka.actor.{Actor, ActorLogging, Props}

object Printer {
  def props: Props = Props[Printer]
  final case class Print(message: String)
  final case class PrintCurrency(currency: BigDecimal)
}

class Printer extends Actor with ActorLogging {
import Printer._ //gets us object printer

  def receive = this.synchronized {
    case Print(message) => println(s"$message")
    case PrintCurrency(currency) => println(s"$currency")
  }
}
