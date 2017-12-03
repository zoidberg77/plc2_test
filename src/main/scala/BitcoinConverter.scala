import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object BitcoinConverter {
  def props(printerActor: ActorRef): Props = Props(new BitcoinConverter(printerActor))
  case class bitcoin2euro(bitcoin: BigDecimal)
  case class bitcoin2dollar(bitcoin: BigDecimal)
}

class BitcoinConverter(printerActor: ActorRef) extends Actor with ActorLogging {
  import BitcoinConverter._
  import Printer._

  def receive = {
    case bitcoin2euro(bitcoin) =>
      printerActor ! PrintCurrency (bitcoin*5551)
      //printerActor ! Print(" Euro")

    case bitcoin2dollar(bitcoin) =>
      printerActor ! PrintCurrency (bitcoin*6500)
      //printerActor ! Print(" Dollar")
  }
}