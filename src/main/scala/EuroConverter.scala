import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object EuroConverter {
  def props(printerActor: ActorRef): Props = Props(new EuroConverter(printerActor))
  case class euro2bitcoin(euro: BigDecimal)
  case class euro2dollar(euro: BigDecimal)
}

class EuroConverter(printerActor: ActorRef) extends Actor with ActorLogging {
  import EuroConverter._
  import Printer._

  def receive = {
    case euro2bitcoin(euro) =>
      printerActor ! PrintCurrency (euro/5551)
      //printerActor ! Print(" Bitcoin")

    case euro2dollar(euro) =>
      printerActor ! PrintCurrency (1.18*euro)
      //printerActor ! Print(" Dollar")
  }

}