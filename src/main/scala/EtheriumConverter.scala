import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object EtheriumConverter {
  def props(printerActor: ActorRef): Props = Props(new EtheriumConverter(printerActor))
  case class etherium2dollar(etherium: BigDecimal)
  case class etherium2euro(etherium: BigDecimal)
  case class etherium2bitcoin(etherium: BigDecimal)
}

class EtheriumConverter(printerActor: ActorRef) extends Actor with ActorLogging {
  import EtheriumConverter._
  import Printer._

  def receive = {
    case etherium2dollar(etherium) =>
      printerActor ! PrintCurrency (etherium*400)
    //printerActor ! Print(" Bitcoin")

    case etherium2euro(etherium) =>
      printerActor ! PrintCurrency (etherium*300)
    //printerActor ! Print(" Euro")

    case etherium2bitcoin(etherium) =>
      printerActor ! PrintCurrency (etherium*0.036)
    //printerActor ! Print(" Euro")
  }


}