import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object DollarConverter {
  def props(printerActor: ActorRef): Props = Props(new DollarConverter(printerActor))
  case class dollar2bitcoin(dollar: BigDecimal)
  case class dollar2euro(dollar: BigDecimal)
  case class dollar2etherium(dollar: BigDecimal)
}

class DollarConverter(printerActor: ActorRef) extends Actor with ActorLogging {
  import DollarConverter._
  import Printer._

  def receive = {
    case dollar2bitcoin(dollar) =>
      printerActor ! PrintCurrency (dollar/6500)
      //printerActor ! Print(" Bitcoin")

    case dollar2euro(dollar) =>
      printerActor ! PrintCurrency (dollar*0.85)
      //printerActor ! Print(" Euro")

    case dollar2etherium(dollar) =>
      printerActor ! PrintCurrency (dollar/400)
    //printerActor ! Print(" Euro")
  }


}