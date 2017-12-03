import EuroConverter._
import BitcoinConverter._
import DollarConverter._
import Printer._
import akka.actor.{ActorRef, ActorSystem, Props}

// test verschiedneer konzepte
// case class

case class Pocket(money: Double, size: Int) // wie ein struct

object MainApp extends App {

  val system: ActorSystem = ActorSystem("mainAkka")

  try {
    val printer: ActorRef = system.actorOf(Printer.props, "printerActor")
    val ec: ActorRef = system.actorOf(EuroConverter.props(printer), "euroConverter")
    val bc: ActorRef = system.actorOf(BitcoinConverter.props(printer), "bitcoinConverter")
    val dc: ActorRef = system.actorOf(DollarConverter.props(printer), "dollarConverter")

    val bitcoin = BigDecimal("1")
    val dollar = BigDecimal("200")
    val euro = BigDecimal("300")

    bc ! bitcoin2dollar(bitcoin)
    bc ! bitcoin2euro(bitcoin)
    dc ! dollar2euro(dollar)
    dc ! dollar2bitcoin(dollar)
    ec ! euro2dollar(euro)
    ec ! euro2bitcoin(euro)

    //TEST CASE CLASS
    val p1 = Pocket(123.5, 5)
    val p2 = Pocket(234.5, 6)
    var p3 = p1
    val p4 = p1.copy(money = 0) //copy lets u modify values during copying process

    println(p1.money)
    println(p2.money)
    println(p3.money)
    println(p4.money)

    //TEST OBJECT
    object TestAddition{
      val test =  12
      val test2 : Int = test *  2 //so geht das mit den type annotations

      def function (value1: Int, value2: Int) : Int = {
        println(test2)
        value1 + value2
      }
    }

    println(TestAddition.function(2, 5))

    //TEST HIGHER ORDER FUNCTION UND ANONYMOUS FUNCTION

    def differentAddition( f: (Int, Int) => Int, a : Int, b: Int ) : Int = {
      f(a,b)
    }

    def sum(f: Int=>Int, a: Int, b: Int): Int =
      if (a == b) f(a) else f(a) + sum(f, a + 1, b)

    println(sum( (x:Int) => x+1, 2, 5))

    println(differentAddition(TestAddition.function, 23, 24))


    //TEST PATTERN MATCHING
    def errorMsg(errorCode: Int) = errorCode match {
      case 1 => "File not found"
      case 2 => "Permission denied"
      case 3 => "Invalid operation"
    }
    println(errorMsg(2))


    abstract class Notification

    case class Email(sender: String, title: String, body: String) extends Notification

    case class SMS(caller: String, message: String) extends Notification

    case class VoiceRecording(contactName: String, link: String) extends Notification


    def showNotification(notification: Notification): String = {
      notification match {
        case Email(email, title, _) =>
          s"You got an email from $email with title: $title"
        case SMS(number, message) =>
          s"You got an SMS from $number! Message: $message"
        case VoiceRecording(name, link) =>
          s"you received a Voice Recording from $name! Click the link to hear it: $link"
      }
    }
    val someSms = SMS("12345", "Are you there?")
    val someVoiceRecording = VoiceRecording("Tom", "voicerecording.org/id/123")

    println(showNotification(someSms))  // prints You got an SMS from 12345! Message: Are you there?

    println(showNotification(someVoiceRecording))  // you received a Voice Recording from Tom! Click the link to hear it: voicerecording.org/id/123

    //TEST RECURSION
    def sum1(n: Int): Int =
      if (n == 0) 0 else n + sum1(n - 1)
    val m = sum1(10)
    println(m)

    //Tail recursion - scala capable of tail call optimization (tco)
    def sum2(n: Int, acc: Int):Int =
      if(n == 0) acc else sum2(n - 1, acc + n)
    val r = sum2(10000, 0)
    println(r)

    //CONTAINER CLASSES
    val a = List(1,2,3,4,5,6,7)
    val b = a.map(x => x * x)
    val c = a.filter(x => x < 5)
    val d = a.reduce((x, y)=>x+y)
    println(b)
    println(c)
    println(d)

    def even(x: Int) = (x % 2) == 0
    val e = List(2, 4, 6, 5, 10, 11, 13, 12)
    // are all members even?
    println(a.forall(even))
    // is there an even element in the sequence?
    println(a.exists(even))
    //take while the element is even -
    //stop at the first odd element
    println(e.takeWhile(even))
    //partition into two sublists: even and odd
    println(a.partition(even))

    //TEST NESTED FUNCTIONS AND FUNCTIONS RETURNING FUNCTIONS
    def fun():Int => Int = {
      def sqr(x: Int):Int = x * x
      sqr
    }
    val f = fun()
    println(f(10))



    //TEST LEXICAL CLOSURE
    def weirdAddition (x : Int, y : Int) : Int => Int = {
      val z = 5
      def retFunction (a : Int) = z + x + y + a
      retFunction
    }
    var func1 = weirdAddition(5 ,10)
    println(func1(7))

    //TEST LEXICAL CLOSURE WITH ANON FUNCTION
    def evenMoreWeirdAddition(tau : Int) : Int => Int = x => x + tau
    func1 = evenMoreWeirdAddition(5)
    println(func1(7))

    //TEST LEXICAL CLOSURE WITH FURTHER FANCY STUFF
    def sqr(x: Int) =  x*x
    def cube(x: Int) =  x*x*x
    def compose(f: Int=>Int, g: Int=>Int): Int=>Int =
      x => f(g(x))
    val psi = compose(sqr, cube)
    println(psi(2))
    println(a.map(psi))
    println(a.map(cube).map(sqr))


    def removeLowScores(a: List[Int], threshold: Int): List[Int] =
      a.filter(score => score >= threshold)
    val v = List(95, 87, 20, 45, 35, 66, 10, 15)
    println(removeLowScores(v, 30))


    var count = 0
    def third(x: Int) = {
      var bool = count % 2 == 0
      count+=1
      bool
    }
    println(v.filter(third))

    def addA(x: Int, y: Int, z: Int)=
      x+y+z

    def addB(x: Int): Int=>(Int=> Int) = {
      y => z => x+y+z
    }

    val lorenz = addA(6,6,6)
    val dumm = addB(1)(1)(1)

    println(lorenz)
    println(dumm)







  }

  finally {
    system.terminate()
  }
}