import java.lang.{Exception, RuntimeException}

import EuroConverter._
import BitcoinConverter._
import DollarConverter._
import Printer._
import akka.actor.{ActorRef, ActorSystem, Props}

import scala.collection.mutable.ListBuffer

//REPETITION OF SCALA CONCEPTS

//PATTERN MATCHING, INHERITANCE, CASE CLASSES AND TRAITS BEGIN
sealed abstract class Notification
trait msgTrait1 {
  final def printMsg : Unit = {
    this match {
      case messageType1 : messageType1 => print("The Notification contains the following Mesage: " + messageType1.msg1 + "\n")
      case messageType2: messageType2 => print("The Notification contains the following Mesage: " + messageType2.msg1 + " " + messageType2.msg2 + "\n")
    }
  }
}
case class messageType1(msg1 : String) extends Notification with msgTrait1
case class messageType2(msg1 : String, msg2 : String) extends Notification with msgTrait1
//PATTERN MATCHING AND TRAITS END

//SINGLETON/COMPANION OBJECT BEGIN
object Pocket {
  final val defaultMsg = "comanion name"
  final def companionMethod() : String = {
    defaultMsg
  }
}
//SINGLETON/COMPANION OBJECT END

//CONSTRUCTORS BEGIN
class Pocket (val name : String, var size : Int, var value : Double){
  import Pocket._

  def this() {
    this(Pocket.defaultMsg, 2, 0)
  }
  def this (name : String) {
    this(name, 5, 0)
  }
  def this (name : String, value : Int) {
    this(name, value, 0)
  }
  override def toString(): String = {
    "The pocket " + name + " has size " + s"$size" + " and value " + s"$value"
  }
}
//CONSTRUCTORS END


object MainApp extends App {
  val system: ActorSystem = ActorSystem("mainAkka")
  try{
    println("PATTERN MATCHING, INHERITANCE, CASE CLASSES AND TRAITS BEGIN")
    val email = messageType1("email")
    val sms = messageType2("SMS", "0664 123 456 7")
    email.printMsg
    sms.printMsg
    val sms2 = sms.copy("SMS2")
    sms2.printMsg
    println("PATTERN MATCHING, INHERITANCE, CASE CLASSES AND TRAITS END\n")
    println("HIGHER ORDER FUNCTIONS BEGIN")
    def concatenation (mode : Int, string3 : String) : (String, String) => String  = {
      mode match {
        case 1 => {
          def concat1 (string1: String, string2: String): String = {
            "concat mode 1: " + string1 + " " + string2 + " " + string3 +  "\n"
          }
          concat1
        }
        case 2 =>
        {
          def concat2(string1: String, string2: String): String = {
            "concat mode 2: " + string1 + " " + string3 + " " + string2 +  "\n"
          }
          concat2
        }
        case _ =>  {
          def concat_(string1: String, string2: String): String = {
            "concat mode _: " + string3 + " " + string2 + " " + string1 +  "\n"
          }
          concat_
        }
      }
    }

    print(concatenation(1, "outerarg").apply("innerarg1", "innerarg2"))
    println("HIGHER ORDER FUNCTIONS END\n")
    println("CURRYING FUNCTIONS BEGIN")
    def firstLetters (s1 : String, s2: String) :  String = {
      s1.substring(0,1) + s2.substring(0,1)
    }
    println(firstLetters("abc","def"))

    def firstLettersCurry(s1 : String) : String => String = {
      def flFunc (s2 : String) : String = { s1.substring(0,1) + s2.substring(0,1) }
      flFunc
    }
    println(firstLettersCurry("abc")("def"))
    println("CURRYING FUNCTIONS END\n")

    println("CONSTRUCTORS BEGIN")
    val p1 = new Pocket("p1")
    val p2 = new Pocket("p2", 10)
    val p3 = new Pocket("p3", 15, 100)
    println(p1); println(p2); println(p3)
    println("CONSTRUCTORS END\n")

    println("LEXIAL CLOSURE, NESTED FUNCTIONS, ANONYMOUS FUNCTIONS BEGIN")
    def calcWithAnonFunction(lop : Int, rop : Int, f : (Int, Int) => Int ) : Int = {
      f(lop, rop)
    }
    println(calcWithAnonFunction(3,4, (v1 : Int, v2 : Int) => v2 * v1))
    println(calcWithAnonFunction(3,4, (v1 : Int, v2 : Int) => v2 + v1))
    def calcWithAnonFunctionAndLexicalCLosure() : (Int, Int) => Int = {
      val y = 10
      (lop, rop) => (lop + rop) * y
    }
    println(calcWithAnonFunctionAndLexicalCLosure()(3,4))
    var someFunction = calcWithAnonFunctionAndLexicalCLosure()
    println(someFunction(3,4))
    println("LEXIAL CLOSURE, NESTED FUNCTIONS, ANONYMOUS FUNCTIONS END\n")

    println("MUTABLE AND IMMUTABLE CONTAINERS BEGIN")
    var cnt = 0
    def everyThird(x : Int) : Boolean = {
      if(cnt % 2 == 0) { cnt +=1; true }
      else { cnt += 1; false }

    }
    val list1 = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
    val list1Filtered = list1.filter( x => x % 2 != 0)
    println(list1Filtered)
    println(list1.filter( x => x % 3 != 0))
    println(list1.filter(everyThird))

    println(List(p1,p2,p3).sortBy(_.value))
    println(List(p1,p2,p3).sortWith( (lop : Pocket, rop : Pocket) => lop.size > rop.size ))

    println(List(p1,p2,p3).+:(new Pocket()))
    println(List(p1,p2,p3).foreach(p => println(p)))

    println(1::list1Filtered)
    var vector1 = (1::list1Filtered).toVector
    println(vector1.count(x => x > 1))
    println(vector1)

    var list2 = scala.collection.mutable.ListBuffer(2,3)
    list2.+=(13)
    println(list2)

    var list3 = list1.takeWhile(x => x%2 !=0)
    var list4 = list1.to[ListBuffer]
    println("MUTABLE AND IMMUTABLE CONTAINERS END\n")

    //lazy vals
    println("SINGLETON/COMPANION OBJECT BEGIN")
    print(Pocket.companionMethod())
    println("SINGLETON/COMPANION OBJECT END\n")

    println("AKKA BEGIN")
    val printer: ActorRef = system.actorOf(Printer.props, "printerActor")
    val ec: ActorRef = system.actorOf(EuroConverter.props(printer), "euroConverter")
    val bc: ActorRef = system.actorOf(BitcoinConverter.props(printer), "bitcoinConverter")
    val dc: ActorRef = system.actorOf(DollarConverter.props(printer), "dollarConverter")
    val tc: ActorRef = system.actorOf(DollarConverter.props(printer), "etheriumConverter")


    val bitcoin = BigDecimal("1")
    val dollar = BigDecimal("200")
    val euro = BigDecimal("300")

    printer ! Print("wtest")

    bc ! bitcoin2dollar(bitcoin)
    bc ! bitcoin2euro(bitcoin)
    dc ! dollar2euro(dollar)
    dc ! dollar2bitcoin(dollar)
    ec ! euro2dollar(euro)
    ec ! euro2bitcoin(euro)
    tc ! euro2etherium(euro)
    tc ! bitcoin2etherium(bitcoin)

    println("AKKA END\n")
  }
  catch  {
    case ex : Exception => {
      println(ex.getMessage)
    }
  }
  finally {
    system.terminate()
  }
}

// TEST KONSTRUKTOR
/*
class Test(val value1 : Int, value2 : Int, var variable2 : Int){
  //konstruktor ist hier implizit -
  // beim callen var werte übergeben oder er nmmt defaults
  variable2 = 1
  var variable1 = 0
  override def toString: String = variable1+variable2 toString
}

//TEST OF TRAITS and MORE PATTERN AMTCHHING

sealed abstract class Form //sealed class: wie final, aber darf im selben src file überschrieben werden
case class Kreis(radius : Int)
case class Rechtreck(hoehe : Int, breite : Int)

trait Testtrait { //sowas wie ein interface
  def calcsomething(aa : Int, bb : Int) : Int  = {
    aa * bb
  }
  def calcnothing(aa : Int, bb : Int) : Int
}

object Kreiseck extends Kreis(1) with Testtrait {
  val hoehe = radius * 5
  val breite = calcsomething(hoehe, radius)
  override def calcnothing(aa : Int, bb : Int) : Int = {
    aa+bb
  }
}

//TESTING VALS AND VARS
class ChecksumAccumulator {
  var sum = 0
}

// TESTING ACCESS MODIFIERS - Everything is implicitly public if not declared elsewise
class ChecksumAccumulator2 {
  private var sum = 0
  protected def add(b: Byte): Unit = sum += b
  def checksum(): Int = ~( sum & 0xFF) + 1
}

// CONTAINERS IMMUTABLE BY DEFAULT - USE THOSE FOR MUTABLE CONTAINERS
import scala.collection.mutable


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

    printer ! Print("wtest")

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
    var av = ListBuffer(0,5)
    av.append(7)
    av.append(71)
    av.prepend(8)
    println("\n")
    println(av)
    println("\n")

    val avl = av.filter(x => x < 7).toList

    println(avl)
    println("\n")

    println(avl.map(x => x + 1000))
    println("\n")

    println(avl.reduce((x,y) => x+y))
    println("\n")

    for( a <- 1 to 10){ av.append(a) }
    val avl2 =  av.toList
    println(avl2.sortBy( x => x )) //sortiert nach attribut. man kann auch einfach nur zB .age einwerfen
    println("\n")

    avl2.foreach( x => print(x)) //man darf das x aber nicht verändern
    println("\n")

    println(avl2.sortWith((x,y) => x>y))
    println("\n")

    def uneven (x : Int) : Boolean = { x%2 != 0 }

    println()
    println(avl2.forall(uneven)) // are all elements uneven?
    println(avl2.exists(uneven)) // is there even one uneven elem?
    println(avl2.takeWhile(uneven)) // take until you find an uneven elen
    println(avl2.dropWhile(uneven)) // dont know what it does - lets see
    println("\n")

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


    //TESTING CURRYING


    def addA(x: Int, y: Int, z: Int)=
      x+y+z

    def addB(x: Int): Int=>(Int=> Int) = {
      y => z => x+y+z
    }

    val p = addA(6,6,6)
    val q = addB(1)(1)(1)

    println(p)
    println(q)


    //TESTING FUN WITH LISTS(Not to confuse with"Fun with Flags")

    var list1 = List(1,2,3,4)
    var list2 = List(5)
    var list3 = List(100)
    var list4 = 0::list1
    list4+:list2

    println(list4)
    //Something went wrong here


    //TESTING LISTBUFFER

    var fruits = new ListBuffer[String]()

    fruits += "Apple"
    fruits += "Banana"
    fruits += "(V)(;,,;)(V)"
    fruits -= "Apple"
    println(fruits(0))

    println(fruits)


    //FUN WITH PATTERNMATCHING
    def funPatterMatching(list :List[Int]) = list match {
      case List(0, p, q) => p + q
      case _ => -1
    }

    println(funPatterMatching(List(0,1,2)))
    println(funPatterMatching(List(1,1,2)))


    //FUN WITH ALGEBRAIC DATATYPES

    sealed abstract class Shape
    case class Circle(r: Int) extends Shape
    case class Rectangle(h: Int, w: Int) extends Shape


    val sc:Shape = Circle(10)
    val sr:Shape = Rectangle(5,5)


    def area(a: Shape) = a match {
      case Circle(r) => math.Pi*r*r
      case Rectangle(h,w) => (h*w) toDouble
    }

    println(area(sc))
    println(area(sr))

    // TESTING LAZY VALS

    // Lazy vals are only evaluated when used somewhere ind the code

    def hello() = {
      println("hello")
      10
    }
    lazy val testlazy1 = hello()
    println(testlazy1 + testlazy1 + testlazy1)

    // TESTING PURE FUNCTIIONS
    // PURE FUNCTIONS -> REFERENTIALLY TRANSPARANT -> always give same result for same input
    // FUNCTIONAL PROGRAMMING -> focus on using pure functions. inefficient, so we use mostly pure
    // and some impure outer layers for communication with "outside world".

    case class Player(name: String, score: Int)

    def declareWinner(p: Player) =
      println(p.name + " is the winner!! ")

    def maxScore(p1: Player, p2: Player) =
      if (p1.score > p2.score) p1 else p2

    def winner(p1: Player, p2: Player) =
      declareWinner(maxScore(p1, p2))

    winner(Player("Ram", 10), Player("John", 20))

    //TESTING VALS AND VARS - SEE TOP OF FILE FOR CLASS

    val acc = new ChecksumAccumulator()
    val csa = new ChecksumAccumulator()
    acc.sum = 3

    println(acc.sum + " " + csa.sum)

    //TESTING CHARACTER LITERALS

    println("Welcome to Programming Languages and Concepts 2017 at the" +
      "University of Vienna") // this could caus problems in REPL?

    //STRING INTERPOLATION
    val name = "reader"
    println(s"Hello, $name!")

    //OPERATORS ARE METHODES
    //val sum = 1 + 2
    //is same as
    //val sum1 = 1.+(2)

    //EQUALITY: == and != applay to all objects in scala


    //TESTING SOME LIST OPERATIONS
    val newv = v.map(x => x + 1)

    println(newv)
    println(v)

    var newBufferedList = ListBuffer(95, 87, 20, 45, 35, 66, 10, 15)
    newBufferedList.foreach (x => x + 1)
    println(newBufferedList)
    //this doesnt work properly

  }

  finally {
    system.terminate()
  }
}*/