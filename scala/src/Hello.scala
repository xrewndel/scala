
import javax.xml.bind.DatatypeConverter
import scala.collection.generic.{SeqFactory, GenSeqFactory}
import scala.io.Source
import scala.Some
import scala.xml._
import Params.ValueSet
import scala.xml.{TopScope, Elem}

object Hello extends App {
  //args.foreach(println)

  //val a = args.mkString(" ").split(Array(',','.')).toList map { _.trim }
  //println(a)

  //val (txt, start :: end :: len :: Nil)  = a.splitAt(a.length-3) match {
  //  case (a, b) => (a, b map (_.toInt))
  //}

  //println(bound)
  //for( i<- bound._2(0).toInt until bound._2(1).toInt ) println(bound._1(i))

  //for( i<- a.splitAt(a.length-2)._2(0).toInt until a.splitAt(a.length-2)._2(1).toInt ) println(a.splitAt(a.length-2)._1(i))

  //val res = txt filter { _.length >= len } slice (start, end)
  //println(res)


  // template of params for all requests
  /*val template = Map(
    "terminalId" -> "1",
    "cashierId" -> "2",
    "sessionId" -> "3"
  )           */

  val template = Map(
    Params.cashier -> Params.cashier,
    Params.session -> Params.session
  )

  //println(createXML2("tag", template))

  val map = Map("unum" -> "daya", "duobus" -> "biyu", "tribus" -> "uku")

  val xml1 = createXML("tag", template)
  //println("XML1: " + xml1)
  val xml2 = createXML("root", map)
  //println("XML2: " + xml2)
  val sss = xml1 +: (<ddd>{xml2}</ddd>)       // correct
  //println("SSS: " + sss)
  val sss2 = xml1 ++ {xml2}
  //println("SSS2: " + sss2)


  def createXML(root: String, params: Map[String, String]): xml.Elem = {
    params.foldLeft(<root/>.copy(label = root)) {
      case (acc, (k, v)) => acc.copy(child = acc.child :+ <tag>{v}</tag>.copy(label=k))
    }
  }

  implicit def map2xml(map: Map[String, String]) = new {
    def toXML(root: String = "root") = createXML(root, map)
  }

  private var _id = -1
  def shiftID = { _id = (_id + 1) % 99; _id + 1 }
  //println(0 to 100 foreach { _ => println(shiftID) } )

  //println(createXML2("root", map)) // res10: scala.xml.Elem = <radix><unum>daya</unum><duobus>biyu</duobus><tribus>uku</tribus></radix>
  //println(map.toXML("tushen")) // res11: scala.xml.Elem = <tushen><unum>daya</unum><duobus>biyu</duobus><tribus>uku</tribus></tushen>


  val x = <root><unum>1,2,3</unum></root>
  println(x)

  /*case class PincodeRecord(pincodeSerial: String, pincode1: String, pincode2: String = "", expiredDate: String)
  object PincodeRecord {
    def apply(response: xml.Elem): PincodeRecord = {
      (response \\ "pincodes").text.split(",").toList match {
        case List(a, b, c) => PincodeRecord(a, b, expiredDate = c)
        case List(a, b, c, d) => PincodeRecord(a, b, c, d)

        case l if(l.size == 3) => PincodeRecord(l(0), l(1), expiredDate = l(2))
        case l if(l.size == 4) => PincodeRecord(l(0), l(1), l(2), l(3))

        case a :: b :: c :: Nil => PincodeRecord(a, b,  expiredDate = c)
        case a :: b :: c :: d :: Nil => PincodeRecord(a, b, d, c)

        case l @ (_ :: _ :: _ :: _ :: _) => PincodeRecord(l(0), l(1), l(2), l(3))
        case l @ (_ :: _ :: _ :: _ ) => PincodeRecord(l(0), l(1),  expiredDate = l(3))
      }
    }
  }  */

  case class PincodeRecord(pincodeSerial: String, pincode1: String, pincode2: String = "", expiredDate: String)
  object PincodeRecord {
    def apply(response: Map[String, String]): PincodeRecord = {
      response.get(Params.pincodes).get.split(",").toList match {
        case List(a, b, c) => PincodeRecord(a, b, expiredDate = c)
        case List(a, b, c, d) => PincodeRecord(a, b, c, d)
      }
    }
  }

  val m = Map(Params.pincodes -> "q,w,e,r")
  println(PincodeRecord(m))

  val cr = Map(Params.amount -> "1234", Params.pincodes -> "q,w,e,r"/*, Params.ticket -> "adsfgsdfhsdfhsdfhdfs"*/)
  case class ConfirmResult(pincodes: List[PincodeRecord], amount: Int, originalLoyaltyCardTotalPoint: Int = 0,
                           newLoyaltyCardPoint: Int = 0, loyaltyCardTotalPoint: Int = 0, ticket: String = "")
  object ConfirmResult {
    def apply(response: Map[String, String]): ConfirmResult = {
      def %(name: String): Int = response.get(name).get match {
        case "" => 0 //  It can return “” instead of zero (from API Documentation)
        case num => num.toInt
      }

      def %%(name: String) = response.get(name) match { case Some(v) => v; case None => "" }

      ConfirmResult(List(PincodeRecord(response)), %(Params.amount), ticket = %%(Params.ticket))
    }
  }

  println(ConfirmResult(cr))


  println(Reserve(Map(Params.ref -> "12345"/*, Params.loyalty -> ""*/)))

  case class Reserve(referenceNumber: Int, loyaltyCardTotalPoints: Int = 0)
  object Reserve {
    def apply(response: Map[String, String]): Reserve = {
      def %%(name: String) = response.get(name) match { case Some(v) => v; case None => "" }

      Reserve(%%(Params.ref).toInt, %%(Params.loyalty) match {
          case "" => 0 //  It can return “” instead of zero (from API Documentation)
          case number => number.toInt
        }
      )
    }
  }

  def apply(response: Map[String, String]): ConfirmResult = {
    def %(name: String): Int = response.get(name).get match {
      case "" => 0 //  It can return “” instead of zero (from API Documentation)
      case number => number.toInt
      //case _ => throw ResponseStatus(err)
      case _ => 0
    }

    def %%(name: String) = response.get(name) match { case Some(v) => v; case None => "" }
    //def %%(name: String) = response.get(name).get

    ConfirmResult(List(PincodeRecord(response)), %(Params.amount), ticket = %%(Params.ticket))

    //ConfirmResult(List(PincodeRecord(response)), %(Params.amount), ticket = if (response.contains(Params.ticket)) %%(Params.ticket) else "")

    //if (response.contains(Params.ticket))
    //  ConfirmResult(List(PincodeRecord(response)), %(Params.amount), ticket = %%(Params.ticket))
    //else
    //  ConfirmResult(List(PincodeRecord(response)), %(Params.amount))
  }




  val v = (x \\ "unum").text.split(",").toList
  val list = List("pincodeSerial", "pincode1", "pincode2", "expiredDate")
  val res = (list zip v).toMap
  //println(res)


  /*private val transactionNumber = new java.util.concurrent.atomic.AtomicLong(Long.MaxValue - 3)
  def transaction() = {
    transactionNumber.compareAndSet(Long.MaxValue, 1)
    transactionNumber.getAndIncrement
  }      */

  /*import Foo.transaction

  transaction()
  transaction()
  transaction()
  transaction()
  transaction()
  transaction()
  transaction()
  println(transaction())
  */
  val foo1 = new Foo
  foo1.add

  val foo2 = new Foo
  foo2.add

  val foo3 = new Foo
  foo3.add

  //println(foo3.add)

  //println(Params.amount)
  //println(Params.day)

  val template1 = Map(
    Params.terminal -> Params.terminal,
    Params.cashier -> Params.cashier,
    Params.day -> Params.day
  )

  //println(template1.getClass)

  val s = template1.get(Params.session)
  //println(s)

  /*
  - cashRegisterNumber
  - referenceNumber
  - transactionTermnalId(The POS terminal ID did the transaction)
  - transactionCashierId(Cashier Id did the transaction)
  - transactionNumber
  - transactionTime (format : yyyy-MMdd'T'HH:mm:ss.SSS)
  - gencode
  - pincodeSerial
  - amount
  - transactionStatus (R, C, Y, N)
  */
  for(a <- 1 until 10){
    //println( "Value of a: " + a );
  }

  val params = List("cashRegisterNumber", "referenceNumber", "transactionTermnalId", "transactionCashierId",
    "transactionNumber", "transactionTime", "gencode", "pincodeSerial", "amount", "transactionStatus")

  //println(Map(params map {s => (s, 1)}: _*))

  val lst = List.apply(Map(params map {s => (s, 1)}: _*))
  //println("LST:" + lst)

  /*def mkMap(idx: Int) : Map[String, String] = {
    val res = params map {s => (s, idx)}
    res
  }*/

  val soap = <soapenv:Envelope
  xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
  xmlns:wsg="http://www.ogloba.com/ew/wsg/">
    <soapenv:Header/>
    <soapenv:Body>
      <wsg:initialize>
        <terminalId>34753001001</terminalId>
        <cashierId>339</cashierId>
        <passphrase>U51lWQe9Xbd169Bci20s7zMi758SET7K</passphrase>
      </wsg:initialize>
    </soapenv:Body>
  </soapenv:Envelope>

  //println(soap)
  //println()

  val body =
    <wsg:initialize>
      <terminalId>34753001001</terminalId>
      <cashierId>339</cashierId>
      <passphrase>U51lWQe9Xbd169Bci20s7zMi758SET7K</passphrase>
  </wsg:initialize>

  val head = <soapenv:Envelope
  xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
  xmlns:wsg="http://www.ogloba.com/ew/wsg/">
    <soapenv:Header/>
    <soapenv:Body>{body}</soapenv:Body>
  </soapenv:Envelope>

  //println(head)


 val req = Map("terminalId" ->"34753001001",
   "cashierId" -> "339",
   "passphrase" -> "U51lWQe9Xbd169Bci20s7zMi758SET7K")

  //doRequest("initialize", req)

  protected def doRequest(node: String, request: Map[String, String]) = {
    //val data = createXML(node, template ++ request).toString()
    val data = <soapenv:Envelope
    xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
    xmlns:wsg="http://www.ogloba.com/ew/wsg/">
      <soapenv:Header/>
      <soapenv:Body>{createXML("wsg:" + node, template ++ request)}</soapenv:Body>
    </soapenv:Envelope>

    //println(data)
  }

  val response = <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
      <ns2:initializeResponse xmlns:ns2="http://www.ogloba.com/ew/wsg/">
        <isSuccessful>true</isSuccessful>
        <sessionId>ffExOAZ0abjlCuv282fEcv10tCTt502DgKxqSIJB</sessionId>
      </ns2:initializeResponse>
    </soap:Body>
  </soap:Envelope>


  val irn = response \\ "initializeResponse" filter (e => e.namespace == "http://www.ogloba.com/ew/wsg/")
  //println("1: " + irn)

  val name = (response \\ "initializeResponse").filter(_.prefix == "ns2")
  //println("2: " + name.getClass)

  val name3 = (response \\ "initializeResponse").filter(_.prefix == "ns2") flatMap { _ match {
    case e:Elem => e
    //case _ => NodeSeq.Empty
    case _ => None
  } }
  //println("3: " + name3.getClass)

  val successful = (irn \\ Params.isSuccessful).text.toBoolean
  //println(successful)

  // base64
  val str = "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4= "
  //val b = DatatypeConverter.parseBase64Binary(str)
  //println("B:"+b)
  val result = new String(DatatypeConverter.parseBase64Binary(str))
  //println("RES:" + result)

  private def encodeBase64(sre: String) = new String(DatatypeConverter.parseBase64Binary(str))

  println(encodeBase64(str))

}

class Foo{
  import Transaction.get
  def add = get()
}

object Transaction{
  private val transactionNumber = new java.util.concurrent.atomic.AtomicLong(1)
  def get() = {
    this.synchronized {
      transactionNumber.compareAndSet(Long.MaxValue, 1)
      transactionNumber.getAndIncrement
    }
  }
}

object Params extends Enumeration {
  val terminal = "terminalId"
  val cashier = "cashierId"
  val cashNumber = "cashRegisterNumber"
  val day = "businessDay"
  val session = "sessionId"
  val ref = "referenceNumber"
  val ticket = "ticket"
  val transId = "transactionTermnalId"
  val transNum = "transactionNumber"
  val gencode = "gencode"
  val pincodes = "pincodes"
  val pincode = "pincodeSerial"
  val amount = "amount"
  val transTime = "transactionTime"
  val transStatus = "transactionStatus"
  val isSuccessful = "isSuccessful"
  val errCode = "errorCode"
  val errMsg = "errorMessage"
  val errDetail = "errorDetail"
  val loyalty = "loyaltyCardNumber"
}