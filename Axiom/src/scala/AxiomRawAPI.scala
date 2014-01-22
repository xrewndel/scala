import java.net.URL
import java.util.Date
import javax.net.ssl.HttpsURLConnection
import org.apache.log4j.Logger
import scala.io.Source
import scala.xml.{TopScope, Elem}
import scala.collection.immutable.ListMap

/**
 * Created by Andrew on 10.01.14.
 */
class AxiomRawAPI(url: String, terminalId: String, cashierId: String, passphrase: String, locale: String = "EN") {

  import AxiomRawAPI._
  /** logger */
  private[this] val logger = Logger.getLogger(getClass)
  private def log(message: => AnyRef) { if (logger.isDebugEnabled) logger.debug(message) else print(message)}
  private def log(message: => AnyRef, t: Throwable) { if (logger.isDebugEnabled) logger.debug(message, t) }

  /** Connect timeout */
  val TIMEOUT = 60 // seconds

  /** Shift id (1 to 99) */
  private var _id = -1
  private def shiftID = { _id = (_id + 1) % 99; _id + 1 }

  /** Date format: YYYY-MM-DD */
  private val dateFmt = new java.text.SimpleDateFormat("yyyy-MM-dd")

  /** Root node for xml requests*/
  private val root = "root"

  /** Common error message */
  private val err = "Unexpected xml structure"

  /** Session ID is setting by server side in Authentication message */
  private var sessionId = ""

  // template of params for all requests
  private def template = Map(
    Params.terminal -> terminalId,
    Params.cashier -> cashierId,
    Params.locale -> locale,
    Params.session -> sessionId
  )

  /**
   * Create XML from Map[String, String]
   *
   * @param root Root tag
   * @param params Map of parameters 'param -> value'
   */
  protected def createXML(root: String, params: Map[String, String]): xml.Elem = {
    params.foldLeft(<root/>.copy(label = root)) {
      case (acc, (k, v)) => acc.copy(child = acc.child :+ <tag>{v}</tag>.copy(label=k))
    }
  }

  /**
   * Authentication message (3.1.1 from API Documentation)
   *
   * @return sessionId (String)
   * Request example: TODO
   * Response example: TODO
   */
  def auth: String = doRequest("initialize", Map(Params.pass -> passphrase)).get(Params.session) match {
    case Some(v) => sessionId = v; v
    case _ => throw ResponseStatus(err)
  }

  /**
   * Shift Action (shiftManagement) (3.1.2 from API Documentation))
   *
   * @param action: open or close (ShiftAction)
   * @return balance: String
   */
  def shiftAction(action: ShiftAction, date: Date = new Date()): String = doRequest(Map(
    Params.day -> formatDate(date),
    Params.shift -> shiftID(),
    Params.action -> action.value  /*Open or Close*/
    )).get(Params.balance) match {
      case Some(v) => v
      case _ => throw ResponseStatus(err)
  }

  /**
   * Reserve Pincode (3.2.1 from API Documentation))
   *
   * @param cashRegisterNumber: ECR number
   * @param gencode: Product Code
   * @return referenceNumber: Int
   */
  def reservePincode(cashRegisterNumber: String, gencode: String,
                     track2: String = "", loyaltyCardNumber: String = ""): Reserve = {
    /*doRequest(Map(
      Params.cashNumber -> cashRegisterNumber,
      Params.gencode -> gencode,
      if (!track2.isEmpty ) Params.track2 -> track2,
      if (!loyaltyCardNumber.isEmpty ) Params.loyalty -> loyaltyCardNumber,
      Params.quantity -> 1 // should be always 1 (from Documentation)
    )).get(Params.ref) match {
      case Some(v) => v
      case _ => throw ResponseStatus(err)
    }*/
    Reserve(doRequest("reservePincode", Map(
      Params.cashNumber -> cashRegisterNumber,
      Params.gencode -> gencode,
      if (!track2.isEmpty ) Params.track2 -> track2,
      if (!loyaltyCardNumber.isEmpty ) Params.loyalty -> loyaltyCardNumber,
      Params.quantity -> 1 // should be always 1 (from Documentation)
    )))
  }

  /**
  * Cancel Pincode (3.2.2 from API Documentation))
  *
  * @param referenceNumber
  * @return true
  */
  def cancelPincode(referenceNumber: String): Boolean = {
    doRequest(Map(Params.ref -> referenceNumber))
    true
  }

  /**
   * Confirm Reservation (3.2.3 from API Documentation))
   *
   * @param referenceNumber
   * @return referenceNumber: String
   */
  def confirmReservation(referenceNumber: String, ticketReq: Boolean = false): Map[String, String] = {
    //val response = doRequest(Map(Params.ref -> referenceNumber, Params.ticket -> ticketReq))
    //ConfirmResult(response)
    ConfirmResult(doRequest("confirmReservation", Map(Params.ref -> referenceNumber, Params.ticket -> ticketReq)))
  }

  /**
   * Confirm Pincode Received (3.2.4 from API Documentation))
   *
   * @param referenceNumber
   * @return referenceNumber: String
   */
  def confirmPincodeReceived(referenceNumber: String): Boolean = {
    doRequest("confirmPincodeReceived", Map(Params.ref -> referenceNumber))
    true
  }

  // UNDONE
  /**
   * Reconciliation message (3.3.1 from API Documentation))
   *
   * @param records List of records
   * Record:
      - cashRegisterNumber
      - referenceNumber
      - transactionTermnalId(The POS terminal ID did the transaction)
      - transactionCashierId(Cashier Id did the transaction)
      - transactionNumber
      - transactionTime (format : yyyy-MM-dd'T'HH:mm:ss.SSS)
      - gencode
      - pincodeSerial
      - amount
      - transactionStatus (R, C, Y, N)
   * @return true or false
   */
  //def reconcile(records: List[Map[String, String]]): Boolean = {
  def reconcile(records: Map[String, String]): Boolean = {
    todo reconciliationRecord as case class
    //val recordXml = records.foreach(createXML("record", _))
    val recordXml = createXML("reconciliationRecord", record)
    val head = createXML(root, template ++ Map(Params.ref -> referenceNumber))
    //val xml = head +: (<recordXml>{recordXml}</recordXml>)
    val xml = head ++ {recordXml}
    //val xml = createXML(root, template ++ Map(Params.ref -> referenceNumber)) ++ createXML("reconciliationRecord", record)
    doRequest("reconcile", xml)

    true
  }

  protected def createConnection(url: String): HttpsURLConnection = {
    // setting connection
    val con = new URL(url).openConnection.asInstanceOf[HttpsURLConnection]
    con.setRequestMethod("POST")
    con.setUseCaches(false)
    con.setDoOutput(true)
    con.setDoInput(true)
    con.setConnectTimeout(TIMEOUT * 1000)
    con.setHostnameVerifier(sslVerifier)

    con
  }

  /**
   * Do HTTP request and parse response
   *
   * @param request xml request of operation
   * @return Map of strings as result
   */
  protected def doRequest(node: String, request: Map[String, String]): Map[String, String] = {
    //val data = createXML(root, template ++ request).toString()
    val data = <soapenv:Envelope
    xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
    xmlns:wsg="http://www.ogloba.com/ew/wsg/">
      <soapenv:Header/>
      <soapenv:Body>{createXML("wsg:" + node, template ++ request)}</soapenv:Body>
    </soapenv:Envelope>

    log("Request XML: " + request)
    println("Request: " + request)

    val con = createConnection(url)
    val wr = con.getOutputStream
    wr.write(data.getBytes)
    wr.flush()
    wr.close()

    val res = Source.fromInputStream(con.getInputStream) mkString ""
    println("Response: " + res)
    log("Response: " + res)

    // getting xml
    val xmlResult = try xml.XML.loadString(res) catch {
      case ex: Exception => throw ResponseStatus(err)
    }

    // check if request was correct
    val successful = (xmlResult \\ Params.isSuccessful).text.toBoolean
    if (successful) {
      val resultMap = (for{chld <- xml.child} yield (chld.label, chld.text)).toMap
      if (resultMap.size < 1) throw ResponseStatus("Couldn't parse xml to map")
      resultMap
    }
    else
      throw ResponseStatus(xmlResult)
  }

  /** Get date in required format */
  private def formatDate(dt: Date = new Date()) = { dateFmt.format(dt) }

  /** Error handling */
  /*class ResponseStatus(val response: xml.Elem, val message: String) extends RuntimeException(message) {
    val code = (response \\ "errorCode").text
    val msg = (response \\ "errorMessage").text
    val detail = (response \\ "errorDetail").text

    override def getMessage = code + ", " + msg + ": " + detail
  }*/

  private class ResponseStatus(params : (Int, String)) extends RuntimeException() {
    val code = params._1
    val message = params._2

    override def getMessage = code + ", " + message
  }

  private object ResponseStatus {
    def apply(error: String) = new ResponseStatus(0, error)
    def apply(response: xml.Elem) = new ResponseStatus(getMessage(response))

    def getMessage(response: xml.Elem) = {
      val code = (response \\ Params.errCode).text.toInt
      val message = (response \\ Params.errMsg).text + ": " + (response \\ Params.errDetail).text
      (code, message)
    }
  }

}

private object AxiomRawAPI {
  trait ShiftAction { val value: String }
  object ShiftAction {
    case object Open extends ShiftAction { val value = "O" }
    case object Close extends ShiftAction { val value = "C" }
  }

  case class PincodeRecord(pincodeSerial: String, pincode1: String, pincode2: String = "", expiredDate: String)
  object PincodeRecord {
    def apply(response: Map[String, String]): PincodeRecord = {
      response.get(Params.pincodes).get.split(",").toList match {
        case List(a, b, c) => PincodeRecord(a, b, expiredDate = c)
        case List(a, b, c, d) => PincodeRecord(a, b, c, d)
        case _ => throw ResponseStatus(err)
      }
    }
  }

  case class ConfirmResult(pincodes: List[PincodeRecord], amount: Int, originalLoyaltyCardTotalPoint: Int = 0,
                           newLoyaltyCardPoint: Int = 0, loyaltyCardTotalPoint: Int = 0, ticket: String = "")
  object ConfirmResult {
    def apply(response: Map[String, String]): ConfirmResult = {
      def %(name: String): Int = response.get(name).get match {
        case "" => 0 //  It can return “” instead of zero (from API Documentation)
        case number => number.toInt
        case _ => 0
      }

      def %%(name: String) = response.get(name) match { case Some(v) => v; case None => "" }

      ConfirmResult(List(PincodeRecord(response)), %(Params.amount), %(Params.loyaltyOrig),
        %(Params.loyaltyNew), %(Params.loyaltyTotal), %%(Params.ticket))
    }
  }

  case class Reserve(referenceNumber: Int, loyaltyCardTotalPoints: Int = 0)
  object Reserve {
    def apply(response: Map[String, String]): Reserve = {
      def %%(name: String) = response.get(name) match { case Some(v) => v; case None => "" }

      Reserve(%%(Params.ref).toInt, %%(Params.loyaltyTotal) match {
          case "" => 0 //  It can return “” instead of zero (from API Documentation)
          case number => number.toInt
        }
      )
    }
  }

  /*
    - loyaltyCardNumber (optional)
    - newLoyaltyCardPoint (optional)
  */

  case class ReconcilRec(cashRegisterNumber: String, referenceNumber: Int, transactionTermnalId: String,
                         transactionCashierId: String, transactionNumber: Int, transactionTime: String,
                         gencode: String, pincodeSerial: String, amount: Int, transactionStatus: String,
                         loyaltyCardNumber: String = "", newLoyaltyCardPoint: Int = 0)
  object ReconcilRec {
    def apply(response: Map[String, String]): ReconcilRec = {

    }
  }

  /** Params */
  object Params {
    val action = "actionToDo"                    // String
    val amount = "amount"                        // Int
    val balance = "balance"                      // String ?
    val cashier = "cashierId"
    val cashNumber = "cashRegisterNumber"        // String
    val day = "businessDay"                      // yyyy-MM-dd
    val expired = "expiredDate"
    val errCode = "errorCode"
    val errMsg = "errorMessage"
    val errDetail = "errorDetail"
    val quantity = "quantity"
    val gencode = "gencode"                     // String
    val isSuccessful = "isSuccessful"
    val locale = "locale"
    val loyalty = "loyaltyCardNumber"           // String
    val loyaltyOrig = "originalLoyaltyCardTotalPoint"
    val loyaltyNew = "newLoyaltyCardPoint"
    val loyaltyTotal = "loyaltyCardTotalPoint"
    val pass = "passphrase"
    val pincodes = "pincodes"
    val pincode1 = "pincode1"
    val pinserial = "pincodeSerial"             // String
    val ref = "referenceNumber"                 // Int
    val session = "sessionId"
    val shift = "shiftId"
    val terminal = "terminalId"
    val ticket = "ticket"
    val track2 = "track2"
    val transTerminal = "transactionTermnalId"  // String
    val transCashier = "transactionCashierId"   // String
    val transNum = "transactionNumber"          // Int
    val transTime = "transactionTime"           // format: yyyy-MMdd'T'HH:mm:ss.SSS
    val transStatus = "transactionStatus"       // String (R, C, Y, N)
  }

  /** dump properties of an object into ordered map */
  private def object2Map(o: AnyRef): Map[String, String] =
    (ListMap[String, String]() /: o.getClass.getDeclaredFields.filterNot(_.getName.startsWith("$"))) { (a, f) =>
      f.setAccessible(true)
      a + (f.getName -> (if (f.get(o) == null) null else f.get(o).toString))
    }

}

