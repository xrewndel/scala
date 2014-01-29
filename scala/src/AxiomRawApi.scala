import amdrew.ws._
import amdrew.ws.Reconcile.ReconciliationRecords
import amdrew.ws.Reconcile.ReconciliationRecords.ReconciliationRecord
import com.sun.xml.internal.ws.developer.JAXWSProperties
import java.lang.String
import java.util.{Calendar, GregorianCalendar, Date}
import javax.xml.bind.{JAXBElement, DatatypeConverter}
import javax.xml.datatype.{DatatypeConstants, DatatypeFactory, XMLGregorianCalendar}
import javax.xml.ws.{Holder, BindingProvider}
import javax.xml.ws.handler.Handler
import scala.collection.JavaConverters._

/**
 * Created by Andrew on 28.01.14.
 */
class AxiomRawAPI(url: String, terminalId: String, cashierId: String, passphrase: String, locale: String = "EN") {
  //import AxiomRawAPI._

  /** Connect timeout */
  val TIMEOUT = 60 // seconds

  /** Shift id (1 to 99) */
  private var _id = -1
  private def shiftID: Integer = { _id = (_id + 1) % 99; _id + 1 }
  private var transID = 0
  protected def transactionNumber = {transID = transID + 1; transID.toString }

  // request timeout
  val REQUEST_TIMEOUT = 65000 // milliseconds
  val CONNECT_TIMEOUT = 15000  // milliseconds

  // initialize service
  protected lazy val ws = {
    val ws = (new EwalletTransactionService()).getEwalletTransactionServiceSOAP
    val context = ws.asInstanceOf[BindingProvider].getRequestContext
    context.put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY, "http://localhost:8080")
    context.put(JAXWSProperties.REQUEST_TIMEOUT, REQUEST_TIMEOUT.asInstanceOf[java.lang.Integer])
    context.put(JAXWSProperties.CONNECT_TIMEOUT, CONNECT_TIMEOUT.asInstanceOf[java.lang.Integer])

    // if certificate doesn't match domain
    //context.put(JAXWSProperties.HOSTNAME_VERIFIER, new HostnameVerifier() {
    //  def verify(s: String, sslSession: SSLSession) = true
    //})

    // SOAP logging
    //val bind = ws.asInstanceOf[BindingProvider].getBinding()
    //bind.setHandlerChain(getHandlerChain())

    def getHandlerChain(): java.util.List[javax.xml.ws.handler.Handler[_ <: javax.xml.ws.handler.MessageContext]] = {
      List[Handler[_ <: javax.xml.ws.handler.MessageContext]](new SOAPLoggingHandler()).asJava
    }

    ws
  }

  // template of params for all requests
  val request = new Request
  request.setTerminalId(terminalId)
  request.setLocale(locale)
  request.setCashierId(cashierId)


  /**
   * Authentication message (3.1.1 from API Documentation)
   *
   * @return sessionId (String)
   * Request example: TODO
   * Response example: TODO
   */
  def auth: String = {
    val session = new Holder[java.lang.String]
    val isOk = new Holder[java.lang.Boolean]

    ws.initialize(request.getTerminalId, session, request.getLocale, request.getCashierId, passphrase, isOk)

    assert(isOk.value)
    request.setSessionId(session.value)

    session.value
  }


  /**
   * Shift Action (shiftManagement) (3.1.2 from API Documentation))
   *
   * @param action: open or close (ShiftAction)
   * @return balance: String
   */
  def shiftAction(action: ShiftAction, date: Date = new Date()): Double = {
    val balance = new Holder[java.lang.Double]
    val isOk = new Holder[java.lang.Boolean]

    ws.shiftAction(request.getTerminalId, request.getSessionId, request.getLocale, request.getCashierId, action.value,
      fmtDate(date), shiftID.toString, isOk, balance)

    assert(isOk.value)

    balance.value
  }


  /**
   * Reserve Pincode (3.2.1 from API Documentation))
   *
   * @param cashRegisterNumber: ECR number
   * @param gencode: Product Code
   * @return referenceNumber: Int
   */
  def reservePincode(cashRegisterNumber: String, gencode: String, track2: String = "", loyaltyCardNumber: String = ""): String = {
    val referenceNumber = new Holder[String]
    val isOk = new Holder[java.lang.Boolean]

    ws.reservePincode(request.getTerminalId, request.getSessionId, request.getLocale, request.getCashierId,
      cashRegisterNumber, null, gencode, 1, track2, loyaltyCardNumber, isOk, referenceNumber)

    assert(isOk.value)

    // there is loyaltyCardTotalPoints also returns (as number) in spec but wsdl implementation doesn't contain this param
    referenceNumber.value // Number in spec but String in wsdl implementation
  }


  /**
   * Cancel Pincode (3.2.2 from API Documentation))
   *
   * @param referenceNumber
   * @return true
   * @throws ErrorException
   */
  def cancelPincode(referenceNumber: String): Unit =
    ws.cancelPincode(request.getTerminalId, request.getSessionId, request.getLocale, request.getCashierId, referenceNumber)


  /**
   * Confirm Reservation (3.2.3 from API Documentation))
   *
   * @param referenceNumber
   * @return referenceNumber: String
   */
  def confirmReservation(referenceNumber: String, transNum: String, ticketReq: java.lang.Boolean = false): ConfirmResult = {
    val isOk = new Holder[java.lang.Boolean]
    val amount = new Holder[java.lang.Integer]
    val pincodes = new Holder[Pincodes]
    val originalLoyaltyCardTotalPoint = new Holder[java.lang.Integer]
    val newLoyaltyCardPoint = new Holder[java.lang.Integer]
    val loyaltyCardTotalPoint = new Holder[java.lang.Integer]
    val ticket = new Holder[java.lang.String]

    ws.confirmReservation(request.getTerminalId, request.getSessionId, request.getLocale, request.getCashierId, ticketReq,
      referenceNumber, transNum, isOk, amount, pincodes, originalLoyaltyCardTotalPoint, newLoyaltyCardPoint,
      loyaltyCardTotalPoint, ticket)

    assert(isOk.value)

    ConfirmResult(amount, pincodes, originalLoyaltyCardTotalPoint, newLoyaltyCardPoint, loyaltyCardTotalPoint, ticket)
  }


  /**
   * Confirm Pincode Received (3.2.4 from API Documentation))
   *
   * @param referenceNumber
   * @return referenceNumber: String
   * @throws ErrorException
   */
  def confirmPincodeReceived(referenceNumber: String): Unit =
    ws.confirmPincodeReceived(request.getTerminalId, request.getSessionId, request.getLocale, request.getCashierId, referenceNumber)


  /**
   * Reconciliation message (3.3.1 from API Documentation))
   *
   * @param status - transactionStatus (R, C, Y, N)
   * @return true or false
   */
  def reconciliation(cashRegisterNumber: String, dt: Date, refNum: String, gencode: String, tNum: String, amount: Int,
                     serial: String, status: String): Unit = {
    val reconcileRecord = new ReconciliationRecord
    reconcileRecord.setTransactionTerminalId(terminalId)
    reconcileRecord.setTransactionCashierId(cashierId)
    reconcileRecord.setTransactionTime(fmtTime(dt))
    reconcileRecord.setCashRegisterNumber(cashRegisterNumber)
    reconcileRecord.setReferenceNumber(refNum)
    reconcileRecord.setGencode(gencode)
    reconcileRecord.setTransactionNumber(tNum)
    reconcileRecord.setAmount(amount)
    reconcileRecord.setPincodeSerial(serial)
    reconcileRecord.setTransactionStatus(status)

    val reconcileList = new ReconciliationRecords
    reconcileList.getReconciliationRecord().add(reconcileRecord);

    ws.reconcile(request.getTerminalId, request.getSessionId, request.getLocale, request.getCashierId,
    cashRegisterNumber, fmtDate(dt), reconcileList)
  }


  /** Get date in yyyy-MM-dd format */
  private def fmtDate(dt: Date): XMLGregorianCalendar = {
    val cal = new GregorianCalendar()
    cal.setTime(dt)
    DatatypeFactory.newInstance().newXMLGregorianCalendarDate(
      cal.get(Calendar.YEAR),
      cal.get(Calendar.MONTH) + 1,
      cal.get(Calendar.DAY_OF_MONTH),
      DatatypeConstants.FIELD_UNDEFINED)
  }

  /** Get date in yyyy-MMdd'T'HH:mm:ss.SSS format */
  private def fmtTime(dt: Date): XMLGregorianCalendar = {
    val cal = new GregorianCalendar()
    cal.setTime(dt)
    DatatypeFactory.newInstance().newXMLGregorianCalendar(cal)
  }


  /*private def setPinSerial(codes: Holder[Pincodes]): Unit =
    if (codes.value != null && codes.value.getPincode.size() > 0)
      reconcileRecord.setPincodeSerial(codes.value.getPincode.get(0).getPincodeSerial)
    else reconcileRecord.setPincodeSerial("")
  */

  /** Decode string in Base64 into readable view */
  //private def decodeBase64(str: String) = new String(DatatypeConverter.parseBase64Binary(str))

  case class ConfirmResult(amount: Int, pin: Pincodes = new Pincodes, originalLoyaltyCardTotalPoint: Int = 0,
                           newLoyaltyCardPoint: Int = 0, loyaltyCardTotalPoint: Int = 0, ticket: String = "")
  object ConfirmResult {

    def decodeBase64(str: String) = new String(DatatypeConverter.parseBase64Binary(str))

    def apply(amount: Holder[java.lang.Integer], pincodes: Holder[Pincodes],
              originalLoyaltyCardTotalPoint: Holder[java.lang.Integer], newLoyaltyCardPoint: Holder[java.lang.Integer],
              loyaltyCardTotalPoint: Holder[java.lang.Integer], ticket: Holder[java.lang.String]): ConfirmResult = {

      ConfirmResult(amount.value,
        if (pincodes.value != null)                       pincodes.value else new Pincodes,
        if (originalLoyaltyCardTotalPoint.value != null)  originalLoyaltyCardTotalPoint.value.toInt else 0,
        if (newLoyaltyCardPoint.value != null)            newLoyaltyCardPoint.value.toInt else 0,
        if (loyaltyCardTotalPoint.value != null)          loyaltyCardTotalPoint.value.toInt else 0,
        if (ticket.value != null)                         decodeBase64(ticket.value) else ""
      )
    }
  }

  trait ShiftAction { val value: String }
  object ShiftAction {
    case object Open extends ShiftAction { val value = "O" }
    case object Close extends ShiftAction { val value = "C" }
  }

}

