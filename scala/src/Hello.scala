import net.fmb.integrator.serviceprovider.efresh.Sequence
import scalikejdbc.AutoSession
import scalikejdbc.ConnectionPool
import scalikejdbc._, SQLInterpolation._
import amdrew.ws.Reconcile.ReconciliationRecords
import amdrew.ws.Reconcile.ReconciliationRecords.ReconciliationRecord
import java.io.{FileInputStream, File}
import java.lang.String
import java.security.{PublicKey, KeyFactory, PrivateKey}
import java.security.spec.{X509EncodedKeySpec, PKCS8EncodedKeySpec}
import javax.net.ssl.{HostnameVerifier, SSLSession}
import javax.xml.bind.DatatypeConverter
import scala.collection.generic.{SeqFactory, GenSeqFactory}
import scala.collection.immutable.ListMap
import scala.io.Source
import scala.Some
import scala.xml.{TopScope, Elem}
import amdrew.ws._
import javax.xml.ws.{Holder, BindingProvider}
import javax.xml.datatype.{XMLGregorianCalendar, DatatypeConstants, DatatypeFactory}
import java.util.{Calendar, Date, GregorianCalendar}
import scala.collection.JavaConverters._
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

  val map = Map("unum" -> "daya", "duobus" -> "biyu", "tribus" -> "uku")

  val xml1 = createXML("")
  println("XML1: " + xml1)
  val xml2 = createXML("root", map)
  //println("XML2: " + xml2)
  val sss = xml1 +: (<ddd>{xml2}</ddd>)       // correct
  //println("SSS: " + sss)
  val sss2 = xml1 ++ {xml2}
  //println("SSS2: " + sss2)

  def createXML(root: String, params: Map[String, String] = Map()): xml.Elem = {
    params.foldLeft(<root/>.copy(label = root)) {
      case (acc, (k, v)) => acc.copy(child = acc.child :+ <tag>{v}</tag>.copy(label=k))
    }
  }

  implicit def map2xml(map: Map[String, String]) = new {
    def toXML(root: String = "root") = createXML(root, map)
  }

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

  val response = <PINRESPONSE>
    <VERSION>2.0</VERSION>
    <REFNO>1234567890350001</REFNO>
    <TRANSNO>343434343434</TRANSNO>
    <STATUSCODE>0</STATUSCODE>
    <STATUSDESCRIPTION>Pin Delivered</STATUSDESCRIPTION>
      <PINDATA>
        <PIN>123456</PIN>
        <CUSTOM1>123456</CUSTOM1>
        <CUSTOM2>123456</CUSTOM2>
        <PINSERIAL>123456</PINSERIAL>
        <PINBATCH>123456</PINBATCH>
        <EXPIRYDATE>2006/01/01</EXPIRYDATE>
        <DATESOLD>2008-06-13 15:26:52</DATESOLD>
      </PINDATA>
    </PINRESPONSE>


  println("Class: " + response.getClass)
  val pin = (response \ "PINDATA")
  //println("PIN class: " + pin.getClass)
  val resMap = Map(response \ "_" filter { _.isInstanceOf[xml.Elem] } map { el => el.label -> el.text.trim }: _*) -("PINDATA")
  //println("RESULT= " + resMap)
  //val resMap2 = resMap.-("PINDATA")
  //println("resMap2= "+resMap2)

  val pinMap = Map(pin \ "_" filter { _.isInstanceOf[xml.Elem] } map { el => el.label -> el.text.trim }: _*)
  //println("PIN= "+pinMap)

  val last = xml2Map(response) -("PINDATA") ++ xml2Map(response \ "PINDATA")
  println("LAST: " + last)

  def xml2Map(obj: scala.xml.NodeSeq) = Map(obj \ "_" filter { _.isInstanceOf[xml.Elem] } map { el => el.label -> el.text.trim }: _*)
  val test = xml2Map(pin)
  //println("TEST: " + test)

  val o2m = object2Map(response)
  println("\nO2M: " + o2m)

  def object2Map(o: AnyRef, exclude: String = ""): Map[String, String] =
    (ListMap[String, String]() /: o.getClass.getDeclaredFields.filterNot(_.getName.startsWith("$"))) { (a, f) =>
      f.setAccessible(true)
      if (!f.getName.equals(exclude))
        a + (f.getName -> ( if (f.get(o) == null) null else if (f.get(o) == None) "" else  f.get(o).toString))
      else a
    }

  //println("2MAP: " + (resMap ++ pinMap))

  //for (n <- response.child) {
  //   if (n.nonEmptyChildren.size > 1) println("!!!:" + n.label + ":"+ n.child + "::" + n.text)
  //}

  val result = (response \ "PINDATA") map { x => ((x \ "@name").text -> x) } toMap
  val err = (for{x <- response \ "PINDATA"} yield (x \ "@name", x)).toMap
  //println("ERR:" + result)
  println(response.contains("BALANCE"))

  for (n <- response.child) {
    //println(n.label + ":" + n.child.size + "\n")
   val m = n.nonEmptyChildren.size match {
      //case 1 => Map(n.label -> n.text)
      //case num => if (num > 1) Map(n.label -> n.text)
      case l if(l == 1) => Map(n.label -> n.text)
      case l if(l > 1) => Map(n.label -> n.text)
      case _ => Nil
    }
    //println("M:" + m)
  }

  val response2 = <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
      <ns2:initializeResponse xmlns:ns2="http://www.ogloba.com/ew/wsg/">
        <isSuccessful>true</isSuccessful>
        <sessionId>ffExOAZ0abjlCuv282fEcv10tCTt502DgKxqSIJB</sessionId>
      </ns2:initializeResponse>
    </soap:Body>
  </soap:Envelope>


  val irn = response2 \\ "initializeResponse" filter (e => e.namespace == "http://www.ogloba.com/ew/wsg/")
  //println("1: " + irn)

  val name = (response2 \\ "initializeResponse").filter(_.prefix == "ns2")
  //println("2: " + name.getClass)

  val name3 = (response2 \\ "initializeResponse").filter(_.prefix == "ns2") flatMap { _ match {
    case e:Elem => e
    //case _ => NodeSeq.Empty
    case _ => None
  } }
  //println("3: " + name3.getClass)

  val secret = " NewSequence NewProductVer NewSoftVer GroupId".trim.split(" ").toList.head
  println("secret out:" + secret)


  /*
  val last = List(1, 1, 2, 3, 5, 8)
  def lastRecursive[A](ls: List[A]): A = ls match {
    case h :: Nil  => println("H: " + h); h
    case _ :: tail => println("Tail: " + tail);lastRecursive(tail)
    case _         => println("Exc"); throw new NoSuchElementException
  }
  //println("Tail:" + lastRecursive(last))

  def penultimateBuiltin[A](ls: List[A]): A =
    if (ls.isEmpty) throw new NoSuchElementException
    else ls.init.last

  println(penultimateBuiltin(last))


  def lengthTailRecursive[A](ls: List[A]): Int = {
    def lengthR(result: Int, curList: List[A]): Int = curList match {
      case Nil       => result
      case _ :: tail => lengthR(result + 1, tail)
    }
    lengthR(0, ls)
  }

  // More pure functional solution, with folds.
  def lengthFunctional[A](ls: List[A]): Int = ls.foldLeft(0) { (c, _) => c + 1 }
  val flat = List(List(1, 1), 2, List(3, List(5, 8)))
  */

  val instr = """@LGO:023;
                |@FSN;
                |@CTR;@FNY;Recharge Amount 20 AED
                |@FSN;
                |@CTR;@FNY; Card Number
                |@FNY;@CTR;19253-48144-50592
                |@FSN;Date : 20/12/2012 15:56:21
                |@FSN;Merchant Name : 1005 Al Ramool
                |@FSN;Merchant Id : 34753001
                |@FSN;Terminal Id : 34753001001
                |@FSN;Du Dealer Id : 555345
                |@FSN;------------------------------------------@FSN;Transaction id :551_54-002
                |@FSN;Pin Serial No :9005384679
                |@FSN;------------------------------------------@FNY;@CTR;Recharge Information
                |@FNY;Bonus options:
                |@FNY;"more international":
                |@FSN;Get AED 26 for international calls valid for 30 days.
                |@FSN;Dial *138* card number # to recharge
                |@FNY;"more credit":
                |@FSN;Get AED 23 for all calls valid for 30 days
                |@FSN;Dial *136* card number # to recharge
                |@FNY;Regular option:
                |@FNY;"more time":
                |@FSN;Get AED 20 valid for life.
                |@FSN;Dial *135* card number # to recharge
                |@FNY;New option:
                |@FNY;"more data":
                |@FSN;Get 40MB of National Data valid for 30 days.
                |@FSN;Dial *131* card number # to recharge
                |@FSN;@CTR;------------------------------------------@FSY;@LFT;Recharge Offer:
                |Get AED 40 instead of AED 26 with a validity of 30 days with the “more international” option.
                |To activate this offer dial *135*111# before recharging.
                |@FSN;@CTR;------------------------------------------@FSN;@CTR;Information shown above is correct at time of printing."""

  //println("INSTR:" + instr.getClass)
  //val instr2 = instr.replaceAll("@F(S|N)(N|Y);", "")
  //val instr2 = instr.replaceAll("@[A-Z]{3}(;|:)", "").replaceAll("(-){2,}", "")
  //val instr2 = instr.split("@") map(_.replaceAll("@[A-Z]{3}(;|:)", "").replaceAll("(-){2,}", "")) mkString
  //val instr2 = instr.split(";") map(_.replaceAll("@[A-Z]{3}", "").replaceAll("(-){2,}", "&&").replace("|", "").trim.concat("\n")) mkString
  //val instr2 = instr.split(";") map(_.replaceAll("@[A-Z]{3}", "))").replaceAll("(-){2,}", "&&").replace("|", "??"))
  //for (n <- instr2) println(n)
  //println("INSTR2:" + instr2)

  /** Decode string in Base64 into readable view */
  private def decodeBase64(str: String) = new String(DatatypeConverter.parseBase64Binary(str))

  /** Clean ticket from tags for pos terminal (@... and many ----) */
  //private def cleanTicket(str: String) = str.replaceAll("@[A-Z]{3};", "").replaceAll("(-){2,}", "") split("\n") map(_.trim) mkString("\n")
  //private def cleanTicket(str: String) = str.replaceAll("@[A-Z]{3};", "").replaceAll("(-){2,}", "") split("\n") map(_ match {
  //  case s => if (!s.startsWith("@LGO") && !s.equals("")) s.trim + "\n"
  //}) mkString

  private def cleanTicket(str: String) = str.split("@[A-Z]{3};") map(_ match {
    case s => if (!s.startsWith("@LGO") && !s.startsWith("-")) s else ""
  }) mkString


  val tkt ="QExHTzowMjM7CkBGU047ICAKQENUUjtARk5ZO1JlY2hhcmdlIEFtb3VudCAyMCBBRUQKQEZTTjsgCkBDVFI7QEZOWTsgQ2FyZCBOdW1iZXIKQEZOWTtAQ1RSOzE5MjUzLTQ4MTQ0LTUwNTkyCkBGU047RGF0ZSAgICAgICAgICAgICAgOiAyMC8xMi8yMDEyIDE1OjU2OjIxCkBGU047TWVyY2hhbnQgTmFtZSAgICAgOiAxMDA1IEFsIFJhbW9vbApARlNOO01lcmNoYW50IElkICAgICAgIDogMzQ3NTMwMDEKQEZTTjtUZXJtaW5hbCBJZCAgICAgICA6IDM0NzUzMDAxMDAxCkBGU047RHUgRGVhbGVyIElkICAgICAgOiA1NTUzNDUKQEZTTjstLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0KQEZTTjtUcmFuc2FjdGlvbiBpZCAgICA6NTUxXzU0LTAwMgpARlNOO1BpbiBTZXJpYWwgTm8gICAgIDo5MDA1Mzg0Njc5CkBGU047LS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tCkBGTlk7QENUUjtSZWNoYXJnZSBJbmZvcm1hdGlvbgpARk5ZO0JvbnVzIG9wdGlvbnM6CkBGTlk7Im1vcmUgaW50ZXJuYXRpb25hbCI6CkBGU047R2V0IEFFRCAyNiBmb3IgaW50ZXJuYXRpb25hbCBjYWxscyB2YWxpZCBmb3IgMzAgZGF5cy4KQEZTTjtEaWFsICoxMzgqIGNhcmQgbnVtYmVyICMgdG8gcmVjaGFyZ2UKQEZOWTsibW9yZSBjcmVkaXQiOgpARlNOO0dldCBBRUQgMjMgZm9yIGFsbCBjYWxscyB2YWxpZCBmb3IgMzAgZGF5cwpARlNOO0RpYWwgKjEzNiogY2FyZCBudW1iZXIgIyB0byByZWNoYXJnZQpARk5ZO1JlZ3VsYXIgb3B0aW9uOgpARk5ZOyJtb3JlIHRpbWUiOgpARlNOO0dldCBBRUQgMjAgdmFsaWQgZm9yIGxpZmUuCkBGU047RGlhbCAqMTM1KiBjYXJkIG51bWJlciAjIHRvIHJlY2hhcmdlCkBGTlk7TmV3IG9wdGlvbjoKQEZOWTsibW9yZSBkYXRhIjoKQEZTTjtHZXQgNDBNQiBvZiBOYXRpb25hbCBEYXRhIHZhbGlkIGZvciAzMCBkYXlzLgpARlNOO0RpYWwgKjEzMSogY2FyZCBudW1iZXIgIyB0byByZWNoYXJnZQpARlNOO0BDVFI7LS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tCkBGU1k7QExGVDtSZWNoYXJnZSBPZmZlcjoKR2V0IEFFRCA0MCBpbnN0ZWFkIG9mIEFFRCAyNiB3aXRoIGEgdmFsaWRpdHkgb2YgMzAgZGF5cyB3aXRoIHRoZSDigJxtb3JlIGludGVybmF0aW9uYWzigJ0gb3B0aW9uLgpUbyBhY3RpdmF0ZSB0aGlzIG9mZmVyIGRpYWwgKjEzNSoxMTEjIGJlZm9yZSByZWNoYXJnaW5nLgoKQEZTTjtAQ1RSOy0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLQpARlNOO0BDVFI7SW5mb3JtYXRpb24gc2hvd24gYWJvdmUgaXMgY29ycmVjdCBhdCB0aW1lIG9mIHByaW50aW5nLgo"

  val tktdec = decodeBase64(tkt)
  //println("TKT:"+tktdec)
  val cln = cleanTicket(tktdec)
  //println("CLEAN:" + cln)

  val sequence = new Sequence


}
