import net.fmb.integrator.serviceprovider.efresh.Sequence
import scalikejdbc._, SQLInterpolation._
import java.lang.String
import java.security.{PublicKey, KeyFactory, PrivateKey}
import java.security.spec.{X509EncodedKeySpec, PKCS8EncodedKeySpec}
import javax.net.ssl.{HostnameVerifier, SSLSession}
import scala.collection.generic.{SeqFactory, GenSeqFactory}
import scala.collection.immutable.ListMap
import scala.xml.{TopScope, Elem}
object Hello extends App {
  //args.foreach(println)

  def createXML(root: String, params: Map[String, String] = Map()): xml.Elem = {
    params.foldLeft(<root/>.copy(label = root)) {
      case (acc, (k, v)) => acc.copy(child = acc.child :+ <tag>{v}</tag>.copy(label=k))
    }
  }

  implicit def map2xml(map: Map[String, String]) = new {
    def toXML(root: String = "root") = createXML(root, map)
  }

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

  val result = (response \ "PINDATA") map { x => ((x \ "@name").text -> x) } toMap
  val err = (for{x <- response \ "PINDATA"} yield (x \ "@name", x)).toMap
  //println("ERR:" + result)
  println(response.contains("BALANCE"))

  val sequence = new Sequence


}
