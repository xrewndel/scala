import scala.collection.immutable.{ListMap}
import scala.io.Source
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Extraction._

object app extends App {
  implicit val formats = net.liftweb.json.DefaultFormats

//  case class Country(Area: Option[String] = Some(""), SalesTeam: String = "", SubRegion: String = "",
//                     Region: String = "", GlobalRegion: String = "", Territory: String = "")
  case class Country(Area: Option[String] = None, SalesTeam: Option[String] = None, SubRegion: Option[String] = None,
                     Region: Option[String] = None, GlobalRegion: Option[String] = None, Territory: Option[String] = None)
  //val countryTest = Country(None, Some("STeam"), Some("SBReg"), Some("Regs"), Some("Greg"), Some("MyTerr"))
  val countryTest = Country(GlobalRegion = Some("SBReg"))

  val sObject = "pmdf"
  val element = "GlobalRegion"

  //val countries = Source.fromFile(args(0)).getLines.map(_.replaceAll("\"", "")).filter(_.startsWith("Ang")).toList
  val countries = Source.fromFile(args(0)).getLines.map(_.replaceAll("\"", "")).toList
  //for (name <- countries) println(name)

//  def mkJson2(country: String, section: String): (String, JObject) = {
//    country -> (section -> decompose(countryTest))
//  }
//  val genTest2 = countries.map(c => mkJson2(c, "pmdf")).toMap
//  println("genTest2:" + genTest2.getClass)
//  println(pretty(render(decompose(genTest2))))

  def mkJson(country: String, section: String): JObject = {
    section -> decompose(countryTest)
  }

  val genTest = ListMap(countries.map(c => (c, mkJson(c, sObject))): _*)
  println(pretty(render(decompose(genTest))))

  val countriesSection = args(1)
  val countryString = Source.fromFile(countriesSection).getLines.map(_.replaceAll("[ ]{2,20}", ":"))

  val map = countryString.map(splitToTuple(_, ":")).toMap
  println(map.size)
  //for (name <- map) println(name)
  println(map.get("Macau").get)

  def splitToTuple(s: String, splt: String): (String, String) = s.split(splt) match {
    case Array(x: String, y: String) => Tuple2(y, x)
  }
}

