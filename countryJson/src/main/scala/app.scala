import java.io._
import scala.collection.immutable.{ListMap}
import scala.io.Source
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Extraction._

object app extends App {
  implicit val formats = net.liftweb.json.DefaultFormats

  case class Country(Area: Option[String] = None, SalesTeam: Option[String] = None, SubRegion: Option[String] = None,
                     Region: Option[String] = None, GlobalRegion: Option[String] = None, Territory: Option[String] = None)
  //val countryTest = Country(None, Some("STeam"), Some("SBReg"), Some("Regs"), Some("Greg"), Some("MyTerr"))
  //val countryTest = Country(GlobalRegion = Some("SBReg"))

  val sObject = "pmdf"
  val element = "GlobalRegion"
  val outputFile = args(2)

  //// making map Country -> Value to add into JSON
  val countryString = Source.fromFile(args(1)).getLines.map(_.replaceAll("[ ]{2,20}", ":"))

  val map = countryString.map(splitToTuple(_, ":")).toMap
  //for (name <- map) println(name)
  //println(map.get("Macau").get)


  //// making json by finding values in map
  val countries = Source.fromFile(args(0)).getLines.map(_.replaceAll("\"", "")).toList
  //for (name <- countries) println(name)

  val jsonResult = ListMap(countries.map(c => (c, mkJson(Country(GlobalRegion = map.get(c)), sObject))): _*)
  println(pretty(render(decompose(jsonResult))))

  val file = new File(outputFile)
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write(pretty(render(decompose(jsonResult))))
  bw.close()


  def splitToTuple(s: String, splt: String): (String, String) = s.split(splt) match {
    case Array(x: String, y: String) => Tuple2(y, x)
  }

  def mkJson(country: Country, section: String): JObject = {
    section -> decompose(country)
  }
}

