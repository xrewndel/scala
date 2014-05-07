import java.io.PrintWriter
import scalikejdbc._
import SQLInterpolation._

/**
 * Created by Andrew on 05.05.14.
 *
 * Class for processing sequence (read, update).
 *
 * According to documentation, terminal sends 0 for first time initialization as sequence value.
 * Then, terminal sends the received sequence number in subsequent request.
 * EWS only allows one initialization per terminal. It can be reset by server administrator manually.
 * So we need to store received sequence for next request. Sequence stored in SQLite.
 * There is only one thread can send request at time, so we need synchronized access to it's value.
 */
class Sequence(db: String) {
  Class.forName("org.sqlite.JDBC")
  ConnectionPool.singleton("jdbc:sqlite:" + db, null, null)
  implicit val session = AutoSession

  // check if database exist. if not - create it, and table also
  def checkDB() = {
    if (!new java.io.File(db).exists()) {
      println("Create DB")
      val out = new PrintWriter(db)
      out.close()
      println("Create table")
      createTable
    }
  }

  // check if table exists
  //private def tableExist(): Boolean =
  //  sql"select name from sqlite_master WHERE type='table' AND name=kv".map(_.string("name")).single().apply() match {
  //  case s:Some[String] => true
  //  case _ => false
  //}

  // create table and initialize lock with value 0 (unlock)
  private def createTable = {
    sql"create table kv (sequence varchar(64), lock integer not null primary key)".execute.apply()
    sql"insert into kv (lock) values (0)".update.apply()
  }

  // SQL like
  /*
  private def unlock = sql"update kv set lock = 0 where lock = 1".update.apply()
  private def lock = sql"update kv set lock = 1 where lock = 0".update.apply()
  def set(newSeq: String) = {
    sql"update kv set sequence = ${newSeq} where lock = 1".update.apply()
    unlock
  }

  def get(): String = {
    lock
    sql"select sequence from kv WHERE lock = 1".map(_.string("sequence")).single().apply() match {
      case s:Some[String] => s.get
      case _ => throw new RuntimeException("Sequence not found")
    }
  }*/


  // DSL like
  // define a class to map the result
  case class Sqn(sequence: String, lock: Int)
  // QueryDSL
  object Sqn extends SQLSyntaxSupport[Sqn] {
    override val tableName = "kv"
    override val columns = Seq("sequence", "lock")

    def apply(e: ResultName[Sqn])(rs: WrappedResultSet): Sqn =
      new Sqn(sequence = rs.get(e.sequence), lock = rs.get(e.lock))
  }

  val column = Sqn.column
  private def unlock = withSQL { update(Sqn).set(column.lock -> 0).where.eq(column.lock, 1) }.update.apply()
  private def lock = withSQL { update(Sqn).set(column.lock -> 1).where.eq(column.lock, 0) }.update.apply()
  def set(newSeq: String) = {
    withSQL { update(Sqn).set(column.sequence -> newSeq).where.eq(column.lock, 1) }.update.apply()
    unlock
  }

  def get(): String = {
    lock
    val s = Sqn.syntax("s")
    withSQL { select(s.result.sequence).from(Sqn as s).where.eq(column.lock, 1) }
      .map(rs => rs.string(s.resultName.sequence)).single.apply() match {
      case s:Some[String] => s.get
      case _ => throw new RuntimeException("Sequence not found")
    }
  }

  def isLocked(): Boolean = {
    val s = Sqn.syntax("s")
    withSQL { select(s.result.lock).from(Sqn as s) }
      .map(rs => rs.string(s.resultName.lock)).single.apply() match {
      case s:Some[String] => s.get.toInt.equals(1)  // 1 => locked
      case _ => throw new RuntimeException("Lock not found")
    }
  }
}
