package db

import com.github.mauricio.async.db.mysql.{MySQLQueryResult, MySQLConnection}
import com.github.mauricio.async.db.{RowData, QueryResult}
import play.api.Logger

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import config.Global


trait DB {
  lazy val pool = Global.pool

  /**
   * Creates a prepared statement with the given query
   * and passes it to the connection pool with given values.
   */
  def execute(query: String, values: Any*): Future[QueryResult] = {
    if (values.size > 0)
      pool.sendPreparedStatement(query, values)
    else
      pool.sendQuery(query)
  }

  /**
   * Creates a prepared statement with the given query
   * and passes it to the connection pool with given values.
   * @return Seq[RowData] of the query
   */
  def fetch(query: String, values: Any*): Future[Option[Seq[RowData]]] =
    execute(query, values: _*).map(_.rows)

  // -- Generic Queries
  def findOneOption[T](stmt: String, params: Seq[Any] = List(), mapper: (RowData) => T): Future[Option[T]] = {
    pool.sendPreparedStatement(stmt, params).map {
      _.rows.get.map(rs => mapper(rs)).collectFirst({case x => x})
    }
  }

  def findOne[T](stmt: String, params: Seq[Any] = List(), mapper: (RowData) => T): Future[T] = {
    pool.sendPreparedStatement(stmt, params).map {
      _.rows.map(rs =>  mapper(rs(0))).get
    }
  }


  def find[T](stmt: String, params: Seq[Any] = List(), mapper: (RowData) => T): Future[IndexedSeq[T]] = {
    pool.sendPreparedStatement(stmt, params).map(_.rows.get.map(item => mapper(item)))
  }

  def findIn[T](stmt: String, params: Seq[Any] = List(), inParams: Map[String,String], mapper: (RowData) => T): Future[IndexedSeq[T]] = {
    var newStmt = stmt
    inParams.map{
      v => newStmt = newStmt.replace(v._1, v._2)
    }
	
    pool.sendPreparedStatement(newStmt, params).map(_.rows.get.map(item => mapper(item)))
  }
  /**
   * Expects a query that returns the count as first column of the resulting row.
   */
  def count[T](stmt: String, params: Seq[Any] = List()): Future[Long] = {
    pool.sendPreparedStatement(stmt, params).map(_.rows.get(0)(0).asInstanceOf[Long])
  }

  def insert[T](stmt: String, params: Seq[Any] = List()):Future[Long] = {
    pool.connect.flatMap{
      connection => connection.inTransaction{
        con =>
          con.sendPreparedStatement(stmt, params).flatMap{r =>
            con.sendQuery("SELECT last_insert_id() as id").map{
              _.rows.get(0)("id").asInstanceOf[Long]
            }
          }
      }
    }
  }
}
