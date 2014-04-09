package appia.lib

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.meta.MTable

import appia.api.Node

trait PostgresTable extends Node[Session] {

  type Row
  type Tbl <: Table[Row]

  def q: TableQuery[Tbl]

  override def done(session: Session) = {
    implicit val implicitSession = session
    val tbl = q.baseTableRow.tableName
    MTable.getTables(tbl).firstOption.isDefined
  }

  def construct(rows: TraversableOnce[Row], bufSize: Int = 100000)(implicit session: Session) {
    q.ddl.create
    rows.toIterator.grouped(bufSize).foreach(q.insertAll)
  }

}