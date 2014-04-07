package appia.lib

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.meta.MTable

import appia.Node

trait Postgres extends Node {

  override type Context = Session

  type Row
  type Tbl <: Table[Row]

  def q: TableQuery[Tbl]

  override def done(session: Session) = {
    implicit val s = session
    val tbl = q.baseTableRow.tableName
    MTable.getTables(tbl).firstOption.isDefined
  }

}