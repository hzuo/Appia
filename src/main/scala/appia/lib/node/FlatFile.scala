package appia.lib.node

import scala.slick.driver.PostgresDriver.simple._

import appia.api.Node

trait FlatFile extends Node[Any] {

  type Record
  type Field
  type Domain

  def split(row: Record): TraversableOnce[Seq[Field]]

  def parse(fields: Seq[Field]): TraversableOnce[Domain]

  def lift(file: TraversableOnce[Record]): Iterator[Domain] = {
    new Iterator[Domain] {
      val forward = {
        for {
          row <- file
          splitted <- split(row)
          parsed <- parse(splitted)
        } yield parsed
      }.toIterator

      def hasNext: Boolean = forward.hasNext
      def next(): Domain = forward.next()
    }
  }

  override def done(c: Any): Boolean = true

}