package appia.lib

import appia.Node

trait FlatFile extends Node {

  override type Context >: Nothing

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

  override def done(c: Context): Boolean = true

}