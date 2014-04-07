package appia.lib.node

trait FixedWidth extends FlatFile {

  override type Record = String
  override type Field = String

  def widths: Seq[Int]

  override def split(line: String): TraversableOnce[IndexedSeq[String]] = {
    val partialSums = widths.scanLeft(0)(_ + _)
    val splits = for (i <- 1 until partialSums.size) yield (partialSums(i - 1), partialSums(i))
    val ret = for ((start, end) <- splits) yield line.substring(start, end).trim()
    Some(ret)
  }

}