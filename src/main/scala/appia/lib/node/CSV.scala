package appia.lib.node

import au.com.bytecode.opencsv.CSVParser

trait CSV extends FlatFile {

  override type Record = String
  override type Field = String

  protected def separator = CSVParser.DEFAULT_SEPARATOR
  protected def quote = CSVParser.DEFAULT_QUOTE_CHARACTER
  protected def escape = CSVParser.DEFAULT_ESCAPE_CHARACTER
  protected def strictQuotes = CSVParser.DEFAULT_STRICT_QUOTES
  protected def ignoreLeadingWhitespace = CSVParser.DEFAULT_IGNORE_LEADING_WHITESPACE

  protected val parser = new CSVParser(separator, quote, escape, strictQuotes, ignoreLeadingWhitespace)

  override def split(line: String): TraversableOnce[Seq[String]] = {
    val fields: Seq[String] = parser.parseLine(line)
    Some(fields)
  }

}