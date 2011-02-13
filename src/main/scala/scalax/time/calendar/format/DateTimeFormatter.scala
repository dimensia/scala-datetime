/*
 * Copyright (c) 2008-2010, Stephen Colebourne & Michael Nascimento Santos
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  * Neither the name of JSR-310 nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package scalax.time.calendar.format

import java.io.IOException
import java.text.FieldPosition
import java.text.Format
import java.text.ParseException
import java.text.ParsePosition
import java.util.Locale
import scalax.time.calendar.Calendrical
import scalax.time.calendar.CalendricalMerger
import scalax.time.calendar.CalendricalRule
import scalax.time.calendar.UnsupportedRuleException

/**
 * Formatter for printing and parsing calendricals.
 * <p>
 * This class provides the main application entry point for printing and parsing.
 * Instances of DateTimeFormatter are constructed using DateTimeFormatterBuilder
 * or by using one of the predefined constants on DateTimeFormatters.
 * <p>
 * Some aspects of printing and parsing are dependent on the locale.
 * The locale can be changed using the {@link #withLocale ( Locale )} method
 * which returns a new formatter in the requested locale.
 * <p>
 * Not all formatters can print and parse. Some can only print, while others can only parse.
 * The {@link #isPrintSupported()} and {@link #isParseSupported()} methods determine
 * which operations are available.
 * <p>
 * Some applications may need to use the older {@link Format} class for formatting.
 * The {@link #toFormat()} method returns an implementation of the old API.
 * <p>
 * DateTimeFormatter is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object DateTimeFormatter {
  /**
   * Validates that the input value is not null.
   *
   * @param object the object to check
   * @param errorMessage the error to throw
   * @throws NullPointerException if the object is null
   */
  private[format] def checkNotNull(obj: AnyRef, errorMessage: String): Unit = {
    if (obj == null) throw new NullPointerException(errorMessage)
  }
}


/**
 * Constructor used by immutable copying.
 *
 * @param symbols the symbols to use for text formatting, not null
 * @param asciiNumerics whether to use ASCII numerics (true) or locale numerics (false)
 * @param printerParser the printer/parser to use, not null
 */
final class DateTimeFormatter private[format](symbols: DateTimeFormatSymbols, printerParser: CompositePrinterParser) {

  /**
   * Constructor.
   *
   * @param locale the locale to use for text formatting, not null
   * @param printerParser the printer/parser to use, not null
   */
  def this(locale: Locale, printerParser: CompositePrinterParser) {
    this (DateTimeFormatSymbols.getInstance(locale), printerParser)
  }

  /**
   * Returns this formatter as a {@code java.text.Format} instance.
   * <p>
   * The {@link Format} instance will print any {@link Calendrical}
   * and parses to a merged {@link CalendricalMerger}.
   * <p>
   * The format will throw {@code UnsupportedOperationException} and
   * {@code IndexOutOfBoundsException} in line with those thrown by the
   * {@link #print(Calendrical, Appendable) print} and
   * {@link #parse(String, ParsePosition) parse} methods.
   * <p>
   * The format does not support attributing of the returned format string.
   *
   * @return this formatter as a classic format instance, never null
   */
  def toFormat: Format = new ClassicFormat

  /**
   * Returns a description of the underlying formatters.
   *
   * @return the pattern that will be used, never null
   */
  override def toString: String = {
    val pattern: String = printerParser.toString
    return if (pattern.startsWith("[")) pattern else pattern.substring(1, pattern.length - 1)
  }

  /**
   * Returns the formatter as a composite printer parser.
   *
   * @param optional whether the printer/parser should be optional
   * @return the printer/parser, never null
   */
  private[format] def toPrinterParser(optional: Boolean): CompositePrinterParser = printerParser.withOptional(optional)

  /**
   * Implements the classic Java Format API.
   */
  private[format] class ClassicFormat extends Format {
    /**{@inheritDoc}*/
    def parseObject(source: String, pos: ParsePosition): AnyRef = {
      val context: DateTimeParseContext = parse(source, pos)
      return if (context != null) context.toCalendricalMerger.merge else null
    }

    /**{@inheritDoc}*/
    def format(obj: AnyRef, toAppendTo: StringBuffer, pos: FieldPosition): StringBuffer = {
      DateTimeFormatter.checkNotNull(obj, "Object to be printed must not be null")
      DateTimeFormatter.checkNotNull(toAppendTo, "StringBuffer must not be null")
      DateTimeFormatter.checkNotNull(pos, "FieldPosition must not be null")
      if (!obj.isInstanceOf[Calendrical]) {
        throw new IllegalArgumentException("DateTimeFormatter can format Calendrical instances")
      }
      pos.setBeginIndex(0)
      pos.setEndIndex(0)
      print(obj.asInstanceOf[Calendrical], toAppendTo)
      return toAppendTo
    }

    /**{@inheritDoc}*/
    override def parseObject(source: String): AnyRef = {
      try {
        return parse(source)
      }
      catch {
        case ex: CalendricalParseException => {
          throw new ParseException(ex.getMessage, ex.getErrorIndex)
        }
      }
    }
  }

  /**
   * Parses the text into a Calendrical.
   * <p>
   * The result may be invalid including out of range values such as a month of 65.
   * The methods on the calendrical allow you to handle the invalid input.
   * For example:
   * <pre>
   * DateTime dt = parser.parse(str).mergeStrict().toLocalDateTime();
   * </pre>
   *
   * @param text the text to parse, not null
   * @param position the position to parse from, updated with length parsed
   *  and the index of any error, not null
   * @return the parsed text, null only if the parse results in an error
   * @throws UnsupportedOperationException if this formatter cannot parse
   * @throws NullPointerException if the text or position is null
   * @throws IndexOutOfBoundsException if the position is invalid
   */
  def parse(text: String, position: ParsePosition): DateTimeParseContext = {
    DateTimeFormatter.checkNotNull(text, "Text must not be null")
    DateTimeFormatter.checkNotNull(position, "ParsePosition must not be null")
    var context: DateTimeParseContext = new DateTimeParseContext(symbols)
    var pos: Int = position.getIndex
    pos = printerParser.parse(context, text, pos)
    if (pos < 0) {
      position.setErrorIndex(~pos)
      return null
    }
    position.setIndex(pos)
    return context
  }

  /**
   * Checks whether this formatter can parse.
   * <p>
   * Depending on how this formatter is initialized, it may not be possible
   * for it to parse at all. This method allows the caller to check whether
   * the parse methods will throw UnsupportedOperationException or not.
   *
   * @return true if the formatter supports parsing
   */
  def isParseSupported: Boolean = printerParser.isParseSupported

  /**
   * Prints the calendrical to an Appendable using this formatter.
   * <p>
   * This method prints the calendrical to the specified Appendable.
   * Appendable is a general purpose interface that is implemented by all
   * key character output classes including StringBuffer, StringBuilder,
   * PrintStream and Writer.
   * <p>
   * Although Appendable methods throw an IOException, this method does not.
   * Instead, any IOException is wrapped in a runtime exception.
   * See {@link CalendricalPrintException#rethrowIOException()} for a means
   * to extract the IOException.
   *
   * @param calendrical the calendrical to print, not null
   * @param appendable the appendable to print to, not null
   * @throws UnsupportedOperationException if this formatter cannot print
   * @throws NullPointerException if the calendrical or appendable is null
   * @throws CalendricalPrintException if an error occurs during printing
   */
  def print(calendrical: Calendrical, appendable: Appendable): Unit = {
    DateTimeFormatter.checkNotNull(calendrical, "Calendrical must not be null")
    DateTimeFormatter.checkNotNull(appendable, "Appendable must not be null")
    try {
      printerParser.print(calendrical, appendable, symbols)
    }
    catch {
      case ex: UnsupportedRuleException[_] => {
        throw new CalendricalPrintFieldException(ex)
      }
      case ex: IOException => {
        throw new CalendricalPrintException(ex.getMessage, ex)
      }
    }
  }

  /**
   * Fully parses the text returning a merger that can be used to manage the
   * merging of separate parsed fields to a meaningful calendrical.
   * <p>
   * If the parse completes without reading the entire length of the text,
   * or a problem occurs during parsing, then an exception is thrown.
   * <p>
   * The result may be invalid including out of range values such as a month of 65.
   * The methods on the calendrical allow you to handle the invalid input.
   * For example:
   * <pre>
   * DateTime dt = parser.parse(str).merge().get(DateTime.rule());
   * </pre>
   *
   * @param text the text to parse, not null
   * @return the parsed text, never null
   * @throws UnsupportedOperationException if this formatter cannot parse
   * @throws NullPointerException if the text is null
   * @throws CalendricalParseException if the parse fails
   */
  def parse(text: String): CalendricalMerger = {
    var pos: ParsePosition = new ParsePosition(0)
    var result: DateTimeParseContext = parse(text, pos)
    if (pos.getErrorIndex >= 0 || pos.getIndex < text.length) {
      var str: String = text
      if (str.length > 64) {
        str = str.substring(0, 64) + "..."
      }
      if (pos.getErrorIndex >= 0) {
        throw new CalendricalParseException("Text '" + str + "' could not be parsed at index " + pos.getErrorIndex, text, pos.getErrorIndex)
      }
      else {
        throw new CalendricalParseException("Text '" + str + "' could not be parsed, unparsed text found at index " + pos.getIndex, text, pos.getIndex)
      }
    }
    return result.toCalendricalMerger
  }

  /**
   * Gets the locale to be used during formatting.
   *
   * @return the locale of this DateTimeFormatter, never null
   */
  def getLocale: Locale = symbols.getLocale

  /**
   * Returns a copy of this DateTimeFormatter with a new locale.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param locale the new locale, not null
   * @return a new DateTimeFormatter with the same format and the new locale, never null
   */
  def withLocale(locale: Locale): DateTimeFormatter = {
    DateTimeFormatter.checkNotNull(locale, "Locale must not be null")
    if (locale.equals(this.getLocale)) {
      return this
    }
    var newSymbols: DateTimeFormatSymbols = DateTimeFormatSymbols.getInstance(locale)
    return new DateTimeFormatter(newSymbols, printerParser)
  }

  /**
   * Fully parses the text producing an object of the type defined by the rule.
   * <p>
   * This parses the entire text to produce the required calendrical value.
   * For example:
   * <pre>
   * DateTime dt = parser.parse(str, DateTime.rule());
   * </pre>
   * If the parse completes without reading the entire length of the text,
   * or a problem occurs during parsing or merging, then an exception is thrown.
   *
   * @param text the text to parse, not null
   * @return the parsed date, never null
   * @throws UnsupportedOperationException if this formatter cannot parse
   * @throws NullPointerException if the text is null
   * @throws CalendricalParseException if the parse fails
   */
  def parse[T](text: String, rule: CalendricalRule[T]): T = {
    DateTimeFormatter.checkNotNull(text, "Text must not be null")
    DateTimeFormatter.checkNotNull(rule, "CalendricalRule must not be null")
    var merger: CalendricalMerger = parse(text)
    var result: Option[T] = merger.merge.get(rule)
    result match {
      case Some(result) => result
      case None =>
        var str: String = text
        if (str.length > 64) {
          str = str.substring(0, 64) + "..."
        }
        throw new CalendricalParseException("Text '" + str + "' could not be parsed into " + rule.getName + " but was parsed to " + merger, text, 0)
    }
  }

  /**
   * Checks whether this formatter can print.
   * <p>
   * Depending on how this formatter is initialized, it may not be possible
   * for it to print at all. This method allows the caller to check whether
   * the print methods will throw UnsupportedOperationException or not.
   *
   * @return true if the formatter supports printing
   */
  def isPrintSupported: Boolean = printerParser.isPrintSupported

  /**
   * Prints the calendrical using this formatter.
   * <p>
   * This method prints the calendrical to a String.
   *
   * @param calendrical the calendrical to print, not null
   * @return the printed string, never null
   * @throws UnsupportedOperationException if this formatter cannot print
   * @throws NullPointerException if the calendrical is null
   * @throws CalendricalPrintException if an error occurs during printing
   */
  def print(calendrical: Calendrical): String = {
    var buf: java.lang.StringBuilder = new java.lang.StringBuilder(32)
    this.print(calendrical, buf)
    return buf.toString
  }
}