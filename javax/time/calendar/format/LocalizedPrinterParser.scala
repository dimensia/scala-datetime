/*
 * Copyright (c) 2009-2010, Stephen Colebourne & Michael Nascimento Santos
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
package javax.time.calendar.format

import java.util.Locale
import javax.time.calendar.Calendrical
import javax.time.calendar.Chronology

/**
 * Prints or parses a localized pattern.
 * <p>
 * LocalizedPrinterParser is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */

/**
 * Constructor.
 *
 * @param dateStyle the date style to use, may be null
 * @param timeStyle the time style to use, may be null
 * @param chronology the chronology to use, not null
 */
final class LocalizedPrinterParser private[format](val dateStyle: DateTimeFormatterBuilder.FormatStyle,
                                                   val timeStyle: DateTimeFormatterBuilder.FormatStyle,
                                                   val chronology: Chronology)
  extends DateTimePrinter with DateTimeParser {

  /**{@inheritDoc}*/
  override def print(calendrical: Calendrical, appendable: Appendable, symbols: DateTimeFormatSymbols): Unit =
    formatter(symbols.getLocale).toPrinterParser(false).print(calendrical, appendable, symbols)

  /**{@inheritDoc}*/
  override def isPrintDataAvailable(calendrical: Calendrical): Boolean = true

  /**{@inheritDoc}*/
  override def toString: String =
    "Localized(" + (if (dateStyle != null) dateStyle else "") + "," +
      (if (timeStyle != null) timeStyle else "") + "," + chronology.getName + ")"

  /**{@inheritDoc}*/
  def parse(context: DateTimeParseContext, parseText: String, position: Int): Int =
    formatter(context.getLocale).toPrinterParser(false).parse(context, parseText, position)


  /**
   * Gets the formatter to use.
   *
   * @param locale the locale to use, not null
   * @return the formatter, never null
   * @throws IllegalArgumentException if the formatter cannot be found
   */
  private def formatter(locale: Locale): DateTimeFormatter =
    (new DateTimeFormatterProviderImpl).getFormatter(dateStyle, timeStyle, locale, chronology)
}