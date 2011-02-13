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
import scalax.time.calendar.Calendrical

/**
 * Pads the output to a fixed width.
 * <p>
 * PadPrinterParserDecorator is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */

/**
 * Constructor.
 *
 * @param printer the printer, may be null in which case print() must not be called
 * @param parser the parser, may be null in which case parse() must not be called
 * @param padWidth the width to pad to, 1 or greater
 * @param padChar the pad character
 */
final class PadPrinterParserDecorator private[format](printer: DateTimePrinter, parser: DateTimeParser, padWidth: Int, padChar: Char)
  extends DateTimePrinter with DateTimeParser {
  /**{@inheritDoc}*/
  def parse(context: DateTimeParseContext, _parseText: String, position: Int): Int = {
    var parseText = _parseText
    if (position > parseText.length) {
      throw new IndexOutOfBoundsException
    }
    var endPos: Int = position + padWidth
    if (endPos > parseText.length) return ~position
    var pos: Int = position
    while (pos < endPos && parseText.charAt(pos) == padChar) {
      ({
        pos += 1;
        pos
      })
    }
    parseText = parseText.substring(0, endPos)
    var firstError: Int = 0
    while (pos >= position) {
      var resultPos: Int = parser.parse(context, parseText, pos)
      if (resultPos < 0) {
        if (firstError == 0) {
          firstError = resultPos
        }
        pos -= 1
        //continue //todo: continue is not supported
      }
      if (resultPos != endPos) return ~position
      return resultPos
    }
    return firstError
  }

  /**{@inheritDoc}*/
  override def isPrintDataAvailable(calendrical: Calendrical): Boolean = printer.isPrintDataAvailable(calendrical)

  /**{@inheritDoc}*/
  override def print(calendrical: Calendrical, appendable: Appendable, symbols: DateTimeFormatSymbols): Unit = {
    var buf: java.lang.StringBuilder = new java.lang.StringBuilder(32)
    printer.print(calendrical, buf, symbols)
    var len: Int = buf.length
    if (len > padWidth) {
      throw new CalendricalPrintException("Output of " + len + " characters exceeds pad width of " + padWidth)
    }
    var i: Int = 0
    while (i < padWidth - len) {
      appendable.append(padChar)
      i += 1
    }
    appendable.append(buf)
  }

  /**{@inheritDoc}*/
  override def toString: String = {
    var base: String = "Pad("
    if (printer == parser) base += printer
    else base += (if (printer == null) "" else printer) + "," + (if (parser == null) "" else parser)
    return base + "," + padWidth + (if (padChar == ' ') ")" else ",'" + padChar + "')")
  }
}