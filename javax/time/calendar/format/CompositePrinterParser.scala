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
package javax.time.calendar.format

import java.util.Arrays
import java.util.List
import javax.time.calendar.Calendrical

/**
 * Composite printer and parser.
 * <p>
 * CompositePrinterParser is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */

/**
 * Constructor.
 *
 * @param printers the printers, may be null in which case print() must not be called
 * @param parsers the parsers, may be null in which case parse() must not be called
 * @param optional whether the print/parse is optional
 */

final class CompositePrinterParser private[format](_printers: List[DateTimePrinter], _parsers: List[DateTimeParser], val optional: Boolean)
  extends DateTimePrinter with DateTimeParser {
  this.printers = if (_printers.contains(null)) null else _printers.toArray(new Array[DateTimePrinter](_printers.size))
  this.parsers = if (_parsers.contains(null)) null else _parsers.toArray(new Array[DateTimeParser](_parsers.size))

  /**
   * The list of printers that will be used, treated as immutable.
   */
  private var printers: Array[DateTimePrinter] = null

  /**
   * The list of parsers that will be used, treated as immutable.
   */
  private var parsers: Array[DateTimeParser] = null

  /**
   * Returns a copy of this printer-parser with the optional flag changed.
   *
   * @param optional the optional flag to set in the copy
   * @return the new printer-parser, never null
   */
  def withOptional(optional: Boolean): CompositePrinterParser = {
    if (optional == this.optional) this
    else new CompositePrinterParser(Arrays.asList(printers: _*), Arrays.asList(parsers: _*), optional)
  }

  /**{@inheritDoc}*/
  override def parse(context: DateTimeParseContext, parseText: String, _position: Int): Int = {
    var position = _position
    if (parsers == null) {
      throw new UnsupportedOperationException("Formatter does not support parsing")
    }
    if (optional) {
      context.startOptional
      var pos: Int = position
      for (parser <- parsers) {
        pos = parser.parse(context, parseText, pos)
        if (pos < 0) {
          context.endOptional(false)
          return position
        }
      }
      context.endOptional(true)
      return pos
    }
    else {
      for (parser <- parsers) {
        position = parser.parse(context, parseText, position)
        if (position < 0) {
          //break //todo: break is not supported
        }
      }
      return position
    }
  }

  /**{@inheritDoc}*/
  override def toString: String = {
    val buf: StringBuilder = new StringBuilder
    if (printers != null) {
      buf.append(if (optional) "[" else "(")
      for (printer <- printers) {
        buf.append(printer)
      }
      buf.append(if (optional) "]" else ")")
    }
    return buf.toString
  }

  /**{@inheritDoc}*/
  override def print(calendrical: Calendrical, appendable: Appendable, symbols: DateTimeFormatSymbols): Unit = {
    if (printers == null) {
      throw new UnsupportedOperationException("Formatter does not support printing")
    }
    if (optional) {
      for (printer <- printers) {
        if (printer.isPrintDataAvailable(calendrical) == false) {
          return
        }
      }
    }
    for (printer <- printers) {
      printer.print(calendrical, appendable, symbols)
    }
  }

  /**{@inheritDoc}*/
  def isParseSupported: Boolean = parsers != null

  /**{@inheritDoc}*/
  def isPrintSupported: Boolean = printers != null

  /**{@inheritDoc}*/
  override def isPrintDataAvailable(calendrical: Calendrical): Boolean = {
    if (optional) {
      return true
    }
    for (printer <- printers) {
      if (printer.isPrintDataAvailable(calendrical) == false) {
        return false
      }
    }
    return true
  }
}