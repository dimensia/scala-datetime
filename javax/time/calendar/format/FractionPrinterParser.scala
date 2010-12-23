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

import java.math.BigDecimal
import java.math.RoundingMode
import javax.time.calendar.Calendrical
import javax.time.calendar.DateTimeFieldRule

/**
 * Prints and parses a numeric date-time field with optional padding.
 * <p>
 * FractionPrinterParser is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */

/**
 * Constructor.
 *
 * @param rule the rule to output, not null
 * @param minWidth the minimum width to output, from 0 to 9
 * @param maxWidth the maximum width to output, from 0 to 9
 */
final class FractionPrinterParser private[format](val rule: DateTimeFieldRule[_], val minWidth: Int, val maxWidth: Int)
        extends DateTimePrinter with DateTimeParser {

  /**{@inheritDoc}*/
  override def toString: String = "Fraction(" + rule.getID + "," + minWidth + "," + maxWidth + ")"

  /**{@inheritDoc}*/
  override def print(calendrical: Calendrical, appendable: Appendable, symbols: DateTimeFormatSymbols): Unit = {
    var value: Int = rule.getInt(calendrical)
    var fraction: BigDecimal = rule.convertIntToFraction(value)
    if (fraction.scale == 0) {
      if (minWidth > 0) {
        appendable.append(symbols.getDecimalPointChar)

        var i: Int = 0
        while (i < minWidth) {
          appendable.append(symbols.getZeroChar)
          i += 1;
        }
      }
    }
    else {
      var outputScale: Int = Math.min(Math.max(fraction.scale, minWidth), maxWidth)
      fraction = fraction.setScale(outputScale, RoundingMode.FLOOR)
      var str: String = fraction.toPlainString.substring(2)
      str = symbols.convertNumberToI18N(str)
      appendable.append(symbols.getDecimalPointChar)
      appendable.append(str)
    }
  }

  /**{@inheritDoc}*/
  override def isPrintDataAvailable(calendrical: Calendrical): Boolean = calendrical.get(rule) != null

  /**{@inheritDoc}*/
  def parse(context: DateTimeParseContext, parseText: String, _position: Int): Int = {
    var position = _position
    var length: Int = parseText.length
    if (position == length) {
      if (minWidth > 0) {
        return ~position
      }
      return position
    }
    var point: Char = parseText.charAt(position)
    if (point != context.getSymbols.getDecimalPointChar) {
      if (minWidth > 0) {
        return ~position
      }
      return position
    }
    position += 1;
    var minEndPos: Int = position + minWidth
    if (minEndPos > length) {
      return ~position
    }
    var maxEndPos: Int = Math.min(position + maxWidth, length)
    var total: Int = 0
    var pos: Int = position
    while (pos < maxEndPos) {
      var ch: Char = parseText.charAt(({
        pos += 1;
        pos
      }))
      var digit: Int = context.getSymbols.convertToDigit(ch)
      if (digit < 0) {
        if (pos < minEndPos) {
          return ~position
        }
        pos -= 1;
        //break //todo: break is not supported
      }
      total = total * 10 + digit
    }
    var fraction: BigDecimal = new BigDecimal(total).movePointLeft(pos - position)
    var value: Int = rule.convertFractionToInt(fraction)
    context.setParsed(rule, value)
    return pos
  }
}