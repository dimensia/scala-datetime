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

import scala.util.control.Breaks._
import javax.time.calendar.Calendrical
import javax.time.calendar.DateTimeFieldRule
import javax.time.calendar.format.DateTimeFormatterBuilder.SignStyle
import javax.time.calendar.format.DateTimeFormatterBuilder.SignStyle._

/**
 * Prints and parses a numeric date-time field with optional padding.
 * <p>
 * NumberPrinterParser is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object NumberPrinterParser {
  /**
   * Array of 10 to the power of n
   */
  private[format] val ExceedPoints: Array[Int] = Array[Int](0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)
}


/**
 * Constructor.
 *
 * @param rule the rule of the field to print, not null
 * @param minWidth the minimum field width, from 1 to 10
 * @param maxWidth the maximum field width, from minWidth to 10
 * @param signStyle the positive/negative sign style, not null
 * @param subsequentWidth the width of subsequent non-negative numbers, 0 or greater
 */
class NumberPrinterParser private[format](rule: DateTimeFieldRule[_], val minWidth: Int, maxWidth: Int, signStyle: SignStyle, subsequentWidth: Int = 0)
  extends DateTimePrinter with DateTimeParser {

  /**{@inheritDoc}*/
  def print(calendrical: Calendrical, appendable: Appendable, symbols: DateTimeFormatSymbols): Unit = {
    var value: Int = getValue(calendrical)
    var str: String = (if (value == Int.MinValue) "2147483648" else math.abs(value).toString)
    if (str.length > maxWidth) {
      throw new CalendricalPrintFieldException(rule, value, maxWidth)
    }
    str = symbols.convertNumberToI18N(str)
    if (value >= 0) {
      signStyle match {
        case ExceedsPad =>
          if (minWidth < 10 && value >= NumberPrinterParser.ExceedPoints(minWidth)) {
            appendable.append(symbols.getPositiveSignChar)
          }
        case Always =>
          appendable.append(symbols.getPositiveSignChar)
      }
    }
    else {
      signStyle match {
        case Normal | ExceedsPad | Always =>
          appendable.append(symbols.getNegativeSignChar)
        case NotNegative =>
          throw new CalendricalPrintFieldException(rule, value)
      }
    }

    var i: Int = 0
    while (i < minWidth - str.length) {
      appendable.append(symbols.getZeroChar)
      i += 1;
    }

    appendable.append(str)
  }

  /**{@inheritDoc}*/
  override def isPrintDataAvailable(calendrical: Calendrical): Boolean = calendrical.get(rule) != null

  /**{@inheritDoc}*/
  override def toString: String = {
    if (minWidth == 1 && maxWidth == 10 && signStyle == SignStyle.Normal) {
      return "Value(" + rule.getID + ")"
    }
    if (minWidth == maxWidth && signStyle == SignStyle.NotNegative) {
      return "Value(" + rule.getID + "," + minWidth + ")"
    }
    return "Value(" + rule.getID + "," + minWidth + "," + maxWidth + "," + signStyle + ")"
  }

  /**{@inheritDoc}*/
  def parse(context: DateTimeParseContext, parseText: String, _position: Int): Int = {
    var position = _position
    var length: Int = parseText.length
    if (position == length) {
      return ~position
    }
    var sign: Char = parseText.charAt(position)
    var negative: Boolean = false
    var positive: Boolean = false
    if (sign == context.getSymbols.getPositiveSignChar) {
      positive = true
      signStyle match {
        case Always | ExceedsPad => position += 1
        case _ =>
          if (context.isStrict || (signStyle != SignStyle.Normal && minWidth == maxWidth)) return ~position
          position += 1
      }
    }
    else if (sign == context.getSymbols.getNegativeSignChar) {
      negative = true
      signStyle match {
        case Always | ExceedsPad | Normal => position += 1
        case _ =>
          if (context.isStrict || minWidth == maxWidth) return ~position
          position += 1;
      }
    }
    else {
      if (signStyle == SignStyle.Always && context.isStrict) {
        return ~position
      }
    }
    val minEndPos: Int = position + minWidth
    if (minEndPos > length) {
      return ~position
    }
    var effMaxWidth: Int = maxWidth + subsequentWidth
    var total: Long = 0
    var pos: Int = position

    var pass: Int = 0
    breakable{
      while (pass < 2) {
        val maxEndPos: Int = math.min(pos + effMaxWidth, length)
        breakable{
          while (pos < maxEndPos) {
            val ch: Char = parseText.charAt(({
              pos += 1;
              pos
            }))
            val digit: Int = context.getSymbols.convertToDigit(ch)
            if (digit < 0) {
              pos -= 1;
              if (pos < minEndPos) {
                return ~position
              }
              break
            }
            total = total * 10 + digit
          }
        }
        if (subsequentWidth > 0 && pass == 0) {
          val parseLen: Int = pos - position
          effMaxWidth = math.max(minWidth, parseLen - subsequentWidth)
          pos = position
          total = 0
        }
        else {
          break
        }
        pass += 1;
      }
    }

    if (negative) {
      if (total == 0) {
        return ~(position - 1)
      }
      total = -total
    }
    else if (signStyle == SignStyle.ExceedsPad && context.isStrict) {
      val parseLen: Int = pos - position
      if (positive) {
        if (parseLen <= minWidth) {
          return ~(position - 1)
        }
      }
      else {
        if (parseLen > minWidth) {
          return ~position
        }
      }
    }
    if (total > Int.MaxValue || total < Int.MinValue) {
      total /= 10
      pos -= 1;
    }
    setValue(context, total)
    return pos
  }

  /**
   * Gets the value to output.
   * @param calendrical the calendrical, not null
   * @return the value
   */
  private[format] def getValue(calendrical: Calendrical): Int = rule.getInt(calendrical)

  /**
   * Stores the value.
   * @param context the context to store into, not null
   * @param value the value
   */
  private[format] def setValue(context: DateTimeParseContext, value: Long): Unit = throw new Exception("Not implemented!") //FIXME
  // context.setParsed(rule, value.toInt)

  /**
   * Returns a new instance with an updated subsequent width.
   *
   * @param subsequentWidth the width of subsequent non-negative numbers, 0 or greater
   * @return a new updated printer-parser, never null
   */
  private[format] def withSubsequentWidth(subsequentWidth: Int): NumberPrinterParser =
    new NumberPrinterParser(rule, minWidth, maxWidth, signStyle, this.subsequentWidth + subsequentWidth)
}