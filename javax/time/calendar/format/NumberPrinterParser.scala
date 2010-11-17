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

import java.io.IOException
import javax.time.calendar.Calendrical
import javax.time.calendar.DateTimeFieldRule
import javax.time.calendar.format.DateTimeFormatterBuilder.SignStyle

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
  private[format] val EXCEED_POINTS: Array[Int] = Array[Int](0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)
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
class NumberPrinterParser private(rule: DateTimeFieldRule[_], minWidth: Int, maxWidth: Int, signStyle: SignStyle, subsequentWidth: Int)
  extends DateTimePrinter with DateTimeParser {

  /**
   * Constructor.
   *
   * @param rule the rule of the field to print, not null
   * @param minWidth the minimum field width, from 1 to 10
   * @param maxWidth the maximum field width, from minWidth to 10
   * @param signStyle the positive/negative sign style, not null
   */
  private[format] def this(rule: DateTimeFieldRule[_], minWidth: Int, maxWidth: Int, signStyle: SignStyle.type) {
    this (rule, minWidth, maxWidth, signStyle, 0)
  }

  /** { @inheritDoc }*/
  def print(calendrical: Calendrical, appendable: Appendable, symbols: DateTimeFormatSymbols): Unit = {
    var value: Int = getValue(calendrical)
    var str: String = (if (value == Integer.MIN_VALUE) "2147483648" else Integer.toString(Math.abs(value)))
    if (str.length > maxWidth) {
      throw new CalendricalPrintFieldException(rule, value, maxWidth)
    }
    str = symbols.convertNumberToI18N(str)
    if (value >= 0) {
      signStyle match {
        case EXCEEDS_PAD =>
          if (minWidth < 10 && value >= EXCEED_POINTS(minWidth)) {
            appendable.append(symbols.getPositiveSignChar)
          }
        case ALWAYS =>
          appendable.append(symbols.getPositiveSignChar)
      }
    }
    else {
      signStyle match {
        case NORMAL || EXCEEDS_PAD || ALWAYS =>
          appendable.append(symbols.getNegativeSignChar)
        case NOT_NEGATIVE =>
          throw new CalendricalPrintFieldException(rule, value)
      }
    }
    {
      var i: Int = 0
      while (i < minWidth - str.length) {
        {
          appendable.append(symbols.getZeroChar)
        }
        ({
          i += 1;
          i
        })
      }
    }
    appendable.append(str)
  }

  /** { @inheritDoc }*/
  override def isPrintDataAvailable(calendrical: Calendrical): Boolean = {
    return calendrical.get(rule) != null
  }

  /** { @inheritDoc }*/
  override def toString: String = {
    if (minWidth == 1 && maxWidth == 10 && signStyle == SignStyle.NORMAL) {
      return "Value(" + rule.getID + ")"
    }
    if (minWidth == maxWidth && signStyle == SignStyle.NOT_NEGATIVE) {
      return "Value(" + rule.getID + "," + minWidth + ")"
    }
    return "Value(" + rule.getID + "," + minWidth + "," + maxWidth + "," + signStyle + ")"
  }

  /** { @inheritDoc }*/
  def parse(context: DateTimeParseContext, parseText: String, position: Int): Int = {
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
        case ALWAYS =>
        case EXCEEDS_PAD =>
          ({
            position += 1;
            position
          })
          break //todo: break is not supported
        case _ =>
          if (context.isStrict || (signStyle != SignStyle.NORMAL && minWidth == maxWidth)) {
            return ~position
          }
          ({
            position += 1;
            position
          })
          break //todo: break is not supported
      }
    }
    else if (sign == context.getSymbols.getNegativeSignChar) {
      negative = true
      signStyle match {
        case ALWAYS =>
        case EXCEEDS_PAD =>
        case NORMAL =>
          ({
            position += 1;
            position
          })
          break //todo: break is not supported
        case _ =>
          if (context.isStrict || minWidth == maxWidth) {
            return ~position
          }
          ({
            position += 1;
            position
          })
          break //todo: break is not supported
      }
    }
    else {
      if (signStyle == SignStyle.ALWAYS && context.isStrict) {
        return ~position
      }
    }
    var minEndPos: Int = position + minWidth
    if (minEndPos > length) {
      return ~position
    }
    var effMaxWidth: Int = maxWidth + subsequentWidth
    var total: Long = 0
    var pos: Int = position

    {
      var pass: Int = 0
      while (pass < 2) {
        {
          var maxEndPos: Int = Math.min(pos + effMaxWidth, length)
          while (pos < maxEndPos) {
            var ch: Char = parseText.charAt(({
              pos += 1;
              pos
            }))
            var digit: Int = context.getSymbols.convertToDigit(ch)
            if (digit < 0) {
              ({
                pos -= 1;
                pos
              })
              if (pos < minEndPos) {
                return ~position
              }
              break //todo: break is not supported
            }
            total = total * 10 + digit
          }
          if (subsequentWidth > 0 && pass == 0) {
            var parseLen: Int = pos - position
            effMaxWidth = Math.max(minWidth, parseLen - subsequentWidth)
            pos = position
            total = 0
          }
          else {
            break //todo: break is not supported
          }
        }
        ({
          pass += 1;
          pass
        })
      }
    }
    if (negative) {
      if (total == 0) {
        return ~(position - 1)
      }
      total = -total
    }
    else if (signStyle == SignStyle.EXCEEDS_PAD && context.isStrict) {
      var parseLen: Int = pos - position
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
    if (total > Integer.MAX_VALUE || total < Integer.MIN_VALUE) {
      total /= 10
      ({
        pos -= 1;
        pos
      })
    }
    setValue(context, total)
    return pos
  }

  /**
   * Gets the value to output.
   * @param calendrical the calendrical, not null
   * @return the value
   */
  private[format] def getValue(calendrical: Calendrical): Int = {
    return rule.getInt(calendrical)
  }

  /**
   * Stores the value.
   * @param context the context to store into, not null
   * @param value the value
   */
  private[format] def setValue(context: DateTimeParseContext, value: Long): Unit = {
    context.setParsed(rule, value.asInstanceOf[Int])
  }

  /**
   * Returns a new instance with an updated subsequent width.
   *
   * @param subsequentWidth the width of subsequent non-negative numbers, 0 or greater
   * @return a new updated printer-parser, never null
   */
  private[format] def withSubsequentWidth(subsequentWidth: Int): NumberPrinterParser = {
    return new NumberPrinterParser(rule, minWidth, maxWidth, signStyle, this.subsequentWidth + subsequentWidth)
  }
}