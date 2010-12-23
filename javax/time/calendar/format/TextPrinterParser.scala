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
import javax.time.calendar.DateTimeFieldRule.TextStore
import javax.time.calendar.format.DateTimeFormatterBuilder.SignStyle
import javax.time.calendar.format.DateTimeFormatterBuilder.TextStyle

/**
 * Prints or parses field text.
 * <p>
 * TextPrinterParser is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */

/**
 * Constructor.
 *
 * @param rule the rule to output, not null
 * @param textStyle the text style, not null
 */
final class TextPrinterParser private[format](rule: DateTimeFieldRule[_], textStyle: DateTimeFormatterBuilder.TextStyle)
  extends DateTimePrinter with DateTimeParser {

  /**{@inheritDoc}*/
  override def toString: String = if (textStyle == TextStyle.Full) "Text(" + rule.getID + ")"
  else return "Text(" + rule.getID + "," + textStyle + ")"

  /**{@inheritDoc}*/
  def parse(context: DateTimeParseContext, parseText: String, _position: Int): Int = {
    var position = _position
    var length: Int = parseText.length
    if (position > length) {
      throw new IndexOutOfBoundsException
    }
    if (context.isStrict) {
      var textStore: DateTimeFieldRule.TextStore = rule.getTextStore(context.getLocale, textStyle)
      if (textStore != null) {
        var matched: Long = textStore.matchText(!context.isCaseSensitive, parseText.substring(position))

        if (matched == 0) {
          return ~position
        }
        else if (matched > 0) {
          position += (matched >>> 32).toInt
          context.setParsed(rule, matched.toInt)
          return position
        }
      }

      return numberPrinterParser.parse(context, parseText, position)
    }
    else {
      for (textStyle <- TextStyle.values) {
        var textStore: DateTimeFieldRule.TextStore = rule.getTextStore(context.getLocale, textStyle)
        if (textStore != null) {
          var matched: Long = textStore.matchText(!context.isCaseSensitive, parseText.substring(position))
          if (matched > 0) {
            position += (matched >>> 32).toInt
            context.setParsed(rule, matched.toInt)
            return position
          }
        }
      }

      return numberPrinterParser.parse(context, parseText, position)
    }
  }

  /**
   * The cached number printer parser.
   * Immutable and volatile, so no synchronization needed.
   */
  @volatile
  private var _numberPrinterParser: NumberPrinterParser = null

  /**{@inheritDoc}*/
  override def print(calendrical: Calendrical, appendable: Appendable, symbols: DateTimeFormatSymbols): Unit = {
    var value: Int = rule.getInt(calendrical)
    var textStore: DateTimeFieldRule.TextStore = rule.getTextStore(symbols.getLocale, textStyle)
    var text: String = (if (textStore != null) textStore.getValueText(value) else null)
    if (text != null) {
      appendable.append(text)
    }
    else {
      numberPrinterParser.print(calendrical, appendable, symbols)
    }
  }

  /**
   * Create and cache a number printer parser.
   * @return the number printer parser for this field, never null
   */
  private def numberPrinterParser: NumberPrinterParser = {
    if (_numberPrinterParser == null) _numberPrinterParser = new NumberPrinterParser(rule, 1, 10, SignStyle.Normal)
    _numberPrinterParser
  }

  /**{@inheritDoc}*/
  override def isPrintDataAvailable(calendrical: Calendrical): Boolean = calendrical.get(rule) != null
}