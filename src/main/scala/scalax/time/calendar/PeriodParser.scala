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
package scalax.time.calendar

import scalax.time.calendar.format.CalendricalParseException
import scala.util.control.Breaks._

/**
 * An period parser that creates an instance of {@code Period} from a string
 * using the ISO8601 period format {@code PnYnMnDTnHnMn.nS}.
 *
 * @author Darryl West
 * @author Stephen Colebourne
 */

/**
 * Gets the singleton instance of the parser.
 *
 * @return the instance of the parser
 */
object PeriodParser extends PeriodParser {

  /**
   * The standard string representing a zero period.
   */
  private val Zero: String = "PT0S"
  /**
   * Used to validate the correct sequence of tokens.
   */
  private val TokenSequence: String = "PYMDTHMS"

  /**
   * Parse values container created for each parse
   * @param text Original text.
   */
  private class ParseValues private[calendar](private[calendar] var text: String) {
    /**
     * Whether the seconds were negative.
     */
    private[calendar] var negativeSecs: Boolean = false
    /**
     * The number of days.
     */
    private[calendar] var days: Int = 0
    /**
     * The number of hours.
     */
    private[calendar] var hours: Int = 0
    /**
     * The number of seconds.
     */
    private[calendar] var seconds: Int = 0
    /**
     * The number of months.
     */
    private[calendar] var months: Int = 0

    /**
     * The number of minutes.
     */
    private[calendar] var minutes: Int = 0
    /**
     * The number of years.
     */
    private[calendar] var years: Int = 0
    /**
     * The number of nanoseconds.
     */
    private[calendar] var nanos: Long = 0L

    /**
     * Parser position index.
     */
    private[calendar] var index: Int = 0

    private[calendar] def toPeriod: Period =
      Period.of(years, months, days, hours, minutes, seconds, if (negativeSecs || seconds < 0) -nanos else nanos)
  }

}

class PeriodParser protected {
  private def parseNanos(values: PeriodParser.ParseValues, s: String, baseIndex: Int): Long = {
    if (s.length > 9) {
      throw new CalendricalParseException("Period could not be parsed, nanosecond range exceeded: " + values.text, values.text, baseIndex + values.index - s.length)
    }
    return (s + "000000000").substring(0, 9).toLong
  }

  private def parseInt(values: PeriodParser.ParseValues, s: String, baseIndex: Int): Int = {
    try {
      var value: Int = s.toInt
      if (s.charAt(0) == '-' && value == 0) {
        throw new CalendricalParseException("Period could not be parsed, invalid number '" + s + "': " + values.text, values.text, baseIndex + values.index - s.length)
      }
      return value
    }
    catch {
      case ex: NumberFormatException => {
        throw new CalendricalParseException("Period could not be parsed, invalid number '" + s + "': " + values.text, values.text, baseIndex + values.index - s.length)
      }
    }
  }

  private def prepareTime(values: PeriodParser.ParseValues, _s: String, baseIndex: Int): String = {
    var s = _s
    if (s.contains('.')) {
      var i: Int = s.indexOf('.') + 1
      if (Character.isDigit(s.charAt(i))) {
        i += 1;
      }
      else {
        throw new CalendricalParseException("Period could not be parsed, invalid decimal number: " + values.text, values.text, baseIndex + values.index)
      }
      while (i < s.length) {
        var c: Char = s.charAt(i)
        if (Character.isDigit(c) || c == 'S') {
          i += 1;
        }
        else {
          throw new CalendricalParseException("Period could not be parsed, invalid decimal number: " + values.text, values.text, baseIndex + values.index)
        }
      }
      s = s.replace('S', 'N').replace('.', 'S')
      if (s.contains("-0S")) {
        values.negativeSecs = true
        s = s.replace("-0S", "0S")
      }
    }
    return s
  }

  private def parseNumber(values: PeriodParser.ParseValues, s: String): String = {
    val start: Int = values.index
    breakable{
      while (values.index < s.length) {
        val c: Char = s.charAt(values.index)
        if ((c < '0' || c > '9') && c != '-') break
        values.index += 1;
      }
    }
    return s.substring(start, values.index)
  }

  private def parseDate(values: PeriodParser.ParseValues, s: String, baseIndex: Int): Unit = {
    values.index = 0
    while (values.index < s.length) {
      val value: String = parseNumber(values, s)
      if (values.index < s.length) {
        val c: Char = s.charAt(values.index)
        c match {
          case 'Y' => values.years = parseInt(values, value, baseIndex)
          case 'M' => values.months = parseInt(values, value, baseIndex)
          case 'D' => values.days = parseInt(values, value, baseIndex)
          case _ => throw new CalendricalParseException("Period could not be parsed, unrecognized letter '" + c + ": " + values.text, values.text, baseIndex + values.index)
        }
        values.index += 1;
      }
    }
  }

  /**
   * Obtains an instance of {@code Period} from a string.
   * <p>
   * This will parse the string produced by {@code toString()} which is
   * a subset of the ISO8601 period format {@code PnYnMnDTnHnMn.nS}.
   * <p>
   * The string consists of a series of numbers with a suffix identifying their meaning.
   * The values, and suffixes, must be in the sequence year, month, day, hour, minute, second.
   * Any of the number/suffix pairs may be omitted providing at least one is present.
   * If the period is zero, the value is normally represented as {@code PT0S}.
   * The numbers must consist of ASCII digits.
   * Any of the numbers may be negative. Negative zero is not accepted.
   * The number of nanoseconds is expressed as an optional fraction of the seconds.
   * There must be at least one digit before any decimal point.
   * There must be between 1 and 9 inclusive digits after any decimal point.
   * The letters will all be accepted in upper or lower case.
   * The decimal point may be either a dot or a comma.
   *
   * @param text the input string in the format PnYnMnDTnHnMn.nS, validated not null
   * @return the created Period, never null
   * @throws CalendricalParseException if the text cannot be parsed to a Period
   */
  private[calendar] def parse(text: String): Period = {
    val s: String = text.toUpperCase.replace(',', '.')
    if (Period.Zero == s) {
      return Period.Zero
    }
    if (s.length < 3 || s.charAt(0) != 'P') {
      throw new CalendricalParseException("Period could not be parsed: " + text, text, 0)
    }
    validateCharactersAndOrdering(s, text)
    var values: PeriodParser.ParseValues = new PeriodParser.ParseValues(text)
    var datetime: Array[String] = s.substring(1).split("T")
    datetime.length match {
      case 2 =>
        parseDate(values, datetime(0), 1)
        parseTime(values, datetime(1), datetime(0).length + 2)
      case 1 => parseDate(values, datetime(0), 1)
    }
    return values.toPeriod
  }

  private def validateCharactersAndOrdering(s: String, text: String): Unit = {
    var chars: Array[Char] = s.toCharArray
    var tokenPos: Int = 0
    var lastLetter: Boolean = false

    var i: Int = 0
    while (i < chars.length) {
      if (tokenPos >= PeriodParser.TokenSequence.length) {
        throw new CalendricalParseException("Period could not be parsed, characters after last 'S': " + text, text, i)
      }
      var c: Char = chars(i)
      if ((c < '0' || c > '9') && c != '-' && c != '.') {
        tokenPos = PeriodParser.TokenSequence.indexOf(c, tokenPos)
        if (tokenPos < 0) {
          throw new CalendricalParseException("Period could not be parsed, invalid character '" + c + "': " + text, text, i)
        }
        tokenPos += 1;
        lastLetter = true
      }
      else {
        lastLetter = false
      }
      i += 1;
    }
    if (lastLetter == false) {
      throw new CalendricalParseException("Period could not be parsed, invalid last character: " + text, text, s.length - 1)
    }
  }

  private def parseTime(values: PeriodParser.ParseValues, _s: String, baseIndex: Int): Unit = {
    var s = _s
    values.index = 0
    s = prepareTime(values, s, baseIndex)
    while (values.index < s.length) {
      var value: String = parseNumber(values, s)
      if (values.index < s.length) {
        var c: Char = s.charAt(values.index)
        c match {
          case 'H' => values.hours = parseInt(values, value, baseIndex)
          case 'M' => values.minutes = parseInt(values, value, baseIndex)
          case 'S' => values.seconds = parseInt(values, value, baseIndex)
          case 'N' => values.nanos = parseNanos(values, value, baseIndex)
          case _ => throw new CalendricalParseException("Period could not be parsed, unrecognized letter '" + c + "': " + values.text, values.text, baseIndex + values.index)
        }
        values.index += 1;
      }
    }
  }
}