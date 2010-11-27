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
import javax.time.calendar.ZoneOffset

/**
 * Prints or parses a zone offset.
 * <p>
 * ZoneOffsetPrinterParser is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */

/**
 * Constructor.
 *
 * @param utcText the text to use for UTC, not null
 * @param includeColon whether to include a colon
 * @param allowSeconds whether to allow seconds
 */
final class ZoneOffsetPrinterParser private[format](utcText: String, includeColon: Boolean, allowSeconds: Boolean)
  extends DateTimePrinter with DateTimeParser {

  /** { @inheritDoc }*/
  override def isPrintDataAvailable(calendrical: Calendrical): Boolean = (calendrical.get(ZoneOffset.rule) != null)

  /** { @inheritDoc }*/
  override def toString: String = {
    if (utcText.equals("Z") && includeColon && allowSeconds) return "OffsetId()"
    var converted: String = utcText.replace("'", "''")
    return "Offset('" + converted + "'," + includeColon + "," + allowSeconds + ")"
  }

  /**
   * Parse a two digit zero-prefixed number.
   *
   * @param array the array of parsed data, 0=pos,1=hours,2=mins,3=secs, not null
   * @param arrayIndex the index to parse the value into
   * @param parseText the offset id, not null
   * @param required whether this number is required
   * @return true if an error occurred
   */
  private def parseNumber(array: Array[Int], arrayIndex: Int, parseText: String, required: Boolean): Boolean = {
    if (allowSeconds == false && arrayIndex == 3) {
      return false
    }
    var pos: Int = array(0)
    if (includeColon && arrayIndex > 1) {
      if (pos + 1 > parseText.length || parseText.charAt(pos) != ':') {
        return required
      }
      ({
        pos += 1;
        pos
      })
    }
    if (pos + 2 > parseText.length) {
      return required
    }
    var ch1: Char = parseText.charAt(({
      pos += 1;
      pos
    }))
    var ch2: Char = parseText.charAt(({
      pos += 1;
      pos
    }))
    if (ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9') {
      return required
    }
    var value: Int = (ch1 - 48) * 10 + (ch2 - 48)
    if (value < 0 || value > 59) {
      return required
    }
    array(arrayIndex) = value
    array(0) = pos
    return false
  }

  /** { @inheritDoc }*/
  override def print(calendrical: Calendrical, appendable: Appendable, symbols: DateTimeFormatSymbols): Unit = {
    val offset: ZoneOffset = calendrical.get(ZoneOffset.rule).getOrElse(throw new CalendricalPrintException("Unable to print ZoneOffset"))

    var totalSecs: Int = offset.getAmountSeconds
    if (totalSecs == 0) {
      appendable.append(utcText)
    }
    else if (includeColon && (allowSeconds || offset.getSecondsField == 0)) {
      appendable.append(offset.getID)
    }
    else {
      var absHours: Int = Math.abs(offset.getHoursField)
      var absMinutes: Int = Math.abs(offset.getMinutesField)
      var absSeconds: Int = Math.abs(offset.getSecondsField)
      appendable.append(if (totalSecs < 0) "-" else "+").append((absHours / 10 + '0').asInstanceOf[Char]).append((absHours % 10 + '0').asInstanceOf[Char]).append(if (includeColon) ":" else "").append((absMinutes / 10 + '0').asInstanceOf[Char]).append((absMinutes % 10 + '0').asInstanceOf[Char])
      if (allowSeconds && absSeconds > 0) {
        appendable.append(if (includeColon) ":" else "").append((absSeconds / 10 + '0').asInstanceOf[Char]).append((absSeconds % 10 + '0').asInstanceOf[Char])
      }
    }
  }

  /** { @inheritDoc }*/
  def parse(context: DateTimeParseContext, parseText: String, position: Int): Int = {
    var offset: ZoneOffset = null
    var length: Int = parseText.length
    var utcLen: Int = utcText.length
    if (utcLen == 0) {
      if (position == length) {
        context.setParsed(ZoneOffset.rule, ZoneOffset.UTC)
        return position
      }
    }
    else {
      if (position == length) {
        return ~position
      }
      if (parseText.regionMatches(!context.isCaseSensitive, position, utcText, 0, utcLen)) {
        context.setParsed(ZoneOffset.rule, ZoneOffset.UTC)
        return position + utcLen
      }
    }
    var sign: Char = parseText.charAt(position)
    if (sign == '+' || sign == '-') {
      var negative: Int = (if (sign == '-') -1 else 1)
      var array: Array[Int] = new Array[Int](4)
      array(0) = position + 1
      if (parseNumber(array, 1, parseText, true) || parseNumber(array, 2, parseText, true) || parseNumber(array, 3, parseText, false)) {
        return ~position
      }
      var total: Int = (array(1) * 60 * 60) + (array(2) * 60) + array(3)
      if (total > 18 * 60 * 60) {
        return ~position
      }
      offset = ZoneOffset.ofHoursMinutesSeconds(negative * array(1), negative * array(2), negative * array(3))
      context.setParsed(ZoneOffset.rule, offset)
      return array(0)
    }
    else {
      if (utcLen == 0) {
        context.setParsed(ZoneOffset.rule, ZoneOffset.UTC)
        return position + utcLen
      }
      return ~position
    }
  }
}