/*
 * Copyright (c) 2010, Stephen Colebourne & Michael Nascimento Santos
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

import scalax.time.CalendricalException
import scalax.time.calendar.Calendrical
import scalax.time.calendar.DateTimeFieldRule
import scalax.time.calendar.format.DateTimeFormatterBuilder.SignStyle

/**
 * Prints and parses a reduced numeric date-time field.
 * <p>
 * ReducedPrinterParser is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */

/**
 * Constructor.
 *
 * @param rule the rule of the field to print, validated not null
 * @param width the field width, from 1 to 9
 * @param baseValue the base value
 */

final class ReducedPrinterParser private[format](rule: DateTimeFieldRule[_], width: Int, baseValue: Int)
  extends NumberPrinterParser(rule, width, width, SignStyle.NotNegative, 0) {
  //super(rule, width, width, SignStyle.NotNegative)
  if (width < 1 || width > 9) {
    throw new IllegalArgumentException("The width must be from 1 to 9 inclusive but was " + width)
  }
  if (baseValue > rule.getMaximumValue) {
    throw new IllegalArgumentException("The base value must be within the range of the field")
  }
  this.range = NumberPrinterParser.ExceedPoints(width)
  if (((baseValue.toLong) + range) > Int.MaxValue) {
    throw new CalendricalException("Unable to add printer-parser as the range exceeds the capacity of an int")
  }


  /**{@inheritDoc}*/
  override def toString: String = "ReducedValue(" + rule.getID + "," + minWidth + "," + baseValue + ")"

  /**{@inheritDoc}*/
  private[format] override def getValue(calendrical: Calendrical): Int = {
    val value: Int = rule.getInt(calendrical)
    return math.abs(value % range)
  }

  private[format] override def setValue(context: DateTimeParseContext, _value: Long): Unit = {
    var value = _value
    val lastPart: Int = baseValue % range
    if (baseValue > 0) {
      value = baseValue - lastPart + value
    }
    else {
      value = baseValue - lastPart - value
    }
    if (value < baseValue) {
      value += range
    }
    //    context.setParsed(rule, value.toInt) //FIXME
    throw new Exception("Not implemented!") //FIXME
  }

  /**
   * The range.
   */
  private var range: Int = 0
}