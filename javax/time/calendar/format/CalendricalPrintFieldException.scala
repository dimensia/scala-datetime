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

import javax.time.calendar.CalendricalRule
import javax.time.calendar.DateTimeFieldRule
import javax.time.calendar.UnsupportedRuleException

/**
 * An exception thrown when an error occurs during printing due to a specific rule.
 *
 * @author Stephen Colebourne
 */

/**
 * Constructs a new exception using the specified message.
 *
 * @param fieldRule the rule of the field that caused the exception, may be null
 * @param value the value of the field that caused the exception
 */
@SerialVersionUID(1L)
class CalendricalPrintFieldException(msg: String, rule: DateTimeFieldRule[_], value: Int)
  extends CalendricalPrintException(msg) {

  /**
   * Constructs a new exception creating a standard error message for
   * exceeding padding width.
   *
   * @param fieldRule the rule of the field that caused the exception, may be null
   * @param value the value of the field that caused the exception
   * @param maxWidth the maximum print width
   */
  def this(rule: DateTimeFieldRule[_], value: Int, maxWidth: Int) {
    this ("Rule " + (if (fieldRule == null) "null" else fieldRule.getName) + " cannot be printed as the value " + value + " exceeds the maximum print width of " + maxWidth, rule, value)
  }

  /**
   * Constructs a new exception wrapping the unsupported field exception.
   *
   * @param cause the exception cause, may be null
   */
  def this(cause: UnsupportedRuleException) {
    this ("Rule " + (if (cause.getRule == null) "null" else cause.getRule.getName) + " cannot be printed as the value cannot be obtained", cause.getRule, null)
  }

  /**
   * Constructs a new exception creating a standard error message for
   * unable to print a negative value.
   *
   * @param fieldRule the rule of the field that caused the exception, may be null
   * @param value the value of the field that caused the exception
   */
  def this(fieldRule: DateTimeFieldRule[_], value: Int) {
    this ("Rule " + (if (fieldRule == null) "null" else fieldRule.getName) + " cannot be printed as the value " + value + " cannot be negative according to the SignStyle", fieldRule, value)
  }

  /**
   * Gets the rule that caused the exception.
   *
   * @return the field rule, null if unknown
   */
  def getRule: CalendricalRule[_] = rule

  /**
   * Gets the value of the field that caused the exception.
   *
   * @return the field value, null if unknown
   */
  def getValue: Int = value
}