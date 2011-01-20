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
package javax.time.calendar

/**
 * Context for aspects of date-time calculations that frequently change.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */

/**
 * Constructs an instance that can merge the specified calendrical.
 *
 * @param strict whether to use strict rules
 * @param checkUnusedFields whether to check unused fields
 */
@SerialVersionUID(1L)
final class CalendricalContext(val isStrict: Boolean, val checkUnusedFields: Boolean) extends Serializable {

  /**
   * Whether to use a date resolver for resolving dates.
   */
  val dateResolver: DateResolver = null

  /**
   * A hashcode for this context.
   *
   * @return a suitable hashcode
   */
  override def hashCode: Int =
    (if (isStrict) 1 else 0) + (if (dateResolver == null) 0
    else dateResolver.hashCode) + (if (checkUnusedFields) 1 else 0)

  /**
   * Is this context equal to the specified context.
   *
   * @param obj the other context to compare to, null returns false
   * @return true if this instance is equal to the specified context
   */
  override def equals(obj: Any): Boolean = {
    obj match {
      case other: CalendricalContext => { (this eq other) ||
        (isStrict == other.isStrict && (dateResolver == other.dateResolver || (dateResolver != null && dateResolver.equals(other.dateResolver))) && checkUnusedFields == other.checkUnusedFields)
             }
      case _ => false
    }
//    if (obj eq this) true
//    else if (obj.isInstanceOf[CalendricalContext]) {
//      val other: CalendricalContext = obj.asInstanceOf[CalendricalContext]
//      isStrict == other.isStrict && (dateResolver == other.dateResolver || (dateResolver != null && dateResolver.equals(other.dateResolver))) && checkUnusedFields == other.checkUnusedFields
//    }
//    else false
  }

  /**
   * Resolves the year, month and day-of-month to a date using ISO chronology rules.
   * <p>
   * The three input parameters are resolved to a date.
   * If the context specifies a date resolver, then that is used.
   * Otherwise, the strict/lenient flag is used.
   *
   * @param year the year to resolve
   * @param month the month to resolve
   * @param dayOfMonth the day-of-month to resolve
   * @return the resolved date, never null
   * @throws IllegalCalendarFieldValueException if one of the fields has an invalid value
   * @throws CalendricalException if the input date does not match the stored date
   */
  def resolveDate(year: Int, month: Int, dayOfMonth: Int): LocalDate = {
    if (dateResolver != null) {
      ISOChronology.yearRule.checkValue(year)
      ISOChronology.dayOfMonthRule.checkValue(dayOfMonth)
      return dateResolver.resolveDate(year, MonthOfYear.of(month), dayOfMonth)
    }
    if (isStrict) {
      return LocalDate(year, month, dayOfMonth)
    }
    if (month >= 1 && month <= 12) {
      if (dayOfMonth >= 1 && dayOfMonth <= 28) {
        return LocalDate(year, month, dayOfMonth)
      }
      return LocalDate(year, month, 1).plusDays((dayOfMonth.toLong) - 1)
    }
    return LocalDate(year, 1, 1).plusMonths(month).plusMonths(-1).plusDays((dayOfMonth.toLong) - 1)
  }
}