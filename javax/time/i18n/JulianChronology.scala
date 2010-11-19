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
package javax.time.i18n

import java.io.Serializable
import javax.time.Duration
import javax.time.calendar.Chronology
import javax.time.calendar.ISOChronology
import javax.time.calendar.PeriodUnit

/**
 * The Julian calendar system.
 * <p>
 * JulianChronology defines the rules of the Julian calendar system.
 * The Julian calendar was introduced by Julius Caesar in 46 BCE to replace the
 * previous Roman calendar system.
 * <p>
 * The calendar system is the same as the   { @code ISOChronology ISO-8601 } calendar
 * system with the exception of the rule for the leap year. The Julian definition
 * has a leap year every four years without fail.
 * <p>
 * JulianChronology is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object JulianChronology extends JulianChronology {
  /**
   * Validates that the input value is not null.
   *
   * @param object the object to check
   * @param errorMessage the error to throw
   * @throws NullPointerException if the object is null
   */
  private[i18n] def checkNotNull(obj: AnyRef, errorMessage: String): Unit = {
    if (obj == null) throw new NullPointerException(errorMessage)
  }

  object Years extends Years

  @SerialVersionUID(1L)
  private[time] sealed class Years extends PeriodUnit("JulianYears", Duration.ofSeconds(31557600L)) {

    private def readResolve: AnyRef = Years

  }

  /**
   * Gets the period unit for years.
   * <p>
   * The period unit defines the concept of a period of a year in the Julian calendar system.
   * This has an estimated duration equal to 365.25 days.
   * <p>
   * See   { @link # yearRule ( ) } for the main date-time field.
   *
   * @return the period unit for years, never null
   */
  def periodYears: PeriodUnit = YEARS

  /**
   * Gets the period unit for weeks.
   * <p>
   * The period unit defines the concept of a period of a week.
   * This is equivalent to the ISO weeks period unit.
   *
   * @return the period unit for weeks, never null
   */
  def periodWeeks: PeriodUnit = ISOChronology.periodWeeks

  /**
   * Gets the period unit for months.
   * <p>
   * The period unit defines the concept of a period of a month in the Julian calendar system.
   * This has an estimated duration equal to one-twelfth of 365.25 days.
   * <p>
   * See   { @link # monthOfYearRule ( ) } for the main date-time field.
   *
   * @return the period unit for months, never null
   */
  def periodMonths: PeriodUnit = MONTHS

  /**
   * The singleton instance of   { @code JulianChronology }.
   */
  val INSTANCE: JulianChronology = new JulianChronology

  /**
   * Gets the period unit for days.
   * <p>
   * The period unit defines the concept of a period of a day.
   * This is equivalent to the ISO days period unit.
   * <p>
   * See   { @link # dayOfMonthRule ( ) } for the main date-time field.
   *
   * @return the period unit for days, never null
   */
  def periodDays: PeriodUnit = ISOChronology.periodDays

  object Months extends Months

  /**
   * Unit class for months.
   */
  @SerialVersionUID(1L)
  private[time] sealed class Months private extends PeriodUnit("JulianMonths", Duration.ofStandardHours(31557600L / 12L)) {

    private def readResolve: AnyRef = Months
  }

  /**
   * Period unit for years.
   */
  private val YEARS: PeriodUnit = Years

  /**
   * Period unit for months.
   */
  private val MONTHS: PeriodUnit = Months

  /**
   * Checks if the specified year is a leap year.
   * <p>
   * The Julian calendar system defines a leap year as being divisible by four
   * without remainder. The calculation is proleptic - applying the same rules
   * into the far future and far past.
   *
   * @param year the year to check, not validated for range
   * @return true if the year is a leap year
   */
  def isLeapYear(year: Int): Boolean = ((year & 3) == 0)

}

@SerialVersionUID(1L)
sealed class JulianChronology private extends Chronology with Serializable {

  /**
   * Resolves singleton.
   *
   * @return the singleton instance
   */
  private def readResolve: AnyRef = JulianChronology

  /**
   * Gets the name of the chronology.
   *
   * @return the name of the chronology, never null
   */
  override def getName: String = "Julian"
}