/*
 * Copyright (c) 2007-2010, Stephen Colebourne & Michael Nascimento Santos
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
import javax.time.calendar.Calendrical
import javax.time.calendar.CalendricalMerger
import javax.time.calendar.Chronology
import javax.time.calendar.DateTimeFieldRule
import javax.time.calendar.DayOfWeek
import javax.time.calendar.ISOChronology
import javax.time.calendar.LocalDate
import javax.time.calendar.PeriodUnit

/**
 * The Coptic calendar system.
 * <p>
 * CopticChronology defines the rules of the Coptic calendar system.
 * The Coptic calendar has twelve months of 30 days followed by an additional
 * period of 5 or 6 days, modelled as the thirteenth month in this implementation.
 * <p>
 * Years are measured in the 'Era of the Martyrs'.
 * 0001-01-01 (Coptic) equals 0284-08-29 (ISO).
 * The supported range is from Coptic year 1 to year 9999 (inclusive).
 * <p>
 * CopticChronology is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object CopticChronology {
  /**
   * Gets the rule for the day-of-month field in the Coptic chronology.
   *
   * @return the rule for the day-of-month field, never null
   */
  def dayOfMonthRule: DateTimeFieldRule[Int] = DayOfMonthRule

  /**
   * The singleton instance of   { @code CopticChronology }.
   */
  val INSTANCE: CopticChronology = new CopticChronology
  /**
   * Gets the rule for the day-of-week field in the Coptic chronology.
   *
   * @return the rule for the day-of-week field, never null
   */
  def dayOfWeekRule: DateTimeFieldRule[DayOfWeek] = DayOfWeekRule

  /**
   * Gets the rule for the month-of-year field in the Coptic chronology.
   *
   * @return the rule for the month-of-year field, never null
   */
  def monthOfYearRule: DateTimeFieldRule[Int] = MonthOfYearRule

  /**
   * Rule implementation.
   */
  private object YearRule extends YearRule

  @SerialVersionUID(1L)
  private final class YearRule private
    extends DateTimeFieldRule[Int](classOf[Int], CopticChronology.INSTANCE, "Year", YEARS, null, CopticDate.MIN_YEAR, CopticDate.MAX_YEAR) with Serializable {

    private def readResolve: AnyRef = YearRule

    protected def derive(calendrical: Calendrical): Integer = {
      val cd: CopticDate = calendrical.get(CopticDate.rule)
      if (cd != null) cd.getYear else null
    }

    protected def merge(merger: CalendricalMerger): Unit = {
      val moyVal: Integer = merger.getValue(CopticChronology.monthOfYearRule)
      val domVal: Integer = merger.getValue(CopticChronology.dayOfMonthRule)
      if (moyVal != null && domVal != null) {
        val year: Int = merger.getValue(this)
        var date: CopticDate = null
        if (merger.getContext.isStrict) date = CopticDate.of(year, moyVal, domVal)
        else date = CopticDate.of(year, 1, 1).plusMonths(moyVal).plusMonths(-1).plusDays(domVal).plusDays(-1)
        merger.storeMerged(LocalDate.rule, date.toLocalDate)
        merger.removeProcessed(this)
        merger.removeProcessed(CopticChronology.monthOfYearRule)
        merger.removeProcessed(CopticChronology.dayOfMonthRule)
      }
    }
  }

  /**
   * Gets the period unit for years.
   * <p>
   * The period unit defines the concept of a period of a year.
   * This has an estimated duration equal to 365.25 days.
   * <p>
   * See   { @link # yearRule ( ) } for the main date-time field.
   *
   * @return the period unit for years, never null
   */
  def periodYears: PeriodUnit = YEARS

  /**
   * Gets the rule for the day-of-year field in the Coptic chronology.
   *
   * @return the rule for the day-of-year field, never null
   */
  def dayOfYearRule: DateTimeFieldRule[Int] = DayOfYearRule

  /**
   * Gets the period unit for months.
   * <p>
   * The period unit defines the concept of a period of a month.
   * Coptic months are typically 30 days long, except for the 13th month which is
   * 5 or 6 days long. The rule uses an estimated duration of 29.5 days.
   * <p>
   * See   { @link # monthOfYearRule ( ) } for the main date-time field.
   *
   * @return the period unit for months, never null
   */
  def periodMonths: PeriodUnit = MONTHS

  /**
   * Rule implementation.
   */
  private object DayOfYearRule extends DayOfYearRule

  @SerialVersionUID(1L)
  private final class DayOfYearRule private
    extends DateTimeFieldRule[Int](classOf[Int], CopticChronology.INSTANCE, "DayOfYear", periodDays, YEARS, 1, 366) with Serializable {
    override def getMaximumValue(calendrical: Calendrical): Int = {
      val year: Integer = calendrical.get(CopticChronology.yearRule)
      if (year != null) {
        return if (isLeapYear(year)) 366 else 365
      }
      return getMaximumValue
    }

    override def getSmallestMaximumValue: Int = 365

    private def readResolve: AnyRef = DayOfYearRule

    protected def derive(calendrical: Calendrical): Integer = {
      val cd: CopticDate = calendrical.get(CopticDate.rule)
      return if (cd != null) cd.getDayOfYear else null
    }

    protected def merge(merger: CalendricalMerger): Unit = {
      val yearVal: Integer = merger.getValue(CopticChronology.yearRule)
      if (yearVal != null) {
        val doy: Int = merger.getValue(this)
        var date: CopticDate = null
        if (merger.getContext.isStrict) {
          date = CopticDate.of(yearVal, 1, 1).withDayOfYear(doy)
        }
        else {
          date = CopticDate.of(yearVal, 1, 1).plusDays(doy).plusDays(-1)
        }
        merger.storeMerged(LocalDate.rule, date.toLocalDate)
        merger.removeProcessed(this)
        merger.removeProcessed(CopticChronology.yearRule)
      }
    }

  }

  /**
   * Rule implementation.
   */
  private object MonthOfYearRule extends MonthOfYearRule

  @SerialVersionUID(1L)
  private final class MonthOfYearRule private
    extends DateTimeFieldRule[Int](classOf[Int], CopticChronology.INSTANCE, "MonthOfYear", MONTHS, YEARS, 1, 13)
    with Serializable {
    private def readResolve: AnyRef = MonthOfYearRule

    protected def derive(calendrical: Calendrical): Int = {
      val cd: CopticDate = calendrical.get(CopticDate.rule)
      if (cd != null) cd.getMonthOfYear else null
    }
  }

  /**
   * Unit class for years.
   */

  @SerialVersionUID(1L)
  private final class Years private
    extends PeriodUnit("CopticYears", Duration.ofSeconds(31557600L)) {

    private def readResolve: AnyRef = YEARS
  }

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

  /**
   * Period unit for years.
   */
  private val YEARS: PeriodUnit = new CopticChronology.Years

  /**
   * Rule implementation.
   */
  private object DayOfWeekRule extends DayOfWeekRule

  @SerialVersionUID(1L)
  private final class DayOfWeekRule private
    extends DateTimeFieldRule[DayOfWeek](classOf[DayOfWeek], CopticChronology.INSTANCE, "DayOfWeek", periodDays, periodWeeks, 1, 7) with Serializable {

    private def readResolve: AnyRef = DayOfWeekRule

    protected def derive(calendrical: Calendrical): DayOfWeek = {
      val cd: CopticDate = calendrical.get(CopticDate.rule)
      if (cd != null) cd.getDayOfWeek else null
    }
  }

  /**
   * Rule implementation.
   */
  private object DayOfMonthRule extends DayOfMonthRule

  @SerialVersionUID(1L)
  private final class DayOfMonthRule private
    extends DateTimeFieldRule[Int](classOf[Int], CopticChronology.INSTANCE, "DayOfMonth", periodDays, MONTHS, 1, 30) with Serializable {
    private def readResolve: AnyRef = DayOfMonthRule

    override def getMaximumValue(calendrical: Calendrical): Int = {
      val year: Integer = calendrical.get(CopticChronology.yearRule)
      var moy: Integer = calendrical.get(CopticChronology.monthOfYearRule)
      if (year != null && moy != null) {
        if (moy == 13) return if (isLeapYear(year)) 6 else 5
        else return 30
      }
      return getMaximumValue
    }

    override def getSmallestMaximumValue: Int = 5

    protected def derive(calendrical: Calendrical): Int = {
      val cd: CopticDate = calendrical.get(CopticDate.rule)
      if (cd != null) cd.getDayOfMonth else null
    }
  }

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
   * Checks if the specified year is a leap year.
   * <p>
   * A year is leap if the remainder after division by four equals three.
   * This method does not validate the year passed in, and only has a
   * well-defined result for years in the supported range.
   *
   * @param year the year to check, not validated for range
   * @return true if the year is a leap year
   */
  def isLeapYear(year: Int): Boolean = ((year % 4) == 3)

  /**
   * Period unit for months.
   */
  private val MONTHS: PeriodUnit = new CopticChronology.Months

  /**
   * Unit class for months.
   */
  @SerialVersionUID(1L)
  private final class Months private
    extends PeriodUnit("CopticMonths", Duration.ofStandardHours(24L * 30L - 12L)) {

    private def readResolve: AnyRef = MONTHS

  }

  /**
   * Gets the rule for the year field in the Coptic chronology.
   *
   * @return the rule for the year field, never null
   */
  def yearRule: DateTimeFieldRule[Int] = YearRule
}


/**
 * Restrictive constructor.
 */
@SerialVersionUID(1L)
final class CopticChronology private extends Chronology with Serializable {
  /**
   * Gets the name of the chronology.
   *
   * @return the name of the chronology, never null
   */
  override def getName: String = "Coptic"

  /**
   * Resolves singleton.
   *
   * @return the singleton instance
   */
  private def readResolve: AnyRef = CopticChronology
}