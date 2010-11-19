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
import javax.time.calendar.Calendrical
import javax.time.calendar.CalendricalMerger
import javax.time.calendar.Chronology
import javax.time.calendar.DateTimeFieldRule
import javax.time.calendar.DayOfWeek
import javax.time.calendar.ISOChronology
import javax.time.calendar.InvalidCalendarFieldException
import javax.time.calendar.LocalDate
import javax.time.calendar.MonthOfYear
import javax.time.calendar.PeriodUnit

/**
 * The Historic calendar system.
 * <p>
 * HistoricChronology defines the rules of the Historic calendar system.
 * The Historic calendar has twelve months of 30 days followed by an additional
 * period of 5 or 6 days, modelled as the thirteenth month in this implementation.
 * <p>
 * Years are measured in the 'Era of the Martyrs'.
 * 0001-01-01 (Historic) equals 0284-08-29 (ISO).
 * The supported range is from Historic year 1 to year 9999 (inclusive).
 * <p>
 * HistoricChronology is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object HistoricChronology {

  /**
   * Unit class for months.
   */
  @SerialVersionUID(1L)
  private final class Months private
    extends PeriodUnit("JulianMonths", Duration.ofStandardHours(31557600L / 12L)) {

    private def readResolve: AnyRef = MONTHS
  }

  /**
   * Gets the period unit for months.
   * <p>
   * The period unit defines the concept of a period of a month.
   * Historic months are typically 30 days long, except for the 13th month which is
   * 5 or 6 days long. The rule uses an estimated duration of 29.5 days.
   * <p>
   * See   { @link # monthOfYearRule ( ) } for the main date-time field.
   *
   * @return the period unit for months, never null
   */
  def periodMonths: PeriodUnit = MONTHS

  /**Constructor.
   * @param chrono The chronology.
   **/
  @SerialVersionUID(1L)
  private final class YearRule private(chrono: HistoricChronology)
    extends DateTimeFieldRule[Int](classOf[Int], chrono, "Year", YEARS, null, -(HistoricDate.MAX_YEAR - 1), HistoricDate.MAX_YEAR) with Serializable {

    protected def merge(merger: CalendricalMerger): Unit = {
      val moy: MonthOfYear = merger.getValue(chrono.monthOfYearRule)
      val domVal: Int = merger.getValue(chrono.dayOfMonthRule)
      if (moy != null && domVal != null) {
        val year: Int = merger.getValue(this)
        var date: HistoricDate = null
        if (merger.getContext.isStrict) {
          date = HistoricDate.of(year, moy, domVal)
        }
        else {
          date = HistoricDate.of(year, MonthOfYear.JANUARY, 1).plusMonths(moy.getValue - 1).plusMonths(-1).plusDays(domVal).plusDays(-1)
        }
        merger.storeMerged(LocalDate.rule, date.toLocalDate)
        merger.removeProcessed(this)
        merger.removeProcessed(chrono.monthOfYearRule)
        merger.removeProcessed(chrono.dayOfMonthRule)
      }
    }

    protected def derive(calendrical: Calendrical): Integer = {
      val cd: HistoricDate = calendrical.get(HistoricDate.rule)
      if (cd != null) cd.getYear else null
    }
  }

  /**Constructor.
   * @param chrono The chronology.
   **/
  @SerialVersionUID(1L)
  private final class MonthOfYearRule private(chrono: HistoricChronology)
    extends DateTimeFieldRule[MonthOfYear](classOf[MonthOfYear], chrono, "MonthOfYear", MONTHS, YEARS, 1, 13) with Serializable {

    protected def derive(calendrical: Calendrical): MonthOfYear = {
      val cd: HistoricDate = calendrical.get(HistoricDate.rule)
      if (cd != null) cd.getMonthOfYear else null
    }
  }

  /**Constructor.
   * @param chrono The chronology.
   **/
  @SerialVersionUID(1L)
  private final class DayOfMonthRule private(chrono: HistoricChronology)
    extends DateTimeFieldRule[Int](classOf[Int], chrono, "DayOfMonth", periodDays, MONTHS, 1, 30) with Serializable {
    protected def derive(calendrical: Calendrical): Integer = {
      val cd: HistoricDate = calendrical.get(HistoricDate.rule)
      if (cd != null) cd.getDayOfMonth else null
    }

    override def getSmallestMaximumValue: Int = 28

    override def getMaximumValue(calendrical: Calendrical): Int = {
      val year: Int = calendrical.get(chrono.yearRule)
      val moy: MonthOfYear = calendrical.get(chrono.monthOfYearRule)
      if (moy != null) {
        if (year != null) {
          return moy.lengthInDays(chrono.isLeapYear(year))
        }
        else {
          return moy.maxLengthInDays
        }
      }
      return getMaximumValue
    }

  }

  /**
   * Obtains an instance of <code>HistoricChronology</code> using the standard
   * cutover date of 1582-10-15.
   *
   * @return a { @code HistoricChronology }, never null
   */
  def standardCutover: HistoricChronology = new HistoricChronology(HistoricDate.STANDARD_CUTOVER)

  /**
   * The start of months in a standard year.
   */
  private val STANDARD_MONTH_START: Array[Int] = Array[Int](0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
  /**
   * Period unit for years.
   */
  private val YEARS: PeriodUnit = new HistoricChronology.Years

  /**
   * Unit class for years.
   */
  @SerialVersionUID(1L)
  private final class Years
    extends PeriodUnit("JulianYears", Duration.ofSeconds(31557600L)) {

    private def readResolve: AnyRef = YEARS
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
   * The start of months in a leap year.
   */
  private val LEAP_MONTH_START: Array[Int] = Array[Int](0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)


  /**Constructor.
   * @param chrono The chronology.
   **/
  @SerialVersionUID(1L)
  private final class EraRule private(chrono: HistoricChronology)
    extends DateTimeFieldRule[HistoricEra](classOf[HistoricEra], chrono, "Era", periodEras, null, 0, 1)
    with Serializable {
    protected def derive(calendrical: Calendrical): HistoricEra = {
      val cd: HistoricDate = calendrical.get(HistoricDate.rule)
      if (cd != null) cd.getEra else null
    }

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


  /**Constructor.
   * @param chrono The chronology.
   **/
  @SerialVersionUID(1L)
  private final class DayOfYearRule private(chrono: HistoricChronology)
    extends DateTimeFieldRule[Int](classOf[Int], chrono, "DayOfYear", periodDays, YEARS, 1, 366) with Serializable {
    override def getMaximumValue(calendrical: Calendrical): Int = {
      val year: Int = calendrical.get(chrono.yearRule)
      if (year != null) if (chrono.isLeapYear(year)) 366 else 365
      else getMaximumValue
    }

    override def getSmallestMaximumValue: Int = 365

    protected def derive(calendrical: Calendrical): Integer = {
      val cd: HistoricDate = calendrical.get(HistoricDate.rule)
      if (cd != null) cd.getDayOfYear else null
    }

    protected def merge(merger: CalendricalMerger): Unit = {
      val yearVal: Int = merger.getValue(chrono.yearRule)
      if (yearVal != null) {
        val doy: Int = merger.getValue(this)
        var date: HistoricDate = null
        if (merger.getContext.isStrict) {
          date = HistoricDate.of(yearVal, MonthOfYear.JANUARY, 1).withDayOfYear(doy)
        }
        else {
          date = HistoricDate.of(yearVal, MonthOfYear.JANUARY, 1).plusDays(doy).plusDays(-1)
        }
        merger.storeMerged(LocalDate.rule, date.toLocalDate)
        merger.removeProcessed(this)
        merger.removeProcessed(chrono.yearRule)
      }
    }

  }

  /**
   * Obtains an instance of <code>HistoricChronology</code> specifying the
   * cutover date when the Gregorian/ISO calendar system was first used.
   *
   * @param cutover the cutover date, not null
   * @return a { @code HistoricChronology }, never null
   */
  def cutoverAt(cutover: LocalDate): HistoricChronology = {
    checkNotNull(cutover, "Cutover date must not be null")
    new HistoricChronology(cutover)
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

  @SerialVersionUID(1L)
  private final class DayOfWeekRule private(chrono: HistoricChronology)
    extends DateTimeFieldRule[DayOfWeek](classOf[DayOfWeek], chrono, "DayOfWeek", periodDays, periodWeeks, 1, 7)
    with Serializable {

    protected def derive(calendrical: Calendrical): DayOfWeek = {
      val cd: HistoricDate = calendrical.get(HistoricDate.rule)
      if (cd != null) cd.getDayOfWeek else null
    }
  }

  /**
   * Gets the period unit for eras.
   * <p>
   * The period unit defines the concept of a period of an era.
   * <p>
   * This is a basic unit and has no equivalent period.
   * The estimated duration is equal to 2,000,000,000 years.
   * This is equivalent to the ISO era period unit.
   * <p>
   * See   { @link # eraRule ( ) } for the main date-time field.
   *
   * @return the period unit for years, never null
   */
  def periodEras: PeriodUnit = ISOChronology.periodEras

  /**
   * Period unit for months.
   */
  private val MONTHS: PeriodUnit = new HistoricChronology.Months
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
}

/**
 * Restrictive constructor.
 * @param cutover The cutover from Julian to Gregorian.
 */
final class HistoricChronology private(cutover: LocalDate) extends Chronology with Serializable {

  import HistoricChronology._

  /**
   * Gets the name of the chronology.
   *
   * @return the name of the chronology, never null
   */
  override def getName: String = "Historic " + cutover

  /**
   * Gets the rule for the year field in the Historic chronology.
   *
   * @return the rule for the year field, never null
   */
  def yearRule: DateTimeFieldRule[Int] = new HistoricChronology.YearRule(this)

  /**
   * Gets the rule for the day-of-week field in the Historic chronology.
   *
   * @return the rule for the day-of-week field, never null
   */
  def dayOfWeekRule: DateTimeFieldRule[DayOfWeek] = new HistoricChronology.DayOfWeekRule(this)

  /**
   * Gets the rule for the year field in the Historic chronology.
   *
   * @return the rule for the year field, never null
   */
  def eraRule: DateTimeFieldRule[HistoricEra] = new HistoricChronology.EraRule(this)

  /**
   * Gets the rule for the year field in the Historic chronology.
   *
   * @return the rule for the year field, never null
   */
  def yearOfEraRule: DateTimeFieldRule[Int] = new HistoricChronology.YearRule(this)

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
  def isLeapYear(year: Int): Boolean = {
    if (year < cutover.getYear) {
      return JulianChronology.isLeapYear(year)
    }
    else if (year > cutover.getYear) {
      return ISOChronology.isLeapYear(year)
    }
    else {
      if (cutover.getMonthOfYear.compareTo(MonthOfYear.FEBRUARY) < 0) {
        return false
      }
      return false
    }
  }

  /**
   * Gets the rule for the day-of-year field in the Historic chronology.
   *
   * @return the rule for the day-of-year field, never null
   */
  def dayOfYearRule: DateTimeFieldRule[Int] = new HistoricChronology.DayOfYearRule(this)

  /**
   * Gets the rule for the day-of-month field in the Historic chronology.
   *
   * @return the rule for the day-of-month field, never null
   */
  def dayOfMonthRule: DateTimeFieldRule[Int] = new HistoricChronology.DayOfMonthRule(this)

  /**
   * Gets the cutover date of the chronology.
   *
   * @return the cutover date of the chronology, never null
   */
  def getCutover: LocalDate = cutover

  /**
   * Calculates the day-of-year from a date.
   *
   * @param date the date to use, not null
   * @return the day-of-year
   */
  private[i18n] def getDayOfYear(date: HistoricDate): Int = {
    val moy0: Int = date.getMonthOfYear.ordinal
    val dom: Int = date.getDayOfMonth
    if (isLeapYear(date.getYear)) LEAP_MONTH_START(moy0) + dom
    else STANDARD_MONTH_START(moy0) + dom
  }

  /**
   * Calculates the date from a year and day-of-year.
   *
   * @param year the year, valid
   * @param dayOfYear the day-of-year, valid
   * @return the date, never null
   */
  private[i18n] def getDateFromDayOfYear(year: Int, dayOfYear: Int): HistoricDate = {
    var leap: Boolean = isLeapYear(year)
    if (dayOfYear == 366 && leap == false) {
      throw new InvalidCalendarFieldException("DayOfYear 366 is invalid for year " + year, dayOfYearRule)
    }
    val doy0: Int = dayOfYear - 1
    val array: Array[Int] = (if (leap) LEAP_MONTH_START else STANDARD_MONTH_START)
    var month: Int = 1
    while (month < 12) {
      {
        if (doy0 < array(month)) {
          break //todo: break is not supported
        }
      }
      ({
        month += 1;
        month
      })
    }
    val moy: MonthOfYear = MonthOfYear.of(month)
    val dom: Int = dayOfYear - array(month - 1)
    new HistoricDate(this, year, moy, dom)
  }

  /**
   * Gets the rule for the month-of-year field in the Historic chronology.
   *
   * @return the rule for the month-of-year field, never null
   */
  def monthOfYearRule: DateTimeFieldRule[MonthOfYear] = new HistoricChronology.MonthOfYearRule(this)
}