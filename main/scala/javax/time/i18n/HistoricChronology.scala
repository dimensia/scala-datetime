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

import scala.util.control.Breaks._
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
   * The start of months in a standard year.
   */

  private val StandardMonthStart: Array[Int] = Array[Int](0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)

  /**
   * The start of months in a leap year.
   */
  private val LeapMonthStart: Array[Int] = Array[Int](0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)

  /**
   * Period unit for months.
   */
  private object Months extends Months

  /**
   * Unit class for months.
   */
  @SerialVersionUID(1L)
  private[time] sealed class Months extends PeriodUnit("JulianMonths", Duration.ofStandardHours(31557600L / 12L)) {
    private def readResolve: AnyRef = Months
  }

  /**
   * Period unit for years.
   */
  private object Years extends Years

  /**
   * Unit class for years.
   */
  @SerialVersionUID(1L)
  private[time] sealed class Years extends PeriodUnit("JulianYears", Duration.ofSeconds(31557600L)) {
    private def readResolve: AnyRef = Years
  }

  /**
   * Gets the period unit for months.
   * <p>
   * The period unit defines the concept of a period of a month.
   * Historic months are typically 30 days long, except for the 13th month which is
   * 5 or 6 days long. The rule uses an estimated duration of 29.5 days.
   * <p>
   * See {@link #monthOfYearRule()} for the main date-time field.
   *
   * @return the period unit for months, never null
   */
  def periodMonths: PeriodUnit = Months

  /**Constructor.
   * @param chrono The chronology.
   **/
  @SerialVersionUID(1L)
  private[time] final class YearRule(chrono: HistoricChronology)
    extends DateTimeFieldRule[Int](classOf[Int], chrono, "Year", Years, null, -(HistoricDate.MaxYear - 1), HistoricDate.MaxYear) with Serializable {

    override def merge(merger: CalendricalMerger): Unit = {
      (merger.getValue(chrono.monthOfYearRule), merger.getValue(chrono.dayOfMonthRule)) match {
        case (Some(moy), Some(domVal)) =>
          val year: Int = merger.getValue(this).get
          var date: HistoricDate = null
          if (merger.getContext.isStrict) {
            date = HistoricDate.of(year, moy, domVal)
          }
          else {
            date = HistoricDate.of(year, MonthOfYear.January, 1).plusMonths(moy.getValue - 1).plusMonths(-1).plusDays(domVal).plusDays(-1)
          }
          merger.storeMerged(LocalDate.rule, date.toLocalDate)
          merger.removeProcessed(this)
          merger.removeProcessed(chrono.monthOfYearRule)
          merger.removeProcessed(chrono.dayOfMonthRule)
        case _ =>
      }
    }

    override def derive(calendrical: Calendrical): Option[Int] = calendrical.get(HistoricDate.rule).map(_.getYear)
  }

  /**Constructor.
   * @param chrono The chronology.
   **/
  @SerialVersionUID(1L)
  private[time] final class MonthOfYearRule(chrono: HistoricChronology)
    extends DateTimeFieldRule[MonthOfYear](classOf[MonthOfYear], chrono, "MonthOfYear", Months, Years, 1, 13) with Serializable {

    override def derive(calendrical: Calendrical): Option[MonthOfYear] = calendrical.get(HistoricDate.rule).map(_.getMonthOfYear)
  }

  /**Constructor.
   * @param chrono The chronology.
   **/
  @SerialVersionUID(1L)
  private[time] final class DayOfMonthRule(chrono: HistoricChronology)
    extends DateTimeFieldRule[Int](classOf[Int], chrono, "DayOfMonth", periodDays, Months, 1, 30) with Serializable {
    override def derive(calendrical: Calendrical): Option[Int] = {
      val cd: HistoricDate = calendrical.get(HistoricDate.rule).getOrElse(return None)
      return Some(cd.getDayOfMonth)
    }

    override def getSmallestMaximumValue: Int = 28

    override def getMaximumValue(calendrical: Calendrical): Int = {
      (calendrical.get(chrono.yearRule), calendrical.get(chrono.monthOfYearRule)) match {
        case (Some(year), Some(month)) => month.lengthInDays(chrono.isLeapYear(year))
        case (None, Some(month)) => month.maxLengthInDays
        case _ => getMaximumValue
      }
    }

  }

  /**
   * Obtains an instance of <code>HistoricChronology</code> using the standard
   * cutover date of 1582-10-15.
   *
   * @return a {@code HistoricChronology}, never null
   */
  def standardCutover: HistoricChronology = new HistoricChronology(HistoricDate.StandardCutover)

  /**
   * Gets the period unit for years.
   * <p>
   * The period unit defines the concept of a period of a year.
   * This has an estimated duration equal to 365.25 days.
   * <p>
   * See {@link #yearRule()} for the main date-time field.
   *
   * @return the period unit for years, never null
   */
  def periodYears: PeriodUnit = Years

  /**Constructor.
   * @param chrono The chronology.
   **/
  @SerialVersionUID(1L)
  private[time] final class EraRule(chrono: HistoricChronology)
    extends DateTimeFieldRule[HistoricEra](classOf[HistoricEra], chrono, "Era", periodEras, null, 0, 1)
    with Serializable {
    override def derive(calendrical: Calendrical): Option[HistoricEra] = {
      calendrical.get(HistoricDate.rule).map(_.getEra)
    }
  }

  /**
   * Gets the period unit for days.
   * <p>
   * The period unit defines the concept of a period of a day.
   * This is equivalent to the ISO days period unit.
   * <p>
   * See {@link #dayOfMonthRule()} for the main date-time field.
   *
   * @return the period unit for days, never null
   */
  def periodDays: PeriodUnit = ISOChronology.periodDays


  /**Constructor.
   * @param chrono The chronology.
   **/
  @SerialVersionUID(1L)
  private[time] final class DayOfYearRule(chrono: HistoricChronology)
    extends DateTimeFieldRule[Int](classOf[Int], chrono, "DayOfYear", periodDays, Years, 1, 366) with Serializable {
    override def getMaximumValue(calendrical: Calendrical): Int = {
      val year: Int = calendrical.get(chrono.yearRule).getOrElse(return getMaximumValue)
      return if (chrono.isLeapYear(year)) 366 else 365
    }

    override def getSmallestMaximumValue: Int = 365

    override def derive(calendrical: Calendrical): Option[Int] = calendrical.get(HistoricDate.rule).map(_.getDayOfYear)

    override def merge(merger: CalendricalMerger): Unit = {
      val yearVal: Int = merger.getValue(chrono.yearRule).get
      if (yearVal != null) {
        val doy: Int = merger.getValue(this).get
        val date: HistoricDate =
          if (merger.getContext.isStrict) HistoricDate.of(yearVal, MonthOfYear.January, 1).withDayOfYear(doy)
          else HistoricDate.of(yearVal, MonthOfYear.January, 1).plusDays(doy).plusDays(-1)
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
   * @return a {@code HistoricChronology}, never null
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
  private[time] final class DayOfWeekRule(chrono: HistoricChronology)
    extends DateTimeFieldRule[DayOfWeek](classOf[DayOfWeek], chrono, "DayOfWeek", periodDays, periodWeeks, 1, 7)
    with Serializable {

    override def derive(calendrical: Calendrical): Option[DayOfWeek] = calendrical.get(HistoricDate.rule).map(_.getDayOfWeek)
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
   * See {@link #eraRule()} for the main date-time field.
   *
   * @return the period unit for years, never null
   */
  def periodEras: PeriodUnit = ISOChronology.periodEras

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
      if (cutover.getMonthOfYear < MonthOfYear.February) {
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
    if (isLeapYear(date.getYear)) LeapMonthStart(moy0) + dom
    else StandardMonthStart(moy0) + dom
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
    val array: Array[Int] = (if (leap) LeapMonthStart else StandardMonthStart)
    var month: Int = 1
    breakable {
      while (month < 12) {
        if (doy0 < array(month)) {
          break
        }
        month += 1;
      }
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