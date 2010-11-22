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
import javax.time.calendar.Calendrical
import javax.time.calendar.CalendricalMerger
import javax.time.calendar.CalendricalRule
import javax.time.calendar.DateProvider
import javax.time.calendar.DayOfWeek
import javax.time.calendar.LocalDate
import javax.time.calendar.MonthOfYear

/**
 * A date in the Historic calendar system.
 * <p>
 * HistoricDate is an immutable class that represents a date in the Historic calendar system.
 * The rules of the calendar system are described in   { @link HistoricChronology }.
 * The date has a precision of one day and a range within the era from
 * year 1 to year 999,999,999 (inclusive).
 * <p>
 * Instances of this class may be created from any other object that implements
 * { @link DateProvider } including   { @link LocalDate }. Similarly, instances of
 * this class may be passed into the factory method of any other implementation
 * of   { @code DateProvider }.
 * <p>
 * HistoricDate is immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object HistoricDate {
  /**
   * Obtains an instance of   { @code HistoricDate } from a calendrical.
   * <p>
   * This can be used extract the date directly from any implementation
   * of   { @code Calendrical }, including those in other calendar systems.
   *
   * @param calendrical the calendrical to extract from, not null
   * @return the Historic date, never null
   * @throws UnsupportedRuleException if the day-of-week cannot be obtained
   */
  def of(calendrical: Calendrical): HistoricDate = rule.getValueChecked(calendrical)

  /**
   * The maximum valid year of era.
   * This is currently set to 999,999,999 but may be changed to increase
   * the valid range in a future version of the specification.
   */
  val MAX_YEAR: Int = 999999999
  /**
   * The standard cutover date between the Julian and Gregorian calendar system of 1582-10-15.
   */
  val STANDARD_CUTOVER: LocalDate = LocalDate.of(1582, 10, 15)
  /**
   * Obtains an instance of   { @code LocalDate } from a year, month and day
   * using the standard cutover of 1582-10-15.
   * <p>
   * The day must be valid for the year and month or an exception will be thrown.
   *
   * @param historicYear the year to represent, from -(MAX_YEAR-1) to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, not null
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @return the local date, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def of(historicYear: Int, monthOfYear: MonthOfYear, dayOfMonth: Int): HistoricDate = {
    of(STANDARD_CUTOVER, historicYear, monthOfYear, dayOfMonth)
  }

  /**
   * Obtains an instance of   { @code LocalDate } from a year, month and day
   * specifying the cutover date to use.
   * <p>
   * The day must be valid for the year and month or an exception will be thrown.
   *
   * @param historicYear the year to represent, from -(MAX_YEAR-1) to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, not null
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @return the local date, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def of(cutover: LocalDate, historicYear: Int, monthOfYear: MonthOfYear, dayOfMonth: Int): HistoricDate = {
    HistoricChronology.checkNotNull(cutover, "Cutover date must not be null")
    val chrono: HistoricChronology = HistoricChronology.cutoverAt(cutover)
    chrono.yearRule.checkValue(historicYear)
    HistoricChronology.checkNotNull(monthOfYear, "MonthOfYear must not be null")
    chrono.dayOfMonthRule.checkValue(dayOfMonth)
    new HistoricDate(chrono, historicYear, monthOfYear, dayOfMonth)
  }

  /**
   * Rule implementation.
   */
  private[i18n] object Rule extends Rule

  @SerialVersionUID(1L)
  private[i18n] sealed class Rule private
    extends CalendricalRule[HistoricDate](classOf[HistoricDate], HistoricChronology.standardCutover, "HistoricDate", HistoricChronology.periodDays, null) with Serializable {
    protected override def derive(calendrical: Calendrical): Option[HistoricDate] = {
      val ld: LocalDate = calendrical.get(LocalDate.rule).getOrElse(return None)
      return None //TODO
    }

    protected override def merge(merger: CalendricalMerger): Unit = {
      val cd: HistoricDate = merger.getValue(this)
      merger.storeMerged(LocalDate.rule, cd.toLocalDate)
      merger.removeProcessed(this)
    }

    private def readResolve: AnyRef = Rule
  }

  /**
   * Gets the field rule for   { @code HistoricDate }.
   *
   * @return the field rule for the date, never null
   */
  def rule: CalendricalRule[HistoricDate] = Rule
}


/**
 * Constructs an instance with the specified date.
 *
 * @param chono the chronology, not null
 * @param year the year to represent, valid
 * @param month the month-of-year to represent, not null
 * @param day the day-of-month to represent, valid
 */
final class HistoricDate private[i18n](val chrono: HistoricChronology, @transient val year: Int, @transient val month: MonthOfYear, @transient val day: Int)
  extends DateProvider with Calendrical with Comparable[HistoricDate] with Serializable {

  import HistoricDate._

  /**
   * Gets the historic month-of-year value.
   *
   * @return the month-of-year, never null
   */
  def getMonthOfYear: MonthOfYear = month

  /**
   * Obtains an instance of   { @code HistoricDate } using the previous valid algorithm.
   *
   * @param historicYear the year to represent
   * @param monthOfYear the month-of-year to represent, not null
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @return the historic date, never null
   */
  private def previousValid(historicYear: Int, monthOfYear: MonthOfYear, _dayOfMonth: Int): HistoricDate = {
    var dayOfMonth = _dayOfMonth
    chrono.yearRule.checkValue(historicYear)
    HistoricChronology.checkNotNull(monthOfYear, "MonthOfYear must not be null")
    chrono.dayOfMonthRule.checkValue(dayOfMonth)
    val lastDay: Int = monthOfYear.getLastDayOfMonth(chrono.isLeapYear(year))
    if (dayOfMonth > lastDay) dayOfMonth = lastDay
    new HistoricDate(chrono, year, monthOfYear, dayOfMonth)
  }

  /**
   * Checks is this date is after the specified date.
   * <p>
   * The comparison is based on the year, month, day and cutover date.
   *
   * @param otherDate the other date instance to compare to, not null
   * @return true if this day is after the specified day
   */
  def isAfter(otherDate: HistoricDate): Boolean = compareTo(otherDate) > 0

  /**
   * Compares this date to the specified date.
   * <p>
   * The comparison is based on the year, month, day and cutover date.
   *
   * @param otherDate the other date instance to compare to, not null
   * @return the comparator value, negative if less, positive if greater
   */
  def compareTo(otherDate: HistoricDate): Int = {
    var cmp: Int = toLocalDate.compareTo(otherDate.toLocalDate)
    if (cmp == 0) cmp = chrono.getCutover.compareTo(otherDate.chrono.getCutover)
    return cmp
  }

  /**
   * Gets the historic day-of-month value.
   *
   * @return the day-of-month, from 1 to 30
   */
  def getDayOfMonth: Int = day

  /**
   * Checks is this date is before the specified date.
   * <p>
   * The comparison is based on the year, month, day and cutover date.
   *
   * @param otherDate the other date instance to compare to, not null
   * @return true if this day is before the specified day
   */
  def isBefore(otherDate: HistoricDate): Boolean = compareTo(otherDate) < 0

  /**
   * Returns a copy of this date with the specified number of years added.
   * <p>
   * The result of changing the year may leave the day-of-month invalid.
   * To avoid this, the day-of-month is changed to the largest valid value.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to add, positive or negative
   * @return a { @code HistoricDate } based on this date with the specified years added, never null
   * @throws IllegalCalendarFieldValueException if the year range is exceeded
   */
  def plusYears(years: Int): HistoricDate = {
    val newYear: Int = getYear + years
    previousValid(newYear, month, day)
  }

  /**
   * Returns a copy of this date with the day-of-year value altered.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfYear the day-of-year to represent, from 1 to 366
   * @return a { @code HistoricDate } based on this date with the specified day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-year is out of range
   * @throws InvalidCalendarFieldException if the day-of-year is invalid for the year
   */
  def withDayOfYear(dayOfYear: Int): HistoricDate = {
    chrono.dayOfYearRule.checkValue(dayOfYear)
    chrono.getDateFromDayOfYear(year, dayOfYear)
  }

  /**
   * Checks if the date represented is the leap day in a leap year.
   * <p>
   * The leap day is when the year is a leap year, the month is February and
   * the day is 29.
   *
   * @return true if this date is the leap day in a leap year
   */
  def isLeapDay: Boolean = getMonthOfYear.isFebruary && getDayOfMonth == 29

  /**
   * Returns a copy of this date with the day-of-month value altered.
   * <p>
   * The specified day-of-month must be valid for the month and year.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @return a { @code HistoricDate } based on this date with the specified day, never null
   * @throws IllegalCalendarFieldValueException if the day is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the year and month
   */
  def withDayOfMonth(dayOfMonth: Int): HistoricDate = of(getYear, getMonthOfYear, dayOfMonth)

  /**
   * Outputs the date as a   { @code String }, such as '1723-13-01 (Historic 2010-10-15)'.
   * <p>
   * The output will be in the format 'yyyy-MM-dd (Historic 2010-10-15)' where
   * 2010-10-15 is the cutover date.
   *
   * @return the formatted date string, never null
   */
  override def toString: String = {
    val yearValue: Int = getYear
    val monthValue: Int = getMonthOfYear.getValue
    val dayValue: Int = getDayOfMonth
    val absYear: Int = Math.abs(yearValue)
    val buf: StringBuilder = new StringBuilder(12)
    if (absYear < 1000) {
      buf.append(yearValue + 10000).deleteCharAt(0)
    }
    else {
      buf.append(yearValue)
    }
    buf.append(if (monthValue < 10) "-0" else "-").append(monthValue).append(if (dayValue < 10) "-0" else "-").append(dayValue).append(" (" + chrono.getName + ")").toString
  }

  /**
   * Returns a copy of this date with the month-of-year value altered.
   * <p>
   * The result of setting the month may leave the day-of-month invalid.
   * To avoid this, the day-of-month is changed to the largest valid value.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear the month-of-year to represent, from 1 to 12
   * @return a { @code HistoricDate } based on this date with the specified month, never null
   */
  def withMonthOfYear(monthOfYear: MonthOfYear): HistoricDate = previousValid(getYear, monthOfYear, getDayOfMonth)

  /**
   * Gets the historic era.
   * <p>
   * The era provides a context for the year-of-era.
   * This calendar system defines two eras, BCE and CE.
   *
   * @return the era, never null
   */
  def getEra: HistoricEra = (if (year < 1) HistoricEra.BCE else HistoricEra.CE)

  /**
   * Gets the historic day-of-week.
   *
   * @return the day-of-week, never null
   */
  def getDayOfWeek: DayOfWeek = toLocalDate.getDayOfWeek

  /**
   * Returns a copy of this date with the specified number of days added.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param days the days to add, positive or negative
   * @return a { @code HistoricDate } based on this date with the specified days added, never null
   * @throws IllegalCalendarFieldValueException if the year range is exceeded
   */
  def plusDays(days: Int): HistoricDate = of(toLocalDate.plusDays(days))

  /**
   * Returns a copy of this date with the specified number of months added.
   * <p>
   * The result of changing the month may leave the day-of-month invalid.
   * To avoid this, the day-of-month is changed to the largest valid value.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to add, positive or negative
   * @return a { @code HistoricDate } based on this date with the specified months added, never null
   * @throws IllegalCalendarFieldValueException if the year range is exceeded
   */
  def plusMonths(months: Int): HistoricDate = {
    var month0: Int = (months % 12) + 12 + getMonthOfYear.ordinal
    val years: Int = (months / 12) - 1 + (month0 / 12)
    month0 %= 12
    val newYear: Int = getYear + years
    previousValid(newYear, MonthOfYear.of(month0 + 1), day)
  }

  /**
   * Gets the value of the specified calendar field.
   * <p>
   * This method queries the value of the specified calendar field.
   * If the calendar field is not supported then an exception is thrown.
   *
   * @param rule the field to query, not null
   * @return the value for the field
   * @throws UnsupportedRuleException if no value for the field is found
   */
  def get[T](rule: CalendricalRule[T]): Option[T] = {
    if (rule.equals(LocalDate.rule)) rule.reify(toLocalDate)
    else rule.deriveValueFor(rule, this, this)
  }

  /**
   * Gets the historic year value, which can be negative.
   * <p>
   * The year is value that is continuous.
   * Thus, 1 AD is represented as year 1, and 1 BCE is represented as year 0.
   *
   * @return the year, from -(MAX_YEAR-1) to MAX_YEAR
   */
  def getYear: Int = year

  /**
   * Checks if the date represented is a leap year.
   *
   * @return true if this date is in a leap year
   */
  def isLeapYear: Boolean = chrono.isLeapYear(getYear)

  /**
   * Returns a copy of this date with the year value altered.
   * <p>
   * The result of setting the year may leave the day-of-month invalid.
   * To avoid this, the day-of-month is changed to the largest valid value.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param historicYear the year to represent, from MIN_YEAR to MAX_YEAR
   * @return a { @code HistoricDate } based on this date with the specified year, never null
   * @throws IllegalCalendarFieldValueException if the year is out of range
   */
  def withYear(historicYear: Int): HistoricDate = previousValid(historicYear, getMonthOfYear, getDayOfMonth)

  /**
   * Converts this date to an ISO-8601 calendar system   { @code LocalDate }.
   *
   * @return the equivalent date in the ISO-8601 calendar system, never null
   */
  override def toLocalDate: LocalDate = {
    val possible: LocalDate = LocalDate.of(year, month, day)
    if (possible.isBefore(chrono.getCutover)) {
      val julYear1Days: Long = (year - 1) * 365 + (year / 4) + chrono.getDayOfYear(this) - 1
      LocalDate.ofModifiedJulianDays(julYear1Days + 0)
    } else possible
  }

  /**
   * Checks is this date is equal to the specified date.
   * <p>
   * The comparison is based on the year, month, day and cutover date.
   *
   * @param otherDate the other date instance to compare to, null returns false
   * @return true if this day is equal to the specified day
   */
  override def equals(otherDate: AnyRef): Boolean = {
    if (this == otherDate) true
    else if (otherDate.isInstanceOf[HistoricDate]) {
      val other: HistoricDate = otherDate.asInstanceOf[HistoricDate]
      year == other.year && month == other.month && day == other.day && chrono.getCutover.equals(other.chrono.getCutover)
    }
    else false
  }

  /**
   * Gets the chronology that this date uses, which is the historic calendar system.
   *
   * @return the historic chronology, never null
   */
  def getChronology: HistoricChronology = chrono

  /**
   * Gets the historic year-of-era value.
   * <p>
   * The year-of-era is a value that matches the historic definition.
   * Thus, both 1 AD and 1 BCE are represented as year-of-era 1.
   *
   * @return the year, from 1 to MAX_YEAR
   */
  def getYearOfEra: Int = (if (year < 1) -(year - 1) else year)

  /**
   * A hash code for this date.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = {
    val yearValue: Int = year
    val monthValue: Int = month.getValue
    val dayValue: Int = day
    (yearValue & 0xFFFFF800) ^ ((yearValue << 11) + (monthValue << 6) + (dayValue)) + chrono.getCutover.hashCode
  }

  /**
   * Gets the historic day-of-year value.
   *
   * @return the day-of-year, from 1 to 366
   */
  def getDayOfYear: Int = chrono.getDayOfYear(this)
}