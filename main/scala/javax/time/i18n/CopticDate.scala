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

import javax.time.MathUtils
import javax.time.calendar.Calendrical
import javax.time.calendar.CalendricalMerger
import javax.time.calendar.CalendricalRule
import javax.time.calendar.DateProvider
import javax.time.calendar.DayOfWeek
import javax.time.calendar.IllegalCalendarFieldValueException
import javax.time.calendar.InvalidCalendarFieldException
import javax.time.calendar.LocalDate

/**
 * A date in the Coptic calendar system.
 * <p>
 * {@code CopticDate} is an immutable class that represents a date in the Coptic calendar system.
 * The rules of the calendar system are described in {@link CopticChronology}.
 * <p>
 * Instances of this class may be created from other date objects that implement {@code Calendrical}.
 * Notably this includes {@link LocalDate} and all other date classes from other calendar systems.
 * <p>
 * CopticDate is immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object CopticDate {
  /**
   * The maximum epoch day that is valid.
   * The low maximum year means that overflows don't happen.
   */
  private val MaxEpochDay: Int = 3652134

  /**
   * The minimum epoch day that is valid.
   * The avoidance of negatives makes calculation easier.
   */
  private val MinEpochDay: Int = 0

  /**
   * The maximum valid year.
   * This is currently set to 9999 but may be changed to increase the valid range
   * in a future version of the specification.
   */
  val MaxYear: Int = 9999

  /**
   * The minimum valid year.
   */
  val MinYear: Int = 1

  /**
   * The number of days to add to MJD to get the Coptic epoch day.
   */
  private val ModifiedJulianDaysToCoptic: Int = 574971

  /**
   * Gets the rule for {@code CopticDate}.
   *
   * @return the rule for the date, never null
   */
  def rule: CalendricalRule[CopticDate] = Rule

  /**
   * Obtains an instance of {@code CopticDate} from the Coptic year,
   * month-of-year and day-of-month.
   *
   * @param copticYear the year to represent, from 1 to 9999
   * @param copticMonthOfYear the month-of-year to represent, from 1 to 13
   * @param copticDayOfMonth the day-of-month to represent, from 1 to 30
   * @return the Coptic date, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def of(copticYear: Int, copticMonthOfYear: Int, copticDayOfMonth: Int): CopticDate = {
    CopticChronology.yearRule.checkValue(copticYear)
    CopticChronology.monthOfYearRule.checkValue(copticMonthOfYear)
    CopticChronology.dayOfMonthRule.checkValue(copticDayOfMonth)
    if (copticMonthOfYear == 13 && copticDayOfMonth > 5) {
      if (copticDayOfMonth > 6 || CopticChronology.isLeapYear(copticYear) == false) {
        throw new InvalidCalendarFieldException("Invalid Coptic date", CopticChronology.dayOfMonthRule)
      }
    }
    val epochDays: Int = (copticYear - 1) * 365 + (copticYear / 4) + 30 * (copticMonthOfYear - 1) + copticDayOfMonth - 1
    new CopticDate(epochDays, copticYear, copticMonthOfYear, copticDayOfMonth)
  }

  /**
   * Obtains an instance of {@code CopticDate} from a calendrical.
   * <p>
   * This can be used extract the date directly from any implementation
   * of {@code Calendrical}, including those in other calendar systems.
   *
   * @param calendrical the calendrical to extract from, not null
   * @return the Coptic date, never null
   * @throws CalendricalException if the date cannot be obtained
   */
  def of(calendrical: Calendrical): CopticDate = rule.getValueChecked(calendrical)

  /**
   * Obtains an instance of {@code CopticDate} from a number of epoch days.
   *
   * @param epochDays the epoch days to use, not null
   * @return a CopticDate object, never null
   * @throws IllegalCalendarFieldValueException if the year range is exceeded
   */
  private def copticDateFromEpochDays(epochDays: Int): CopticDate = {
    if (epochDays < MinEpochDay || epochDays > MaxEpochDay) {
      throw new IllegalCalendarFieldValueException("Date exceeds supported range for CopticDate", CopticChronology.yearRule)
    }
    val year: Int = ((epochDays * 4) + 1463) / 1461
    val startYearEpochDays: Int = (year - 1) * 365 + (year / 4)
    val doy0: Int = epochDays - startYearEpochDays
    val month: Int = doy0 / 30 + 1
    val day: Int = doy0 % 30 + 1
    new CopticDate(epochDays, year, month, day)
  }

  /**
   * Obtains an instance of {@code CopticDate} using the previous valid algorithm.
   *
   * @param year the year to represent
   * @param monthOfYear the month-of-year to represent
   * @param dayOfMonth the day-of-month to represent
   * @return the Coptic date, never null
   */
  private def copticDatePreviousValid(year: Int, monthOfYear: Int, _dayOfMonth: Int): CopticDate = {
    var dayOfMonth = _dayOfMonth
    CopticChronology.yearRule.checkValue(year)
    CopticChronology.monthOfYearRule.checkValue(monthOfYear)
    CopticChronology.dayOfMonthRule.checkValue(dayOfMonth)
    if (monthOfYear == 13 && dayOfMonth > 5) {
      dayOfMonth = if (CopticChronology.isLeapYear(year)) 6 else 5
    }
    val epochDays: Int = (year - 1) * 365 + (year / 4) + 30 * (monthOfYear - 1) + dayOfMonth - 1
    new CopticDate(epochDays, year, monthOfYear, dayOfMonth)
  }

  /**
   * Rule implementation.
   */
  private[i18n] object Rule extends Rule

  @SerialVersionUID(1L)
  private[i18n] sealed class Rule
    extends CalendricalRule[CopticDate](classOf[CopticDate], CopticChronology, "CopticDate", CopticChronology.periodDays, null)
    with Serializable {
    override def merge(merger: CalendricalMerger): Unit = {
      val date: CopticDate = merger.getValue(this).get
      merger.storeMerged(LocalDate.rule, date.toLocalDate)
      merger.removeProcessed(this)
    }

    override def derive(calendrical: Calendrical): Option[CopticDate] = {
      val ld: LocalDate = calendrical.get(LocalDate.rule).getOrElse(return None)
      val epochDays: Long = ld.toModifiedJulianDays + ModifiedJulianDaysToCoptic
      return Some(copticDateFromEpochDays(epochDays.toInt))
    }

    private def readResolve: AnyRef = Rule
  }

}

/**
 * Constructs an instance with the specified date.
 *
 * @param epochDays the Coptic epoch days, caller checked to be one or greater (Coptic epoch day count, 0001-01-01 = 0)
 * @param year the year to represent, caller calculated
 * @param month the month-of-year to represent, caller calculated
 * @param day the day-of-month to represent, caller calculated
 */
@SerialVersionUID(1L)
final class CopticDate private(val epochDays: Int, @transient year: Int, @transient month: Int, @transient day: Int)
  extends DateProvider with Calendrical with Ordered[CopticDate] with Serializable {

  import CopticDate._

  /**
   * Checks if this date is before the specified date.
   * <p>
   * The comparison is based on the time-line position of the dates.
   *
   * @param other  the other date to compare to, not null
   * @return true if this is before the specified date
   */
  def isBefore(otherDate: CopticDate): Boolean = epochDays < otherDate.epochDays

  /**
   * A hash code for this date.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = epochDays

  /**
   * Checks if this date is after the specified date.
   * <p>
   * The comparison is based on the time-line position of the dates.
   *
   * @param other  the other date to compare to, not null
   * @return true if this is after the specified date
   */
  def isAfter(otherDate: CopticDate): Boolean = epochDays > otherDate.epochDays

  /**
   * Gets the chronology that this date uses, which is the Coptic calendar system.
   *
   * @return the Coptic chronology, never null
   */
  def getChronology: CopticChronology = CopticChronology

  /**
   * Returns a copy of this date with the day-of-year altered.
   * <p>
   * This method changes the day-of-year of the date.
   * If the resulting date is invalid, an exception is thrown.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfYear  the day-of-year to set in the returned date, from 1 to 365-366
   * @return a {@code CopticDate} based on this date with the requested day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-year value is invalid
   * @throws InvalidCalendarFieldException if the day-of-year is invalid for the year
   */
  def withDayOfYear(_dayOfYear: Int): CopticDate = {
    val dayOfYear = _dayOfYear - 1
    of(getYear, dayOfYear / 30 + 1, dayOfYear % 30 + 1)
  }

  /**
   * Checks if the year is a leap year, according to the Coptic calendar system rules.
   *
   * @return true if this date is in a leap year
   */
  def isLeapYear: Boolean = CopticChronology.isLeapYear(getYear)

  /**
   * Checks if this date is equal to the specified date.
   * <p>
   * The comparison is based on the time-line position of the dates.
   *
   * @param other  the other date to compare to, null returns false
   * @return true if this is equal to the specified date
   */
  override def equals(other: Any): Boolean =
    other match {
      case copticDate: CopticDate => (this eq copticDate) || (epochDays == copticDate.epochDays)
      case _ => false
    }

  /**
   * Gets the Coptic day-of-month field.
   *
   * @return the day-of-month, from 1 to 30
   */
  def getDayOfMonth: Int = day

  /**
   * Returns a copy of this date with the year altered.
   * <p>
   * This method changes the year of the date.
   * If this date is the leap day (month 13, day 6) and the new year is not
   * a leap year, the resulting date will be invalid.
   * To avoid this, the result day-of-month is changed from 6 to 5.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param year  the year to set in the returned date, from 1 to 9999
   * @return a {@code CopticDate} based on this date with the requested year, never null
   * @throws IllegalCalendarFieldValueException if the year value is invalid
   */
  def withYear(year: Int): CopticDate = copticDatePreviousValid(year, getMonthOfYear, getDayOfMonth)

  /**
   * Gets the value of the specified calendrical rule.
   * <p>
   * This method queries the value of the specified calendrical rule.
   * If the value cannot be returned for the rule from this date then
   * {@code null} will be returned.
   *
   * @param rule  the rule to use, not null
   * @return the value for the rule, null if the value cannot be returned
   */
  def get[T](rule: CalendricalRule[T]): Option[T] = {
    if (rule.equals(LocalDate.rule)) rule.reify(toLocalDate)
    //    else rule.deriveValueFor(rule, this, this, CopticChronology)  //FIXME
    else None
  }

  /**
   * Outputs this date as a {@code String}, such as {@code 1723-13-01 (Coptic)}.
   * <p>
   * The output will be in the format {@code yyyy-MM-dd (Coptic)}.
   *
   * @return the formatted date, never null
   */
  override def toString: String = {
    val yearValue: Int = getYear
    val monthValue: Int = getMonthOfYear
    val dayValue: Int = getDayOfMonth
    val absYear: Int = math.abs(yearValue)
    val buf: StringBuilder = new StringBuilder(12)
    if (absYear < 1000) buf.append(yearValue + 10000).deleteCharAt(0)
    else buf.append(yearValue)
    buf.append(if (monthValue < 10) "-0" else "-").append(monthValue).append(if (dayValue < 10) "-0" else "-").append(dayValue).append(" (Coptic)").toString
  }

  /**
   * Gets the Coptic month-of-year field.
   *
   * @return the month-of-year, from 1 to 13
   */
  def getMonthOfYear: Int = month

  /**
   * Returns a copy of this date with the specified number of years added.
   * <p>
   * This method adds the specified amount in years to the date.
   * If the month-day is invalid for the year, then the previous valid day
   * will be selected instead.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years  the years to add, may be negative
   * @return a {@code CopticDate} based on this date with the years added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusYears(years: Int): CopticDate = {
    val newYear: Int = getYear + years
    copticDatePreviousValid(newYear, getMonthOfYear, getDayOfMonth)
  }

  /**
   * Converts this date to a {@code LocalDate}, which is the default representation
   * of a date, and provides values in the ISO-8601 calendar system.
   *
   * @return the equivalent date in the ISO-8601 calendar system, never null
   */
  override def toLocalDate: LocalDate = LocalDate.ofModifiedJulianDays(epochDays - ModifiedJulianDaysToCoptic)

  /**
   * Gets the Coptic day-of-year field.
   *
   * @return the day-of-year, from 1 to 366
   */
  def getDayOfYear: Int = {
    val startYearEpochDays: Int = (year - 1) * 365 + (year / 4)
    epochDays - startYearEpochDays + 1
  }

  /**
   * Returns a copy of this date with the specified number of days added.
   * <p>
   * This method adds the specified amount in days to the date.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param days  the days to add, may be negative
   * @return a {@code CopticDate} based on this date with the days added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusDays(days: Int): CopticDate = {
    val newEpochDays: Int = epochDays + days
    copticDateFromEpochDays(newEpochDays)
  }

  /**
   * Returns a copy of this date with the specified number of months added.
   * <p>
   * This method adds the specified amount in months to the date.
   * If the month-day is invalid for the year, then the previous valid day
   * will be selected instead.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months  the months to add, may be negative
   * @return a {@code CopticDate} based on this date with the months added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusMonths(months: Int): CopticDate = {
    var newMonth0: Int = getMonthOfYear + months - 1
    var years: Int = newMonth0 / 13
    newMonth0 = newMonth0 % 13
    if (newMonth0 < 0) {
      newMonth0 += 13
      years -= 1;
    }
    val newYear: Int = getYear + years
    val newDay: Int = getDayOfMonth
    copticDatePreviousValid(newYear, newMonth0 + 1, newDay)
  }

  /**
   * Returns a copy of this date with the month-of-year altered.
   * <p>
   * This method changes the month-of-year of the date.
   * If this month is from 1 to 12 and the new month is 13 then the
   * resulting date might be invalid. In this case, the last valid
   * day-of-the month will be returned.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear  the month-of-year to set in the returned date, from 1 to 13
   * @return a {@code CopticDate} based on this date with the requested month, never null
   * @throws IllegalCalendarFieldValueException if the month value is invalid
   */
  def withMonthOfYear(monthOfYear: Int): CopticDate = copticDatePreviousValid(getYear, monthOfYear, getDayOfMonth)

  /**
   * Gets the day-of-week field, which is an enum {@code DayOfWeek}.
   * <p>
   * This method returns the enum {@link DayOfWeek} for the day-of-week.
   * This avoids confusion as to what {@code int} values mean.
   * If you need access to the primitive {@code int} value then the enum
   * provides the {@link DayOfWeek#getValue() int value}.
   * <p>
   * Additional information can be obtained from the {@code DayOfWeek}.
   * This includes textual names of the values.
   *
   * @return the day-of-week, never null
   */
  def getDayOfWeek: DayOfWeek = DayOfWeek.of((epochDays + 4) % 7 + 1)

  /**
   * Gets the Coptic year field.
   *
   * @return the year, from MIN_YEAR to MAX_YEAR
   */
  def getYear: Int = year

  /**
   * Returns a copy of this date with the day-of-month altered.
   * <p>
   * This method changes the day-of-month of the date.
   * If the resulting date is invalid, an exception is thrown.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfMonth  the day-of-month to set in the returned date, from 1 to 30
   * @return a {@code CopticDate} based on this date with the requested day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-month value is invalid
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def withDayOfMonth(dayOfMonth: Int): CopticDate = of(getYear, getMonthOfYear, dayOfMonth)

  /**
   * Replaces the date instance from the stream with a valid one.
   *
   * @return the resolved date, never null
   */
  private def readResolve: AnyRef = copticDateFromEpochDays(epochDays)

  /**
   * Compares this date to another date.
   * <p>
   * The comparison is based on the time-line position of the dates.
   *
   * @param other  the other date to compare to, not null
   * @return the comparator value, negative if less, positive if greater
   */
  def compare(otherDate: CopticDate): Int = MathUtils.safeCompare(epochDays, otherDate.epochDays)
}