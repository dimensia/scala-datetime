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
package javax.time.calendar

import java.io.Serializable
import javax.time.Instant
import javax.time.InstantProvider
import javax.time.MathUtils
import javax.time.calendar.format.DateTimeFormatter
import javax.time.calendar.format.DateTimeFormatters

/**
 * A date with a zone offset from UTC in the ISO-8601 calendar system,
 * such as '2007-12-03+01:00'.
 * <p>
 * OffsetDate is an immutable calendrical that represents a date, often viewed
 * as year-month-day-offset. This object can also access other date fields such as
 * day-of-year, day-of-week and week-of-year.
 * <p>
 * This class does not store or represent a time.
 * Thus, for example, the value "2nd October 2007 +02:00" can be stored
 * in a OffsetDate.
 * <p>
 * OffsetDate is immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object OffsetDate {

  /**
   * Rule implementation.
   */
  private[calendar] object Rule extends Rule

  @SerialVersionUID(1L)
  private[calendar] final class Rule
    extends CalendricalRule[OffsetDate](classOf[OffsetDate], ISOChronology.INSTANCE, "OffsetDate", ISOChronology.periodDays, null)
    with Serializable {

    protected override def derive(calendrical: Calendrical): OffsetDate = {
      val odt: OffsetDateTime = calendrical.get(OffsetDateTime.rule)
      if (odt != null) odt.toOffsetDate else null
    }

    private def readResolve: AnyRef = Rule

  }

  /**
   * Obtains an instance of    { @code OffsetDate } from a    { @code DateProvider }.
   *
   * @param dateProvider the date provider to use, not null
   * @param offset the zone offset, not null
   * @return the offset date, never null
   */
  def of(dateProvider: DateProvider, offset: ZoneOffset): OffsetDate = {
    val date: LocalDate = LocalDate.of(dateProvider)
    return new OffsetDate(date, offset)
  }

  /**
   * Obtains the current date from the specified clock.
   * <p>
   * This will query the specified clock to obtain the current date - today.
   * The offset will be set based on the time-zone in the clock.
   * <p>
   * Using this method allows the use of an alternate clock for testing.
   * The alternate clock may be introduced using    { @link Clock dependency injection }.
   *
   * @param clock the clock to use, not null
   * @return the current date, never null
   */
  def now(clock: Clock): OffsetDate = {
    ISOChronology.checkNotNull(clock, "Clock must not be null")
    return ofInstant(clock.instant, clock.getZone.getRules.getOffset(clock.instant))
  }

  /**
   * Obtains an instance of    { @code OffsetDate } from a year, month and day.
   *
   * @param year the year to represent, from MIN_VALUE + 1 to MAX_VALUE
   * @param monthOfYear the month-of-year, not null
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @param offset the zone offset, not null
   * @return the offset date, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def of(year: Int, monthOfYear: MonthOfYear, dayOfMonth: Int, offset: ZoneOffset): OffsetDate = {
    val date: LocalDate = LocalDate.of(year, monthOfYear, dayOfMonth)
    new OffsetDate(date, offset)
  }

  /**
   * Obtains an instance of    { @code OffsetDate } from a year, month and day.
   *
   * @param year the year to represent, from MIN_VALUE + 1 to MAX_VALUE
   * @param monthOfYear the month-of-year to represent, from 1 (January) to 12 (December)
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @param offset the zone offset, not null
   * @return the offset date, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def of(year: Int, monthOfYear: Int, dayOfMonth: Int, offset: ZoneOffset): OffsetDate = {
    val date: LocalDate = LocalDate.of(year, monthOfYear, dayOfMonth)
    new OffsetDate(date, offset)
  }

  /**
   * Obtains an instance of    { @code OffsetDate } from a text string using a specific formatter.
   * <p>
   * The text is parsed using the formatter, returning a date.
   *
   * @param text the text to parse, not null
   * @param formatter the formatter to use, not null
   * @return the parsed offset date, never null
   * @throws UnsupportedOperationException if the formatter cannot parse
   * @throws CalendricalException if the text cannot be parsed
   */
  def parse(text: String, formatter: DateTimeFormatter): OffsetDate = {
    ISOChronology.checkNotNull(formatter, "DateTimeFormatter must not be null")
    formatter.parse(text, rule)
  }

  /**
   * Obtains the current date from the system clock in the default time-zone.
   * <p>
   * This will query the system clock in the default time-zone to obtain the current date - today.
   * The offset will be set based on the time-zone in the system clock.
   * <p>
   * Using this method will prevent the ability to use an alternate clock for testing
   * because the clock is hard-coded.
   *
   * @return the current date using the system clock, never null
   */
  def nowSystemClock: OffsetDate = now(Clock.systemDefaultZone)

  /**
   * Obtains an instance of    { @code OffsetDate } from an    { @code InstantProvider }.
   * <p>
   * This conversion drops the time component of the instant.
   *
   * @param instantProvider the instant to convert, not null
   * @param offset the zone offset, not null
   * @return the offset date, never null
   * @throws CalendarConversionException if the instant exceeds the supported date range
   */
  def ofInstant(instantProvider: InstantProvider, offset: ZoneOffset): OffsetDate = {
    val instant: Instant = Instant.of(instantProvider)
    ISOChronology.checkNotNull(offset, "ZoneOffset must not be null")
    val epochSecs: Long = instant.getEpochSeconds + offset.getAmountSeconds
    val yearZeroDays: Long = MathUtils.floorDiv(epochSecs, ISOChronology.SECONDS_PER_DAY) + ISOChronology.DAYS_0000_TO_1970
    val date: LocalDate = LocalDate.ofYearZeroDays(yearZeroDays)
    new OffsetDate(date, offset)
  }

  /**
   * Obtains an instance of    { @code OffsetDate } from a text string.
   * <p>
   * The following format is accepted in ASCII:
   * <ul>
   * <li>   { @code   { Year } -   { MonthOfYear } -   { DayOfMonth } { OffsetID } }
   * </ul>
   * The year has between 4 and 10 digits with values from MIN_YEAR to MAX_YEAR.
   * If there are more than 4 digits then the year must be prefixed with the plus symbol.
   * Negative years are allowed, but not negative zero.
   * <p>
   * The month-of-year has 2 digits with values from 1 to 12.
   * <p>
   * The day-of-month has 2 digits with values from 1 to 31 appropriate to the month.
   * <p>
   * The offset ID is the normalized form as defined in    { @link ZoneOffset }.
   *
   * @param text the text to parse such as '2007-12-03+01:00', not null
   * @return the parsed offset date, never null
   * @throws CalendricalException if the text cannot be parsed
   */
  def parse(text: String): OffsetDate = {
    return DateTimeFormatters.isoOffsetDate.parse(text, rule)
  }

  /**
   * Gets the field rule for    { @code OffsetDate }.
   *
   * @return the field rule for the date, never null
   */
  def rule: CalendricalRule[OffsetDate] = Rule
}

/**
 * Constructor.
 *
 * @param date the date, validated as not null
 * @param offset the zone offset, validated as not null
 */
@SerialVersionUID(-3618963189L)
final class OffsetDate(date: LocalDate, offset: ZoneOffset) extends Calendrical with DateProvider with CalendricalMatcher with DateAdjuster with Comparable[OffsetDate] with Serializable {
  if (date == null) throw new NullPointerException("The date must not be null")
  if (offset == null) throw new NullPointerException("The zone offset must not be null")

  /**
   * A hash code for this date.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = {
    return date.hashCode ^ offset.hashCode
  }

  /**
   * Returns a copy of this OffsetDate with the date altered using the adjuster.
   * <p>
   * Adjusters can be used to alter the date in various ways.
   * A simple adjuster might simply set the one of the fields, such as the year field.
   * A more complex adjuster might set the date to the last day of the month.
   * <p>
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param adjuster the adjuster to use, not null
   * @return an { @code OffsetDate } based on this date adjusted as necessary, never null
   * @throws NullPointerException if the adjuster returned null
   */
  def `with`(adjuster: DateAdjuster): OffsetDate = `with`(date.`with`(adjuster), offset)

  /**
   * Returns a copy of this OffsetDate with the specified period in weeks added.
   * <p>
   * This method add the specified amount in weeks to the days field incrementing
   * the month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2008-12-31 plus one week would result in the 2009-01-07.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param weeks the weeks to add, may be negative
   * @return an { @code OffsetDate } based on this date with the weeks added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusWeeks(weeks: Int): OffsetDate = {
    return `with`(date.plusWeeks(weeks), offset)
  }

  /**
   * Is this date before the specified date.
   *
   * @param other the other date to compare to, not null
   * @return true if this point is before the specified date
   * @throws NullPointerException if    { @code other } is null
   */
  def isBefore(other: OffsetDate): Boolean = {
    return compareTo(other) < 0
  }

  /**
   * Returns a copy of this OffsetDate with the specified period in years added.
   * <p>
   * This method add the specified amount to the years field in three steps:
   * <ol>
   * <li>Add the input years to the year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the day-of-month to the last valid day if necessary</li>
   * </ol>
   * <p>
   * For example, 2008-02-29 (leap year) plus one year would result in the
   * invalid date 2009-02-29 (standard year). Instead of returning an invalid
   * result, the last valid day of the month, 2009-02-28, is selected instead.
   * <p>
   * This method does the same as    { @code plusYears ( years, DateResolvers.previousValid ( ) ) }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to add, may be negative
   * @return an { @code OffsetDate } based on this date with the years added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   * @see # plusYears ( int, javax.time.calendar.DateResolver )
   */
  def plusYears(years: Int): OffsetDate = {
    return `with`(date.plusYears(years), offset)
  }

  /**
   * Checks if the date extracted from the calendrical matches this.
   *
   * @param calendrical the calendrical to match, not null
   * @return true if the calendrical matches, false otherwise
   */
  override def matchesCalendrical(calendrical: Calendrical): Boolean = {
    return this.equals(calendrical.get(rule))
  }

  /**
   * Converts this date to a    { @code LocalDate }.
   *
   * @return a LocalDate with the same date as this instance, never null
   */
  override def toLocalDate: LocalDate = date

  /**
   * Gets the chronology that this date uses, which is the ISO calendar system.
   *
   * @return the ISO chronology, never null
   */
  def getChronology: ISOChronology = ISOChronology

  /**
   * Returns a copy of this OffsetDate with a different local date.
   * <p>
   * This method changes the date stored to a different date.
   * No calculation is performed. The result simply represents the same
   * offset and the new date.
   *
   * @param dateProvider the local date to change to, not null
   * @return an { @code OffsetDate } based on this date with the requested date, never null
   */
  def withDate(dateProvider: DateProvider): OffsetDate = {
    val newDate: LocalDate = LocalDate.of(dateProvider)
    if (newDate.equals(date)) this
    else `with`(newDate, offset)
  }

  /**
   * Returns a copy of this    { @code OffsetDate } with the year altered.
   * If the resulting date is invalid, it will be resolved using    { @link DateResolvers # previousValid ( ) }.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This method does the same as    { @code withYear ( year, DateResolvers.previousValid ( ) ) }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param year the year to set in the returned date, from MIN_YEAR to MAX_YEAR
   * @return an { @code OffsetDate } based on this date with the requested year, never null
   * @throws IllegalCalendarFieldValueException if the year value is invalid
   */
  def withYear(year: Int): OffsetDate = `with`(date.withYear(year), offset)

  /**
   * Returns a offset date-time formed from this date at the specified time.
   * <p>
   * This merges the four values -    { @code this } and the specified time -
   * to form an instance of    { @code OffsetDateTime }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay the hour-of-day to use, from 0 to 23
   * @param minuteOfHour the minute-of-hour to use, from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @return the offset date-time formed from this date and the specified time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   */
  def atTime(hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int): OffsetDateTime = {
    atTime(LocalTime.of(hourOfDay, minuteOfHour, secondOfMinute))
  }

  /**
   * Returns a copy of this OffsetDate with the specified period in months added.
   * <p>
   * This method add the specified amount to the months field in three steps:
   * <ol>
   * <li>Add the input months to the month-of-year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the date using    { @code dateResolver } if necessary</li>
   * </ol>
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to add, may be negative
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an { @code OffsetDate } based on this date with the months added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusMonths(months: Int, dateResolver: DateResolver): OffsetDate = {
    `with`(date.plusMonths(months, dateResolver), offset)
  }

  /**
   * Returns a copy of this    { @code OffsetDate } with the specified date period added.
   * <p>
   * This adds the specified period to this date, returning a new date.
   * Before addition, the period is converted to a date-based    { @code Period } using
   * { @link Period # ofDateFields ( PeriodProvider ) }.
   * That factory ignores any time-based ISO fields, thus adding a time-based
   * period to this date will have no effect. If you want to take time fields into
   * account, call    { @link Period # normalizedWith24HourDays ( ) } on the input period.
   * <p>
   * The detailed rules for the addition have some complexity due to variable length months.
   * See    { @link LocalDate # plus ( PeriodProvider ) } for details.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to add, not null
   * @return an { @code OffsetDate } based on this date with the period added, never null
   * @throws CalendricalException if the specified period cannot be converted to a    { @code Period }
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plus(periodProvider: PeriodProvider): OffsetDate = `with`(date.plus(periodProvider), offset)

  /**
   * Is this date after the specified date.
   *
   * @param other the other date to compare to, not null
   * @return true if this is after the specified date
   * @throws NullPointerException if    { @code other } is null
   */
  def isAfter(other: OffsetDate): Boolean = compareTo(other) > 0

  /**
   * Returns a copy of this OffsetDate with the specified number of days subtracted.
   * <p>
   * This method subtract the specified amount to the days field decrementing the
   * month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2009-01-01 minus one day would result in the 2008-12-31.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param days the days to subtract, may be negative
   * @return an { @code OffsetDate } based on this date with the days subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusDays(days: Long): OffsetDate = `with`(date.minusDays(days), offset)

  /**
   * Returns a offset date-time formed from this date at the specified time.
   * <p>
   * This merges the three values -    { @code this } and the specified time -
   * to form an instance of    { @code OffsetDateTime }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay the hour-of-day to use, from 0 to 23
   * @param minuteOfHour the minute-of-hour to use, from 0 to 59
   * @return the offset date-time formed from this date and the specified time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   */
  def atTime(hourOfDay: Int, minuteOfHour: Int): OffsetDateTime = atTime(LocalTime.of(hourOfDay, minuteOfHour))

  /**
   * Returns a copy of this OffsetDate with the specified period in years added.
   * <p>
   * This method add the specified amount to the years field in three steps:
   * <ol>
   * <li>Add the input years to the year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the date using    { @code dateResolver } if necessary</li>
   * </ol>
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to add, may be negative
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an { @code OffsetDate } based on this date with the years added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusYears(years: Int, dateResolver: DateResolver): OffsetDate = `with`(date.plusYears(years, dateResolver), offset)

  /**
   * Returns a copy of this    { @code OffsetDate } with the specified date period subtracted.
   * <p>
   * This subtracts the specified period from this date, returning a new date.
   * Before subtraction, the period is converted to a date-based    { @code Period } using
   * { @link Period # ofDateFields ( PeriodProvider ) }.
   * That factory ignores any time-based ISO fields, thus adding a time-based
   * period to this date will have no effect. If you want to take time fields into
   * account, call    { @link Period # normalizedWith24HourDays ( ) } on the input period.
   * <p>
   * The detailed rules for the subtraction have some complexity due to variable length months.
   * See    { @link LocalDate # minus ( PeriodProvider ) } for details.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to subtract, not null
   * @return an { @code OffsetDate } based on this date with the period subtracted, never null
   * @throws CalendricalException if the specified period cannot be converted to a    { @code Period }
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minus(periodProvider: PeriodProvider): OffsetDate = `with`(date.minus(periodProvider), offset)

  /**
   * Returns a copy of this OffsetDate with the specified period in months added.
   * <p>
   * This method add the specified amount to the months field in three steps:
   * <ol>
   * <li>Add the input months to the month-of-year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the day-of-month to the last valid day if necessary</li>
   * </ol>
   * <p>
   * For example, 2007-03-31 plus one month would result in the invalid date
   * 2007-04-31. Instead of returning an invalid result, the last valid day
   * of the month, 2007-04-30, is selected instead.
   * <p>
   * This method does the same as    { @code plusMonths ( months, DateResolvers.previousValid ( ) ) }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to add, may be negative
   * @return an { @code OffsetDate } based on this date with the months added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   * @see # plusMonths ( int, javax.time.calendar.DateResolver )
   */
  def plusMonths(months: Int): OffsetDate = `with`(date.plusMonths(months), offset)

  /**
   * Returns an offset date-time formed from this date at the specified time.
   * <p>
   * This merges the two objects -    { @code this } and the specified time -
   * to form an instance of    { @code OffsetDateTime }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param time the time to use, not null
   * @return the offset date-time formed from this date and the specified time, never null
   */
  def atTime(time: LocalTime): OffsetDateTime = OffsetDateTime.of(this, time, getOffset)

  /**
   * Gets the day-of-year field.
   * <p>
   * This method returns the primitive    { @code int } value for the day-of-year.
   *
   * @return the day-of-year, from 1 to 365, or 366 in a leap year
   */
  def getDayOfYear: Int = date.getDayOfYear

  /**
   * Returns a copy of this OffsetDate with the specified period in months subtracted.
   * <p>
   * This method subtract the specified amount to the months field in three steps:
   * <ol>
   * <li>Subtract the input months to the month-of-year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the date using    { @code dateResolver } if necessary</li>
   * </ol>
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to subtract, may be negative
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an { @code OffsetDate } based on this date with the months subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusMonths(months: Int, dateResolver: DateResolver): OffsetDate = {
    `with`(date.minusMonths(months, dateResolver), offset)
  }

  /**
   * Returns a copy of this    { @code OffsetDate } with the day-of-year altered.
   * If the resulting date is invalid, an exception is thrown.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfYear the day-of-year to set in the returned date, from 1 to 365-366
   * @return an { @code OffsetDate } based on this date with the requested day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-year value is invalid
   * @throws InvalidCalendarFieldException if the day-of-year is invalid for the year
   */
  def withDayOfYear(dayOfYear: Int): OffsetDate = `with`(date.withDayOfYear(dayOfYear), offset)

  /**
   * Returns a copy of this OffsetDate with the specified period in months subtracted.
   * <p>
   * This method subtract the specified amount to the months field in three steps:
   * <ol>
   * <li>Subtract the input months to the month-of-year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the day-of-month to the last valid day if necessary</li>
   * </ol>
   * <p>
   * For example, 2007-03-31 minus one month would result in the invalid date
   * 2007-02-31. Instead of returning an invalid result, the last valid day
   * of the month, 2007-02-28, is selected instead.
   * <p>
   * This method does the same as    { @code minusMonths ( months, DateResolvers.previousValid ( ) ) }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to subtract, may be negative
   * @return an { @code OffsetDate } based on this date with the months subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   * @see # minusMonths ( int, javax.time.calendar.DateResolver )
   */
  def minusMonths(months: Int): OffsetDate = `with`(date.minusMonths(months), offset)

  /**
   * Gets the day-of-month field.
   * <p>
   * This method returns the primitive    { @code int } value for the day-of-month.
   *
   * @return the day-of-month, from 1 to 31
   */
  def getDayOfMonth: Int = date.getDayOfMonth

  /**
   * Returns a copy of this    { @code OffsetDate } with the month-of-year altered.
   * If the resulting date is invalid, it will be resolved using    { @link DateResolvers # previousValid ( ) }.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This method does the same as    { @code with ( monthOfYear, DateResolvers.previousValid ( ) ) }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear the month-of-year to set in the returned date, not null
   * @return an { @code OffsetDate } based on this date with the requested month, never null
   */
  def `with`(monthOfYear: MonthOfYear): OffsetDate = `with`(date.`with`(monthOfYear), offset)

  /**
   * Returns a copy of this    { @code OffsetDate } with the day-of-month altered.
   * If the resulting date is invalid, it will be resolved using    { @code dateResolver }.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfMonth the day-of-month to set in the returned date, from 1 to 31
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an { @code OffsetDate } based on this date with the requested day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-month value is invalid
   */
  def withDayOfMonth(dayOfMonth: Int, dateResolver: DateResolver): OffsetDate = `with`(date.withDayOfMonth(dayOfMonth, dateResolver), offset)

  /**
   * Adjusts a date to have the value of the date part of this object.
   *
   * @param date the date to be adjusted, not null
   * @return the adjusted date, never null
   */
  override def adjustDate(date: LocalDate): LocalDate = this.date.adjustDate(date)

  /**
   * Returns a new date based on this one, returning    { @code this } where possible.
   *
   * @param date the date to create with, not null
   * @param offset the zone offset to create with, not null
   */
  private def `with`(date: LocalDate, offset: ZoneOffset): OffsetDate = {
    if (this.date == date && this.offset.equals(offset)) this
    else new OffsetDate(date, offset)
  }

  /**
   * Returns a offset date-time formed from this date at the specified time.
   * <p>
   * This merges the five values -    { @code this } and the specified time -
   * to form an instance of    { @code OffsetDateTime }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay the hour-of-day to use, from 0 to 23
   * @param minuteOfHour the minute-of-hour to use, from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @param nanoOfSecond the nano-of-second to represent, from 0 to 999,999,999
   * @return the offset date-time formed from this date and the specified time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   */
  def atTime(hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int, nanoOfSecond: Int): OffsetDateTime = {
    atTime(LocalTime.of(hourOfDay, minuteOfHour, secondOfMinute, nanoOfSecond))
  }

  /**
   * Outputs this date as a    { @code String } using the formatter.
   *
   * @param formatter the formatter to use, not null
   * @return the formatted date string, never null
   * @throws UnsupportedOperationException if the formatter cannot print
   * @throws CalendricalPrintException if an error occurs during printing
   */
  def toString(formatter: DateTimeFormatter): String = {
    ISOChronology.checkNotNull(formatter, "DateTimeFormatter must not be null")
    formatter.print(this)
  }

  /**
   * Returns a copy of this    { @code OffsetDate } with the day-of-month altered.
   * If the resulting date is invalid, an exception is thrown.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfMonth the day-of-month to set in the returned date, from 1 to 28-31
   * @return an { @code OffsetDate } based on this date with the requested day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-month value is invalid
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def withDayOfMonth(dayOfMonth: Int): OffsetDate = `with`(date.withDayOfMonth(dayOfMonth), offset)

  /**
   * Is this date equal to the specified date.
   * <p>
   * This compares the date and the offset.
   *
   * @param other the other date to compare to, null returns false
   * @return true if this point is equal to the specified date
   */
  override def equals(other: AnyRef): Boolean = {
    if (this == other) true
    else if (other.isInstanceOf[OffsetDate]) {
      val zonedDate: OffsetDate = other.asInstanceOf[OffsetDate]
      date.equals(zonedDate.date) && offset.equals(zonedDate.offset)
    }
    else false
  }

  /**
   * Returns a copy of this    { @code OffsetDate } with the year altered.
   * If the resulting date is invalid, it will be resolved using    { @code dateResolver }.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param year the year to set in the returned date, from MIN_YEAR to MAX_YEAR
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an { @code OffsetDate } based on this date with the requested year, never null
   * @throws IllegalCalendarFieldValueException if the year value is invalid
   */
  def withYear(year: Int, dateResolver: DateResolver): OffsetDate = `with`(date.withYear(year, dateResolver), offset)

  /**
   * Returns a copy of this    { @code OffsetDate } with the month-of-year altered.
   * If the resulting date is invalid, it will be resolved using    { @code dateResolver }.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear the month-of-year to set in the returned date, not null
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an { @code OffsetDate } based on this date with the requested month, never null
   */
  def `with`(monthOfYear: MonthOfYear, dateResolver: DateResolver): OffsetDate = `with`(date.`with`(monthOfYear, dateResolver), offset)

  /**
   * Returns a copy of this OffsetDate with the specified period in years subtracted.
   * <p>
   * This method subtract the specified amount to the years field in three steps:
   * <ol>
   * <li>Subtract the input years to the year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the day-of-month to the last valid day if necessary</li>
   * </ol>
   * <p>
   * For example, 2008-02-29 (leap year) minus one year would result in the
   * invalid date 2007-02-29 (standard year). Instead of returning an invalid
   * result, the last valid day of the month, 2007-02-28, is selected instead.
   * <p>
   * This method does the same as    { @code minusYears ( years, DateResolvers.previousValid ( ) ) }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to subtract, may be negative
   * @return an { @code OffsetDate } based on this date with the years subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   * @see # minusYears ( int, javax.time.calendar.DateResolver )
   */
  def minusYears(years: Int): OffsetDate = `with`(date.minusYears(years), offset)

  /**
   * Outputs this date as a    { @code String }, such as    { @code 2007 -12-03+01:00 }.
   * <p>
   * The output will be in the format    { @code yyyy -MM-ddZZZZ }.
   *
   * @return the formatted date, never null
   */
  override def toString: String = date.toString + offset.toString

  /**
   * Returns a zoned date-time from this date at the earliest valid time according
   * to the rules in the time-zone ignoring the current offset.
   * <p>
   * Time-zone rules, such as daylight savings, mean that not every time on the
   * local time-line exists. When this method converts the date to a date-time it
   * adjusts the time and offset as necessary to ensure that the time is as early
   * as possible on the date, which is typically midnight. Internally this is
   * achieved using the    { @link ZoneResolvers # postGapPreOverlap ( ) zone resolver }.
   * <p>
   * To convert to a specific time in a given time-zone call    { @link # atTime ( LocalTime ) }
   * followed by    { @link OffsetDateTime # atZoneSimilarLocal ( TimeZone ) }. Note that the resolver
   * used by    { @code atZoneSimilarLocal ( ) } is different to that used here (it chooses
   * the later offset in an overlap, whereas this method chooses the earlier offset).
   * <p>
   * The offset from this date is ignored during the conversion.
   * This ensures that the resultant date-time has the same date as this.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param zone the time-zone to use, not null
   * @return the zoned date-time formed from this date and the earliest valid time for the zone, never null
   */
  def atStartOfDayInZone(zone: TimeZone): ZonedDateTime = {
    ZonedDateTime.of(this, LocalTime.MIDNIGHT, zone, ZoneResolvers.postGapPreOverlap)
  }

  /**
   * Gets the value of the specified calendrical rule.
   * <p>
   * This method queries the value of the specified calendrical rule.
   * If the value cannot be returned for the rule from this date then
   * { @code null } will be returned.
   *
   * @param rule the rule to use, not null
   * @return the value for the rule, null if the value cannot be returned
   */
  def get[T](rule: CalendricalRule[T]): T = rule.deriveValueFor(rule, this, this)

  /**
   * Compares this date to another date based on the UTC equivalent dates
   * then local date.
   * <p>
   * This ordering is consistent with    { @code equals ( ) }.
   * For example, the following is the comparator order:
   * <ol>
   * <li>2008-06-29-11:00</li>
   * <li>2008-06-29-12:00</li>
   * <li>2008-06-30+12:00</li>
   * <li>2008-06-29-13:00</li>
   * </ol>
   * Values #2 and #3 represent the same instant on the time-line.
   * When two values represent the same instant, the local date is compared
   * to distinguish them. This step is needed to make the ordering
   * consistent with    { @code equals ( ) }.
   *
   * @param other the other date to compare to, not null
   * @return the comparator value, negative if less, positive if greater
   * @throws NullPointerException if    { @code other } is null
   */
  def compareTo(other: OffsetDate): Int = {
    if (offset.equals(other.offset)) date.compareTo(other.date)
    else {
      val thisDT: LocalDateTime = LocalDateTime.ofMidnight(getYear, getMonthOfYear, getDayOfMonth)
      val otherDT: LocalDateTime = LocalDateTime.ofMidnight(other.getYear, other.getMonthOfYear, other.getDayOfMonth)
      val thisUTC: LocalDateTime = thisDT.plusSeconds(-offset.getAmountSeconds)
      val otherUTC: LocalDateTime = otherDT.plusSeconds(-other.offset.getAmountSeconds)
      var compare: Int = thisUTC.compareTo(otherUTC)
      if (compare == 0) compare = date.compareTo(other.date)

      compare
    }
  }

  /**
   * Gets the day-of-week field, which is an enum    { @code DayOfWeek }.
   * <p>
   * This method returns the enum    { @link DayOfWeek } for the day-of-week.
   * This avoids confusion as to what    { @code int } values mean.
   * If you need access to the primitive    { @code int } value then the enum
   * provides the    { @link DayOfWeek # getValue ( ) int value }.
   * <p>
   * Additional information can be obtained from the    { @code DayOfWeek }.
   * This includes textual names of the values.
   *
   * @return the day-of-week, never null
   */
  def getDayOfWeek: DayOfWeek = date.getDayOfWeek

  /**
   * Checks whether this date matches the specified matcher.
   * <p>
   * Matchers can be used to query the date.
   * A simple matcher might simply compare one of the fields, such as the year field.
   * A more complex matcher might check if the date is the last day of the month.
   *
   * @param matcher the matcher to use, not null
   * @return true if this date matches the matcher, false otherwise
   */
  def matches(matcher: CalendricalMatcher): Boolean = matcher.matchesCalendrical(this)

  /**
   * Gets the year field as a    { @code Year }.
   * <p>
   * This method provides access to an object representing the year field.
   * { @code Year } has methods for querying addition year-based information.
   *
   * @return the year, never null
   */
  def toYear: Year = date.toYear

  /**
   * Gets the year field.
   * <p>
   * This method returns the primitive    { @code int } value for the year.
   * <p>
   * Additional information about the year can be obtained via    { @link # toYear }.
   * This returns a    { @code Year } object which includes information on whether
   * this is a leap year and its length in days.
   *
   * @return the year, from MIN_YEAR to MAX_YEAR
   */
  def getYear: Int = date.getYear

  /**
   * Returns a copy of this    { @code OffsetDate } with the month-of-year altered.
   * If the resulting date is invalid, it will be resolved using    { @link DateResolvers # previousValid ( ) }.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This method does the same as    { @code withMonthOfYear ( monthOfYear, DateResolvers.previousValid ( ) ) }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear the month-of-year to set in the returned date, from 1 (January) to 12 (December)
   * @return an { @code OffsetDate } based on this date with the requested month, never null
   * @throws IllegalCalendarFieldValueException if the month-of-year value is invalid
   */
  def withMonthOfYear(monthOfYear: Int): OffsetDate = `with`(date.withMonthOfYear(monthOfYear), offset)

  /**
   * Returns a copy of this    { @code OffsetDate } with the month-of-year altered.
   * If the resulting date is invalid, it will be resolved using    { @code dateResolver }.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear the month-of-year to set in the returned date, from 1 (January) to 12 (December)
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an { @code OffsetDate } based on this date with the requested month, never null
   * @throws IllegalCalendarFieldValueException if the month-of-year value is invalid
   */
  def withMonthOfYear(monthOfYear: Int, dateResolver: DateResolver): OffsetDate = {
    `with`(date.withMonthOfYear(monthOfYear, dateResolver), offset)
  }

  /**
   * Returns a copy of this OffsetDate with the specified period in years subtracted.
   * <p>
   * This method subtract the specified amount to the years field in three steps:
   * <ol>
   * <li>Subtract the input years to the year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the date using    { @code dateResolver } if necessary</li>
   * </ol>
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to subtract, may be negative
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an { @code OffsetDate } based on this date with the years subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusYears(years: Int, dateResolver: DateResolver): OffsetDate = {
    `with`(date.minusYears(years, dateResolver), offset)
  }

  /**
   * Gets the month-of-year field, which is an enum    { @code MonthOfYear }.
   * <p>
   * This method returns the enum    { @link MonthOfYear } for the month.
   * This avoids confusion as to what    { @code int } values mean.
   * If you need access to the primitive    { @code int } value then the enum
   * provides the    { @link MonthOfYear # getValue ( ) int value }.
   * <p>
   * Additional information can be obtained from the    { @code MonthOfYear }.
   * This includes month lengths, textual names and access to the quarter-of-year
   * and month-of-quarter values.
   *
   * @return the month-of-year, never null
   */
  def getMonthOfYear: MonthOfYear = date.getMonthOfYear

  /**
   * Returns a copy of this OffsetDate with the specified period in days added.
   * <p>
   * This method add the specified amount to the days field incrementing the
   * month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2008-12-31 plus one day would result in the 2009-01-01.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param days the days to add, may be negative
   * @return an { @code OffsetDate } based on this date with the days added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusDays(days: Long): OffsetDate = `with`(date.plusDays(days), offset)

  /**
   * Returns an offset date-time formed from this date at the time of midnight.
   * <p>
   * This merges the two objects -    { @code this } and    { @link LocalTime # MIDNIGHT } -
   * to form an instance of    { @code OffsetDateTime }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @return the offset date-time formed from this date and the time of midnight, never null
   */
  def atMidnight: OffsetDateTime = OffsetDateTime.of(this, LocalTime.MIDNIGHT, getOffset)

  /**
   * Returns a copy of this OffsetDate with a different zone offset.
   * <p>
   * This method changes the offset stored to a different offset.
   * No calculation is performed. The result simply represents the same
   * local date and the new offset.
   *
   * @param offset the zone offset to change to, not null
   * @return a new updated OffsetDate, never null
   */
  def withOffset(offset: ZoneOffset): OffsetDate = {
    ISOChronology.checkNotNull(offset, "ZoneOffset must not be null")
    `with`(date, offset)
  }

  /**
   * Returns a copy of this OffsetDate with the specified period in weeks subtracted.
   * <p>
   * This method subtract the specified amount in weeks to the days field incrementing
   * the month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2009-01-07 minus one week would result in the 2008-12-31.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param weeks the weeks to subtract, may be negative
   * @return an { @code OffsetDate } based on this date with the weeks subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusWeeks(weeks: Int): OffsetDate = {
    `with`(date.minusWeeks(weeks), offset)
  }

  /**
   * Gets the zone offset.
   *
   * @return the zone offset, never null
   */
  def getOffset: ZoneOffset = offset
}