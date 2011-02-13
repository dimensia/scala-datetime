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
package scalax.time.calendar

import java.io.Serializable
import scalax.time.Instant
import scalax.time.InstantProvider
import scalax.time.MathUtils
import scalax.time.calendar.format.DateTimeFormatter
import scalax.time.calendar.format.DateTimeFormatters

/**
 * A date with a zone offset from UTC in the ISO-8601 calendar system,
 * such as {@code 2007-12-03+01:00}.
 * <p>
 * {@code OffsetDate} is an immutable calendrical that represents a date, often viewed
 * as year-month-day-offset. This object can also access other date fields such as
 * day-of-year, day-of-week and week-of-year.
 * <p>
 * This class does not store or represent a time.
 * Thus, for example, the value "2nd October 2007 +02:00" can be stored
 * in a {@code OffsetDate}.
 * <p>
 * OffsetDate is immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */

object OffsetDate {

  /**
   * Obtains the current date from the specified clock.
   * <p>
   * This will query the specified clock to obtain the current date - today.
   * The offset will be calculated from the time-zone in the clock.
   * <p>
   * Using this method allows the use of an alternate clock for testing.
   * The alternate clock may be introduced using {@link Clock dependency injection}.
   *
   * @param clock  the clock to use, by default {@code Clock.systemDefaultZone}, not null
   * @return the current date, never null
   */
  def now(implicit clock: Clock = Clock.systemDefaultZone): OffsetDate = {
    ISOChronology.checkNotNull(clock, "Clock must not be null")
    val now: Instant = clock.instant
    ofInstant(now, clock.getZone.getRules.getOffset(now))
  }

  /**
   * Obtains an instance of {@code OffsetDate} from a year, month and day.
   *
   * @param year  the year to represent, from MIN_YEAR to MAX_YEAR
   * @param monthOfYear  the month-of-year to represent, not null
   * @param dayOfMonth  the day-of-month to represent, from 1 to 31
   * @param offset  the zone offset, not null
   * @return the offset date, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def of(year: Int, monthOfYear: MonthOfYear, dayOfMonth: Int, offset: ZoneOffset): OffsetDate = {
    val date: Date = Date(year, monthOfYear, dayOfMonth)
    new OffsetDate(date, offset)
  }

  /**
   * Obtains an instance of {@code OffsetDate} from a year, month and day.
   *
   * @param year  the year to represent, from MinYear to MaxYear
   * @param monthOfYear  the month-of-year to represent, from 1 (January) to 12 (December)
   * @param dayOfMonth  the day-of-month to represent, from 1 to 31
   * @param offset  the zone offset, not null
   * @return the offset date, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def of(year: Int, monthOfYear: Int, dayOfMonth: Int, offset: ZoneOffset): OffsetDate = {
    val date: Date = Date(year, monthOfYear, dayOfMonth)
    new OffsetDate(date, offset)
  }

  /**
   * Obtains an instance of {@code OffsetDate} from a date provider.
   *
   * @param dateProvider  the date provider to use, not null
   * @param offset  the zone offset, not null
   * @return the offset date, never null
   */
  def of(dateProvider: DateProvider, offset: ZoneOffset): OffsetDate = {
    val date: Date = Date(dateProvider)
    new OffsetDate(date, offset)
  }

  /**
   * Obtains an instance of {@code OffsetDate} from an {@code InstantProvider}.
   * <p>
   * This conversion drops the time component of the instant effectively
   * converting at midnight at the start of the day.
   *
   * @param instantProvider  the instant to convert, not null
   * @param offset  the zone offset, not null
   * @return the offset date, never null
   * @throws CalendarConversionException if the instant exceeds the supported date range
   */
  def ofInstant(instantProvider: InstantProvider, offset: ZoneOffset): OffsetDate = {
    val instant: Instant = Instant.of(instantProvider)
    ISOChronology.checkNotNull(offset, "ZoneOffset must not be null")
    val epochSecs: Long = instant.seconds + offset.getAmountSeconds
    val yearZeroDays: Long = MathUtils.floorDiv(epochSecs, ISOChronology.SecondsPerDay) + ISOChronology.Days0000To1970
    val date: Date = Date.ofYearZeroDays(yearZeroDays)
    new OffsetDate(date, offset)
  }

  /**
   * Obtains an instance of {@code OffsetDate} from a text string such as {@code 2007-12-03+01:00}.
   * <p>
   * The following format is accepted in ASCII:
   * <ul>
   * <li>{@code {Year}-{MonthOfYear}-{DayOfMonth}{OffsetID}}
   * </ul>
   * The year has between 4 and 10 digits with values from MIN_YEAR to MAX_YEAR.
   * If there are more than 4 digits then the year must be prefixed with the plus symbol.
   * Negative years are allowed, but not negative zero.
   * <p>
   * The month-of-year has 2 digits with values from 1 to 12.
   * <p>
   * The day-of-month has 2 digits with values from 1 to 31 appropriate to the month.
   * <p>
   * The offset ID is the normalized form as defined in {@link ZoneOffset}.
   *
   * @param text  the text to parse such as '2007-12-03+01:00', not null
   * @return the parsed offset date, never null
   * @throws CalendricalException if the text cannot be parsed
   */
  def parse(text: String): OffsetDate = DateTimeFormatters.isoOffsetDate.parse(text, rule)

  /**
   * Obtains an instance of {@code OffsetDate} from a text string using a specific formatter.
   * <p>
   * The text is parsed using the formatter, returning a date.
   *
   * @param text  the text to parse, not null
   * @param formatter  the formatter to use, not null
   * @return the parsed offset date, never null
   * @throws UnsupportedOperationException if the formatter cannot parse
   * @throws CalendricalException if the text cannot be parsed
   */
  def parse(text: String, formatter: DateTimeFormatter): OffsetDate = {
    ISOChronology.checkNotNull(formatter, "DateTimeFormatter must not be null")
    formatter.parse(text, rule)
  }

  /**
   * Gets the rule for {@code OffsetDate}.
   *
   * @return the rule for the date, never null
   */
  def rule = Rule

  /**
   * Rule implementation.
   */
  private[calendar] object Rule extends Rule

  @SerialVersionUID(1L)
  private[calendar] sealed class Rule
    extends CalendricalRule[OffsetDate](classOf[OffsetDate], ISOChronology, "OffsetDate", ISOChronology.periodDays, null) with Serializable {

    private def readResolve: AnyRef = Rule

    override def derive(calendrical: Calendrical): Option[OffsetDate] = calendrical.get(OffsetDateTime.rule).map(_.toOffsetDate)
  }

}

/**
 * Constructor.
 *
 * @param date  the date, validated as not null
 * @param offset  the zone offset, validated as not null
 */
@SerialVersionUID(-3618963189L)
final class OffsetDate private(val date: Date, val offset: ZoneOffset) extends Calendrical with DateProvider with CalendricalMatcher with DateAdjuster with Comparable[OffsetDate] with Serializable {
  if (date == null) throw new NullPointerException("The date must not be null")
  if (offset == null) throw new NullPointerException("The zone offset must not be null")

  import OffsetDate._

  /**
   * Returns a new date based on this one, returning {@code this} where possible.
   *
   * @param date  the date to create with, not null
   * @param offset  the zone offset to create with, not null
   */
  private def `with`(date: Date, offset: ZoneOffset): OffsetDate = {
    if (this.date == date && this.offset.equals(offset)) this
    else new OffsetDate(date, offset)
  }

  /**
   * Gets the chronology that this date uses, which is the ISO calendar system.
   *
   * @return the ISO chronology, never null
   */
  def getChronology: ISOChronology = ISOChronology

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
  //  def get[T](rule: CalendricalRule[T]): Option[T] = rule.deriveValueFor(rule, this, this, ISOChronology) //FIXME
  def get[T](rule: CalendricalRule[T]): Option[T] = None

  /**
   * Gets the zone offset.
   *
   * @return the zone offset, never null
   */
  def getOffset: ZoneOffset = offset

  /**
   * Returns a copy of this {@code OffsetDate} with the specified offset.
   * <p>
   * This method returns an object with the same {@code Date} and the specified {@code ZoneOffset}.
   * No calculation is needed or performed.
   * For example, if this time represents {@code 2007-12-03+02:00} and the offset specified is
   * {@code +03:00}, then this method will return {@code 2007-12-03+03:00}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param offset  the zone offset to change to, not null
   * @return an {@code OffsetDate} based on this date with the requested offset, never null
   */
  def withOffset(offset: ZoneOffset): OffsetDate = {
    ISOChronology.checkNotNull(offset, "ZoneOffset must not be null")
    `with`(date, offset)
  }

  /**
   * Gets the year field.
   * <p>
   * This method returns the primitive {@code int} value for the year.
   * Additional information about the year can be obtained by creating a {@link Year}.
   *
   * @return the year, from MIN_YEAR to MAX_YEAR
   */
  def getYear: Int = date.getYear

  /**
   * Gets the month-of-year field, which is an enum {@code MonthOfYear}.
   * <p>
   * This method returns the enum {@link MonthOfYear} for the month.
   * This avoids confusion as to what {@code int} values mean.
   * If you need access to the primitive {@code int} value then the enum
   * provides the {@link MonthOfYear#getValue() int value}.
   * <p>
   * Additional information can be obtained from the {@code MonthOfYear}.
   * This includes month lengths, textual names and access to the quarter-of-year
   * and month-of-quarter values.
   *
   * @return the month-of-year, never null
   */
  def getMonthOfYear: MonthOfYear = date.getMonthOfYear

  /**
   * Gets the day-of-month field.
   * <p>
   * This method returns the primitive {@code int} value for the day-of-month.
   *
   * @return the day-of-month, from 1 to 31
   */
  def getDayOfMonth: Int = date.getDayOfMonth

  /**
   * Gets the day-of-year field.
   * <p>
   * This method returns the primitive {@code int} value for the day-of-year.
   *
   * @return the day-of-year, from 1 to 365, or 366 in a leap year
   */
  def getDayOfYear: Int = date.getDayOfYear

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
  def getDayOfWeek: DayOfWeek = date.getDayOfWeek

  /**
   * Checks if the year is a leap year, according to the ISO proleptic
   * calendar system rules.
   * <p>
   * This method applies the current rules for leap years across the whole time-line.
   * In general, a year is a leap year if it is divisible by four without
   * remainder. However, years divisible by 100, are not leap years, with
   * the exception of years divisible by 400 which are.
   * <p>
   * For example, 1904 is a leap year it is divisible by 4.
   * 1900 was not a leap year as it is divisible by 100, however 2000 was a
   * leap year as it is divisible by 400.
   * <p>
   * The calculation is proleptic - applying the same rules into the far future and far past.
   * This is historically inaccurate, but is correct for the ISO8601 standard.
   *
   * @return true if the year is leap, false otherwise
   */
  def isLeapYear: Boolean = date.isLeapYear

  /**
   * Returns a copy of this {@code OffsetDate} with the date altered using the adjuster.
   * <p>
   * Adjusters can be used to alter the date in various ways.
   * A simple adjuster might simply set the one of the fields, such as the year field.
   * A more complex adjuster might set the date to the last day of the month.
   * <p>
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param adjuster  the adjuster to use, not null
   * @return an {@code OffsetDate} based on this date adjusted as necessary, never null
   * @throws NullPointerException if the adjuster returned null
   */
  def `with`(adjuster: DateAdjuster): OffsetDate = `with`(date.`with`(adjuster), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with a different local date.
   * <p>
   * This method changes the date stored to a different date.
   * No calculation is performed. The result simply represents the same
   * offset and the new date.
   *
   * @param dateProvider  the local date to change to, not null
   * @return an {@code OffsetDate} based on this date with the requested date, never null
   */
  def withDate(dateProvider: DateProvider): OffsetDate = {
    val newDate: Date = Date(dateProvider)
    if (newDate.equals(date)) this
    else `with`(newDate, offset)
  }

  /**
   * Returns a copy of this {@code OffsetDate} with the year altered.
   * If the resulting date is invalid, it will be resolved using {@link DateResolvers#previousValid()}.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This method does the same as {@code withYear(year, DateResolvers.previousValid())}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param year  the year to set in the returned date, from MIN_YEAR to MAX_YEAR
   * @return an {@code OffsetDate} based on this date with the requested year, never null
   * @throws IllegalCalendarFieldValueException if the year value is invalid
   */
  def withYear(year: Int): OffsetDate = `with`(date.withYear(year), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the year altered.
   * If the resulting date is invalid, it will be resolved using {@code dateResolver}.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param year  the year to set in the returned date, from MIN_YEAR to MAX_YEAR
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an {@code OffsetDate} based on this date with the requested year, never null
   * @throws IllegalCalendarFieldValueException if the year value is invalid
   */
  def withYear(year: Int, dateResolver: DateResolver): OffsetDate = `with`(date.withYear(year, dateResolver), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the month-of-year altered.
   * If the resulting date is invalid, it will be resolved using {@link DateResolvers#previousValid()}.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This method does the same as {@code withMonthOfYear(monthOfYear, DateResolvers.previousValid())}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear  the month-of-year to set in the returned date, from 1 (January) to 12 (December)
   * @return an {@code OffsetDate} based on this date with the requested month, never null
   * @throws IllegalCalendarFieldValueException if the month-of-year value is invalid
   */
  def withMonthOfYear(monthOfYear: Int): OffsetDate = `with`(date.withMonthOfYear(monthOfYear), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the month-of-year altered.
   * If the resulting date is invalid, it will be resolved using {@code dateResolver}.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear  the month-of-year to set in the returned date, from 1 (January) to 12 (December)
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an {@code OffsetDate} based on this date with the requested month, never null
   * @throws IllegalCalendarFieldValueException if the month-of-year value is invalid
   */
  def withMonthOfYear(monthOfYear: Int, dateResolver: DateResolver): OffsetDate = `with`(date.withMonthOfYear(monthOfYear, dateResolver), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the month-of-year altered.
   * If the resulting date is invalid, it will be resolved using {@link DateResolvers#previousValid()}.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This method does the same as {@code with(monthOfYear, DateResolvers.previousValid())}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear  the month-of-year to set in the returned date, not null
   * @return an {@code OffsetDate} based on this date with the requested month, never null
   */
  def `with`(monthOfYear: MonthOfYear): OffsetDate = `with`(date.`with`(monthOfYear), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the month-of-year altered.
   * If the resulting date is invalid, it will be resolved using {@code dateResolver}.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear  the month-of-year to set in the returned date, not null
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an {@code OffsetDate} based on this date with the requested month, never null
   */
  def `with`(monthOfYear: MonthOfYear, dateResolver: DateResolver): OffsetDate =
    `with`(date.`with`(monthOfYear, dateResolver), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the day-of-month altered.
   * If the resulting date is invalid, an exception is thrown.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfMonth  the day-of-month to set in the returned date, from 1 to 28-31
   * @return an {@code OffsetDate} based on this date with the requested day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-month value is invalid
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def withDayOfMonth(dayOfMonth: Int): OffsetDate = `with`(date.copy(day = dayOfMonth), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the day-of-month altered.
   * If the resulting date is invalid, it will be resolved using {@code dateResolver}.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfMonth  the day-of-month to set in the returned date, from 1 to 31
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an {@code OffsetDate} based on this date with the requested day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-month value is invalid
   */
  def withDayOfMonth(dayOfMonth: Int, dateResolver: DateResolver): OffsetDate =
    `with`(date.withDayOfMonth(dayOfMonth, dateResolver), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the day-of-year altered.
   * If the resulting date is invalid, an exception is thrown.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfYear  the day-of-year to set in the returned date, from 1 to 365-366
   * @return an {@code OffsetDate} based on this date with the requested day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-year value is invalid
   * @throws InvalidCalendarFieldException if the day-of-year is invalid for the year
   */
  def withDayOfYear(dayOfYear: Int): OffsetDate = `with`(date.withDayOfYear(dayOfYear), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the specified date period added.
   * <p>
   * This adds the specified period to this date, returning a new date.
   * Before addition, the period is converted to a date-based {@code Period} using
   * {@link Period#ofDateFields(PeriodProvider)}.
   * That factory ignores any time-based ISO fields, thus adding a time-based
   * period to this date will have no effect. If you want to take time fields into
   * account, call {@link Period#normalizedWith24HourDays()} on the input period.
   * <p>
   * The detailed rules for the addition have some complexity due to variable length months.
   * See {@link Date#plus(PeriodProvider)} for details.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider  the period to add, not null
   * @return an {@code OffsetDate} based on this date with the period added, never null
   * @throws CalendricalException if the specified period cannot be converted to a {@code Period}
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plus(periodProvider: PeriodProvider): OffsetDate = `with`(date.plus(periodProvider), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the specified period in years added.
   * <p>
   * This method adds the specified amount to the years field in three steps:
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
   * This method does the same as {@code plusYears(years, DateResolvers.previousValid())}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years  the years to add, may be negative
   * @return an {@code OffsetDate} based on this date with the years added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   * @see #plusYears(int, scalax.time.calendar.DateResolver)
   */
  def plusYears(years: Long): OffsetDate = `with`(date.plusYears(years), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the specified period in years added.
   * <p>
   * This method adds the specified amount to the years field in three steps:
   * <ol>
   * <li>Add the input years to the year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the date using {@code dateResolver} if necessary</li>
   * </ol>
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years  the years to add, may be negative
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an {@code OffsetDate} based on this date with the years added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusYears(years: Long, dateResolver: DateResolver): OffsetDate = `with`(date.plusYears(years, dateResolver), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the specified period in months added.
   * <p>
   * This method adds the specified amount to the months field in three steps:
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
   * This method does the same as {@code plusMonths(months, DateResolvers.previousValid())}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months  the months to add, may be negative
   * @return an {@code OffsetDate} based on this date with the months added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   * @see #plusMonths(int, scalax.time.calendar.DateResolver)
   */
  def plusMonths(months: Long): OffsetDate = `with`(date.plusMonths(months), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the specified period in months added.
   * <p>
   * This method adds the specified amount to the months field in three steps:
   * <ol>
   * <li>Add the input months to the month-of-year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the date using {@code dateResolver} if necessary</li>
   * </ol>
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months  the months to add, may be negative
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an {@code OffsetDate} based on this date with the months added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusMonths(months: Long, dateResolver: DateResolver): OffsetDate =
    `with`(date.plusMonths(months, dateResolver), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the specified period in weeks added.
   * <p>
   * This method adds the specified amount in weeks to the days field incrementing
   * the month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2008-12-31 plus one week would result in 2009-01-07.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param weeks  the weeks to add, may be negative
   * @return an {@code OffsetDate} based on this date with the weeks added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusWeeks(weeks: Long): OffsetDate = `with`(date.plusWeeks(weeks), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the specified period in days added.
   * <p>
   * This method adds the specified amount to the days field incrementing the
   * month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2008-12-31 plus one day would result in 2009-01-01.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param days  the days to add, may be negative
   * @return an {@code OffsetDate} based on this date with the days added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusDays(days: Long): OffsetDate = `with`(date.plusDays(days), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the specified date period subtracted.
   * <p>
   * This subtracts the specified period from this date, returning a new date.
   * Before subtraction, the period is converted to a date-based {@code Period} using
   * {@link Period#ofDateFields(PeriodProvider)}.
   * That factory ignores any time-based ISO fields, thus subtracting a time-based
   * period from this date will have no effect. If you want to take time fields into
   * account, call {@link Period#normalizedWith24HourDays()} on the input period.
   * <p>
   * The detailed rules for the subtraction have some complexity due to variable length months.
   * See {@link Date#minus(PeriodProvider)} for details.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider  the period to subtract, not null
   * @return an {@code OffsetDate} based on this date with the period subtracted, never null
   * @throws CalendricalException if the specified period cannot be converted to a {@code Period}
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minus(periodProvider: PeriodProvider): OffsetDate = `with`(date.minus(periodProvider), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the specified period in years subtracted.
   * <p>
   * This method subtracts the specified amount from the years field in three steps:
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
   * This method does the same as {@code minusYears(years, DateResolvers.previousValid())}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years  the years to subtract, may be negative
   * @return an {@code OffsetDate} based on this date with the years subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   * @see #minusYears(int, scalax.time.calendar.DateResolver)
   */
  def minusYears(years: Long): OffsetDate = `with`(date.minusYears(years), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the specified period in years subtracted.
   * <p>
   * This method subtracts the specified amount from the years field in three steps:
   * <ol>
   * <li>Subtract the input years to the year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the date using {@code dateResolver} if necessary</li>
   * </ol>
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years  the years to subtract, may be negative
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an {@code OffsetDate} based on this date with the years subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusYears(years: Long, dateResolver: DateResolver): OffsetDate = `with`(date.minusYears(years, dateResolver), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the specified period in months subtracted.
   * <p>
   * This method subtracts the specified amount from the months field in three steps:
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
   * This method does the same as {@code minusMonths(months, DateResolvers.previousValid())}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months  the months to subtract, may be negative
   * @return an {@code OffsetDate} based on this date with the months subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   * @see #minusMonths(int, scalax.time.calendar.DateResolver)
   */
  def minusMonths(months: Long): OffsetDate = `with`(date.minusMonths(months), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the specified period in months subtracted.
   * <p>
   * This method subtracts the specified amount from the months field in three steps:
   * <ol>
   * <li>Subtract the input months to the month-of-year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the date using {@code dateResolver} if necessary</li>
   * </ol>
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months  the months to subtract, may be negative
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an {@code OffsetDate} based on this date with the months subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusMonths(months: Long, dateResolver: DateResolver): OffsetDate = `with`(date.minusMonths(months, dateResolver), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the specified period in weeks subtracted.
   * <p>
   * This method subtracts the specified amount in weeks from the days field decrementing
   * the month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2009-01-07 minus one week would result in 2008-12-31.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param weeks  the weeks to subtract, may be negative
   * @return an {@code OffsetDate} based on this date with the weeks subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusWeeks(weeks: Long): OffsetDate = `with`(date.minusWeeks(weeks), offset)

  /**
   * Returns a copy of this {@code OffsetDate} with the specified number of days subtracted.
   * <p>
   * This method subtracts the specified amount from the days field decrementing the
   * month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2009-01-01 minus one day would result in 2008-12-31.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param days  the days to subtract, may be negative
   * @return an {@code OffsetDate} based on this date with the days subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusDays(days: Long): OffsetDate = `with`(date.minusDays(days), offset)

  /**
   * Checks whether this {@code OffsetDate} matches the specified matcher.
   * <p>
   * Matchers can be used to query the date.
   * A simple matcher might simply compare one of the fields, such as the year field.
   * A more complex matcher might check if the date is the last day of the month.
   *
   * @param matcher  the matcher to use, not null
   * @return true if this date matches the matcher, false otherwise
   */
  def matches(matcher: CalendricalMatcher): Boolean = matcher.matchesCalendrical(this)

  /**
   * Checks if the date extracted from the calendrical matches this date.
   * <p>
   * This method implements the {@code CalendricalMatcher} interface.
   * It is intended that applications use {@link #matches} rather than this method.
   *
   * @param calendrical  the calendrical to match, not null
   * @return true if the calendrical matches, false otherwise
   */
  def matchesCalendrical(calendrical: Calendrical): Boolean = this.equals(calendrical.get(rule))

  /**
   * Adjusts a date to have the value of the date part of this object.
   * <p>
   * This method implements the {@code DateAdjuster} interface.
   * It is intended that applications use {@link #with(DateAdjuster)} rather than this method.
   *
   * @param date  the date to be adjusted, not null
   * @return the adjusted date, never null
   */
  def adjustDate(date: Date): Date = this.date.adjustDate(date)

  /**
   * Returns an offset date-time formed from this date at the specified time.
   * <p>
   * This merges the two objects - {@code this} and the specified time -
   * to form an instance of {@code OffsetDateTime}.
   * If the offset of the time differs from the offset of the date, then the
   * result will have the offset of the date and the time will be adjusted to match.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param time  the time to use, not null
   * @return the offset date-time formed from this date and the specified time, never null
   */
  def atTime(time: OffsetDateTime): OffsetDateTime = OffsetDateTime.of(this, time.withOffsetSameInstant(offset), offset)

  /**
   * Returns an offset date-time formed from this date at the specified time.
   * <p>
   * This merges the two objects - {@code this} and the specified time -
   * to form an instance of {@code OffsetDateTime}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param time  the time to use, not null
   * @return the offset date-time formed from this date and the specified time, never null
   */
  def atTime(time: Time): OffsetDateTime = OffsetDateTime.of(this, time, offset)

  /**
   * Returns an offset date-time formed from this date at the specified time.
   * <p>
   * This merges the three values - {@code this} and the specified time -
   * to form an instance of {@code OffsetDateTime}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay  the hour-of-day to use, from 0 to 23
   * @param minuteOfHour  the minute-of-hour to use, from 0 to 59
   * @return the offset date-time formed from this date and the specified time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   */
  def atTime(hourOfDay: Int, minuteOfHour: Int): OffsetDateTime = atTime(Time.of(hourOfDay, minuteOfHour))

  /**
   * Returns an offset date-time formed from this date at the specified time.
   * <p>
   * This merges the four values - {@code this} and the specified time -
   * to form an instance of {@code OffsetDateTime}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay  the hour-of-day to use, from 0 to 23
   * @param minuteOfHour  the minute-of-hour to use, from 0 to 59
   * @param secondOfMinute  the second-of-minute to represent, from 0 to 59
   * @return the offset date-time formed from this date and the specified time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   */
  def atTime(hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int): OffsetDateTime =
    atTime(Time.of(hourOfDay, minuteOfHour, secondOfMinute))

  /**
   * Returns an offset date-time formed from this date at the specified time.
   * <p>
   * This merges the five values - {@code this} and the specified time -
   * to form an instance of {@code OffsetDateTime}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay  the hour-of-day to use, from 0 to 23
   * @param minuteOfHour  the minute-of-hour to use, from 0 to 59
   * @param secondOfMinute  the second-of-minute to represent, from 0 to 59
   * @param nanoOfSecond  the nano-of-second to represent, from 0 to 999,999,999
   * @return the offset date-time formed from this date and the specified time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   */
  def atTime(hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int, nanoOfSecond: Int): OffsetDateTime =
    atTime(Time.of(hourOfDay, minuteOfHour, secondOfMinute, nanoOfSecond))

  /**
   * Returns an offset date-time formed from this date at the time of midnight.
   * <p>
   * This merges the two objects - {@code this} and {@link Time#MIDNIGHT} -
   * to form an instance of {@code OffsetDateTime}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @return the offset date-time formed from this date and the time of midnight, never null
   */
  def atMidnight: OffsetDateTime = OffsetDateTime.of(this, Time.Midnight, offset)

  /**
   * Returns a zoned date-time from this date at the earliest valid time according
   * to the rules in the time-zone ignoring the current offset.
   * <p>
   * Time-zone rules, such as daylight savings, mean that not every time on the
   * local time-line exists. When this method converts the date to a date-time it
   * adjusts the time and offset as necessary to ensure that the time is as early
   * as possible on the date, which is typically midnight. Internally this is
   * achieved using the {@link ZoneResolvers#postGapPreOverlap() zone resolver}.
   * <p>
   * To convert to a specific time in a given time-zone call {@link #atTime(Time)}
   * followed by {@link OffsetDateTime#atZoneSimilarLocal(ZoneId)}. Note that the resolver
   * used by {@code atZoneSimilarLocal()} is different to that used here (it chooses
   * the later offset in an overlap, whereas this method chooses the earlier offset).
   * <p>
   * The offset from this date is ignored during the conversion.
   * This ensures that the resultant date-time has the same date as this.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param zone  the time-zone to use, not null
   * @return the zoned date-time formed from this date and the earliest valid time for the zone, never null
   */
  def atStartOfDayInZone(zone: ZoneId): ZonedDateTime =
    ZonedDateTime.of(this, Time.Midnight, zone, ZoneResolvers.postGapPreOverlap)

  /**
   * Converts this date to an {@code Instant} at midnight.
   * <p>
   * This conversion treats the time component as midnight at the start of the day.
   *
   * @return an instant equivalent to midnight at the start of this day, never null
   */
  def toInstant: Instant = {
    val epochSecs: Long = toEpochSeconds
    Instant.ofEpochSeconds(epochSecs, 0)
  }

  /**
   * Converts this date to a {@code Date}.
   *
   * @return a local date with the same date as this instance, never null
   */
  def toLocalDate: Date = date

  /**
   * Converts this date to midnight at the start of day in epoch seconds.
   *
   * @return the epoch seconds value
   */
  private def toEpochSeconds: Long = {
    val epochDays: Long = date.toEpochDays
    val secs: Long = epochDays * ISOChronology.SecondsPerDay
    secs - offset.getAmountSeconds
  }

  /**
   * Compares this {@code OffsetDate} to another date based on the UTC equivalent
   * dates then local date.
   * <p>
   * This ordering is consistent with {@code equals()}.
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
   * consistent with {@code equals()}.
   *
   * @param other  the other date to compare to, not null
   * @return the comparator value, negative if less, positive if greater
   */
  def compareTo(other: OffsetDate): Int = {
    if (offset == other.offset) {
      return date.compareTo(other.date)
    }
    var compare: Int = MathUtils.safeCompare(toEpochSeconds, other.toEpochSeconds)
    if (compare == 0) {
      compare = date.compareTo(other.date)
    }
    return compare
  }

  /**
   * Checks if the instant of midnight at the start of this {@code OffsetDate}
   * is after midnight at the start of the specified date.
   * <p>
   * This method differs from the comparison in {@link #compareTo} in that it
   * only compares the instant of the date. This is equivalent to using
   * {@code date1.toInstant().isAfter(date2.toInstant());}.
   *
   * @param other  the other date to compare to, not null
   * @return true if this is after the instant of the specified date
   */
  def isAfter(other: OffsetDate): Boolean = toEpochSeconds > other.toEpochSeconds

  /**
   * Checks if the instant of midnight at the start of this {@code OffsetDate}
   * is before midnight at the start of the specified date.
   * <p>
   * This method differs from the comparison in {@link #compareTo} in that it
   * only compares the instant of the date. This is equivalent to using
   * {@code date1.toInstant().isBefore(date2.toInstant());}.
   *
   * @param other  the other date to compare to, not null
   * @return true if this is before the instant of the specified date
   */
  def isBefore(other: OffsetDate): Boolean = toEpochSeconds < other.toEpochSeconds

  /**
   * Checks if the instant of midnight at the start of this {@code OffsetDate}
   * equals midnight at the start of the specified date.
   * <p>
   * This method differs from the comparison in {@link #compareTo} and {@link #equals}
   * in that it only compares the instant of the date. This is equivalent to using
   * {@code date1.toInstant().equals(date2.toInstant());}.
   *
   * @param other  the other date to compare to, not null
   * @return true if the instant equals the instant of the specified date
   */
  def equalInstant(other: OffsetDate): Boolean = toEpochSeconds == other.toEpochSeconds

  /**
   * Checks if this {@code OffsetDate} is equal to the specified date.
   * <p>
   * This method returns true if the state of the two objects are equal.
   * The state consists of the local date and the offset.
   * <p>
   * To compare for the same instant on the time-line, use {@link #equalInstant}.
   *
   * @param other  the other date to compare to, null returns false
   * @return true if this date is equal to the specified date
   */
  override def equals(other: Any): Boolean =
    other match {
      case offsetDate: OffsetDate => (this eq offsetDate) || ((date == offsetDate.date) && (offset == offsetDate.offset))
      case _ => false
    }

  /**
   * A hash code for this {@code OffsetDate}.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = date.hashCode ^ offset.hashCode

  /**
   * Outputs this date as a {@code String}, such as {@code 2007-12-03+01:00}.
   * <p>
   * The output will be in the format {@code yyyy-MM-ddZZZZ}.
   *
   * @return the formatted date, never null
   */
  override def toString: String = date.toString + offset.toString

  /**
   * Outputs this date as a {@code String} using the formatter.
   *
   * @param formatter  the formatter to use, not null
   * @return the formatted date string, never null
   * @throws UnsupportedOperationException if the formatter cannot print
   * @throws CalendricalPrintException if an error occurs during printing
   */
  def toString(formatter: DateTimeFormatter): String = {
    ISOChronology.checkNotNull(formatter, "DateTimeFormatter must not be null")
    formatter.print(this)
  }
}

