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
 * A date-time with a zone offset from UTC in the ISO-8601 calendar system,
 * such as '2007-12-03T10:15:30+01:00'.
 * <p>
 * OffsetDateTime is an immutable representation of a date-time with an offset.
 * This class stores all date and time fields, to a precision of nanoseconds,
 * as well as the offset from UTC. Thus, for example, the value
 * "2nd October 2007 at 13:45.30.123456789 +02:00" can be stored in an OffsetDateTime.
 * <p>
 * OffsetDateTime is immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object OffsetDateTime {
  /**
   * Obtains an instance of    { @code OffsetDateTime } from a text string.
   * <p>
   * The following formats are accepted in ASCII:
   * <ul>
   * <li>   { @code   { Year } -   { MonthOfYear } -   { DayOfMonth } T   { Hour } :   { Minute } { OffsetID } }
   * <li>   { @code   { Year } -   { MonthOfYear } -   { DayOfMonth } T   { Hour } :   { Minute } :   { Second } { OffsetID } }
   * <li>   { @code   { Year } -   { MonthOfYear } -   { DayOfMonth } T   { Hour } :   { Minute } :   { Second }.   { NanosecondFraction } { OffsetID } }
   * </ul>
   * <p>
   * The year has between 4 and 10 digits with values from MIN_YEAR to MAX_YEAR.
   * If there are more than 4 digits then the year must be prefixed with the plus symbol.
   * Negative years are allowed, but not negative zero.
   * <p>
   * The month-of-year has 2 digits with values from 1 to 12.
   * <p>
   * The day-of-month has 2 digits with values from 1 to 31 appropriate to the month.
   * <p>
   * The hour has 2 digits with values from 0 to 23.
   * The minute has 2 digits with values from 0 to 59.
   * The second has 2 digits with values from 0 to 59.
   * The nanosecond fraction has from 1 to 9 digits with values from 0 to 999,999,999.
   * <p>
   * The offset ID is the normalized form as defined in    { @link ZoneOffset }.
   *
   * @param text the text to parse such as '2007-12-03T10:15:30+01:00', not null
   * @return the parsed offset date-time, never null
   * @throws CalendricalException if the text cannot be parsed
   */
  def parse(text: String): OffsetDateTime = DateTimeFormatters.isoOffsetDateTime.parse(text, rule)

  /**
   * Obtains an instance of    { @code OffsetDateTime } from a date with the
   * time set to midnight at the start of day.
   * <p>
   * The time fields will be set to zero by this factory method.
   *
   * @param dateProvider the date provider to use, not null
   * @param offset the zone offset, not null
   * @return the offset date-time, never null
   */
  def ofMidnight(dateProvider: DateProvider, offset: ZoneOffset): OffsetDateTime = {
    val dt: LocalDateTime = LocalDateTime.ofMidnight(dateProvider)
    new OffsetDateTime(dt, offset)
  }

  /**
   * Obtains an instance of    { @code OffsetDateTime } from year, month,
   * day, hour and minute, setting the second and nanosecond to zero.
   * <p>
   * The second and nanosecond fields will be set to zero by this factory method.
   *
   * @param year the year to represent, from MIN_YEAR to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, not null
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @param offset the zone offset, not null
   * @return the offset date-time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def of(year: Int, monthOfYear: MonthOfYear, dayOfMonth: Int, hourOfDay: Int, minuteOfHour: Int, offset: ZoneOffset): OffsetDateTime = {
    val dt: LocalDateTime = LocalDateTime.of(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour)
    new OffsetDateTime(dt, offset)
  }

  /**
   * Rule implementation.
   */
  private[calendar] object Rule extends Rule

  @SerialVersionUID(1L)
  private[calendar] sealed class Rule
    extends CalendricalRule[OffsetDateTime](classOf[OffsetDateTime], ISOChronology, "OffsetDateTime", ISOChronology.periodNanos, null)
    with Serializable {

    protected override def derive(calendrical: Calendrical): OffsetDateTime = {
      val zdt: ZonedDateTime = calendrical.get(ZonedDateTime.rule)
      if (zdt != null) zdt.toOffsetDateTime else null
    }

    private def readResolve: AnyRef = Rule
  }

  /**
   * Obtains an instance of    { @code OffsetDateTime } from year, month,
   * day, hour, minute and second, setting the nanosecond to zero.
   * <p>
   * The nanosecond field will be set to zero by this factory method.
   *
   * @param year the year to represent, from MIN_YEAR to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, not null
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @param offset the zone offset, not null
   * @return the offset date-time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def of(year: Int, monthOfYear: MonthOfYear, dayOfMonth: Int, hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int, offset: ZoneOffset): OffsetDateTime = {
    val dt: LocalDateTime = LocalDateTime.of(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute)
    new OffsetDateTime(dt, offset)
  }

  /**
   * Gets the field rule for the date-time.
   *
   * @return the field rule for the date-time, never null
   */
  def rule: CalendricalRule[OffsetDateTime] = Rule

  /**
   * Obtains the current date-time from the system clock in the default time-zone.
   * <p>
   * This will query the system clock in the default time-zone to obtain the current time.
   * The offset will be set based on the time-zone in the system clock.
   * <p>
   * Using this method will prevent the ability to use an alternate clock for testing
   * because the clock is hard-coded.
   *
   * @return the current date-time using the system clock, never null
   */
  def nowSystemClock: OffsetDateTime = now(Clock.systemDefaultZone)

  /**
   * Obtains an instance of    { @code OffsetDateTime } from year, month and
   * day with the time set to midnight at the start of day.
   * <p>
   * The time fields will be set to zero by this factory method.
   *
   * @param year the year to represent, from MIN_YEAR to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, not null
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @param offset the zone offset, not null
   * @return the offset date-time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def ofMidnight(year: Int, monthOfYear: MonthOfYear, dayOfMonth: Int, offset: ZoneOffset): OffsetDateTime = {
    val dt: LocalDateTime = LocalDateTime.ofMidnight(year, monthOfYear, dayOfMonth)
    new OffsetDateTime(dt, offset)
  }

  /**
   * Obtains an instance of    { @code OffsetDateTime } from a    { @code DateProvider }
   * and    { @code TimeProvider }.
   *
   * @param dateProvider the date provider to use, not null
   * @param timeProvider the time provider to use, not null
   * @param offset the zone offset, not null
   * @return the offset date-time, never null
   */
  def of(dateProvider: DateProvider, timeProvider: TimeProvider, offset: ZoneOffset): OffsetDateTime = {
    val dt: LocalDateTime = LocalDateTime.of(dateProvider, timeProvider)
    new OffsetDateTime(dt, offset)
  }

  /**
   * Obtains an instance of    { @code OffsetDateTime } from year, month,
   * day, hour, minute and second, setting the nanosecond to zero.
   * <p>
   * The nanosecond field will be set to zero by this factory method.
   *
   * @param year the year to represent, from MIN_YEAR to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, from 1 (January) to 12 (December)
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @param offset the zone offset, not null
   * @return the offset date-time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def of(year: Int, monthOfYear: Int, dayOfMonth: Int, hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int, offset: ZoneOffset): OffsetDateTime = {
    val dt: LocalDateTime = LocalDateTime.of(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute)
    new OffsetDateTime(dt, offset)
  }

  /**
   * Obtains an instance of    { @code OffsetDateTime } from year, month,
   * day, hour and minute, setting the second and nanosecond to zero.
   * <p>
   * The second and nanosecond fields will be set to zero by this factory method.
   *
   * @param year the year to represent, from MIN_YEAR to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, from 1 (January) to 12 (December)
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @param offset the zone offset, not null
   * @return the offset date-time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def of(year: Int, monthOfYear: Int, dayOfMonth: Int, hourOfDay: Int, minuteOfHour: Int, offset: ZoneOffset): OffsetDateTime = {
    val dt: LocalDateTime = LocalDateTime.of(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour)
    new OffsetDateTime(dt, offset)
  }

  /**
   * Obtains the current date-time from the specified clock.
   * <p>
   * This will query the specified clock to obtain the current time.
   * The offset will be set based on the time-zone in the clock.
   * <p>
   * Using this method allows the use of an alternate clock for testing.
   * The alternate clock may be introduced using    { @link Clock dependency injection }.
   *
   * @param clock the clock to use, not null
   * @return the current date-time, never null
   */
  def now(clock: Clock): OffsetDateTime = {
    ISOChronology.checkNotNull(clock, "Clock must not be null")
    ofInstant(clock.instant, clock.getZone.getRules.getOffset(clock.instant))
  }

  /**
   * Obtains an instance of    { @code OffsetDateTime } from year, month,
   * day, hour, minute, second and nanosecond.
   *
   * @param year the year to represent, from MIN_YEAR to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, from 1 (January) to 12 (December)
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @param nanoOfSecond the nano-of-second to represent, from 0 to 999,999,999
   * @param offset the zone offset, not null
   * @return the offset date-time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def of(year: Int, monthOfYear: Int, dayOfMonth: Int, hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int, nanoOfSecond: Int, offset: ZoneOffset): OffsetDateTime = {
    val dt: LocalDateTime = LocalDateTime.of(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, nanoOfSecond)
    new OffsetDateTime(dt, offset)
  }

  /**
   * Obtains an instance of    { @code OffsetDateTime } from year, month and
   * day with the time set to midnight at the start of day.
   * <p>
   * The time fields will be set to zero by this factory method.
   *
   * @param year the year to represent, from MIN_YEAR to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, from 1 (January) to 12 (December)
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @param offset the zone offset, not null
   * @return the offset date-time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def ofMidnight(year: Int, monthOfYear: Int, dayOfMonth: Int, offset: ZoneOffset): OffsetDateTime = {
    val dt: LocalDateTime = LocalDateTime.ofMidnight(year, monthOfYear, dayOfMonth)
    new OffsetDateTime(dt, offset)
  }

  /**
   * Obtains an instance of    { @code OffsetDateTime } using seconds from the
   * epoch of 1970-01-01T00:00:00Z.
   * <p>
   * The nanosecond field is set to zero.
   *
   * @param epochSeconds the number of seconds from the epoch of 1970-01-01T00:00:00Z
   * @return the offset date-time, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def ofEpochSeconds(epochSeconds: Long, offset: ZoneOffset): OffsetDateTime = {
    ISOChronology.checkNotNull(offset, "ZoneOffset must not be null")
    val localSeconds: Long = epochSeconds + offset.getAmountSeconds
    val ldt: LocalDateTime = LocalDateTime.create(localSeconds, 0)
    new OffsetDateTime(ldt, offset)
  }

  /**
   * Obtains an instance of    { @code OffsetDateTime } from year, month,
   * day, hour, minute, second and nanosecond.
   *
   * @param year the year to represent, from MIN_YEAR to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, not null
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @param nanoOfSecond the nano-of-second to represent, from 0 to 999,999,999
   * @param offset the zone offset, not null
   * @return the offset date-time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def of(year: Int, monthOfYear: MonthOfYear, dayOfMonth: Int, hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int, nanoOfSecond: Int, offset: ZoneOffset): OffsetDateTime = {
    val dt: LocalDateTime = LocalDateTime.of(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, nanoOfSecond)
    new OffsetDateTime(dt, offset)
  }

  /**
   * Obtains an instance of    { @code OffsetDateTime } from a text string using a specific formatter.
   * <p>
   * The text is parsed using the formatter, returning a date-time.
   *
   * @param text the text to parse, not null
   * @param formatter the formatter to use, not null
   * @return the parsed offset date-time, never null
   * @throws UnsupportedOperationException if the formatter cannot parse
   * @throws CalendricalException if the text cannot be parsed
   */
  def parse(text: String, formatter: DateTimeFormatter): OffsetDateTime = {
    ISOChronology.checkNotNull(formatter, "DateTimeFormatter must not be null")
    formatter.parse(text, rule)
  }

  /**
   * Obtains an instance of    { @code OffsetDateTime } from a    { @code DateTimeProvider }.
   *
   * @param dateTimeProvider the date-time provider to use, not null
   * @param offset the zone offset, not null
   * @return the offset date-time, never null
   */
  def of(dateTimeProvider: DateTimeProvider, offset: ZoneOffset): OffsetDateTime = {
    val dt: LocalDateTime = LocalDateTime.of(dateTimeProvider)
    new OffsetDateTime(dt, offset)
  }

  /**
   * Obtains an instance of    { @code OffsetDateTime } from an    { @code InstantProvider }.
   *
   * @param instantProvider the instant to convert, not null
   * @param offset the zone offset, not null
   * @return the offset date-time, never null
   * @throws CalendarConversionException if the instant exceeds the supported date range
   */
  def ofInstant(instantProvider: InstantProvider, offset: ZoneOffset): OffsetDateTime = {
    val instant: Instant = Instant.of(instantProvider)
    ISOChronology.checkNotNull(offset, "ZoneOffset must not be null")
    val localSeconds: Long = instant.getEpochSeconds + offset.getAmountSeconds
    val ldt: LocalDateTime = LocalDateTime.create(localSeconds, instant.getNanoOfSecond)
    new OffsetDateTime(ldt, offset)
  }
}


/**
 * Constructor.
 *
 * @param dateTime the date-time, not null
 * @param offset the zone offset, not null
 */
@SerialVersionUID(-456761901L)
final class OffsetDateTime private(val dateTime: LocalDateTime, val offset: ZoneOffset)
  extends Calendrical with InstantProvider with DateTimeProvider with CalendricalMatcher
  with DateAdjuster with TimeAdjuster with Comparable[OffsetDateTime] with Serializable {
  if (dateTime == null) throw new NullPointerException("The date-time must not be null")
  if (offset == null) throw new NullPointerException("The zone offset must not be null")

  /**
   * Gets the nano-of-second field.
   *
   * @return the nano-of-second, from 0 to 999,999,999
   */
  def getNanoOfSecond: Int = dateTime.getNanoOfSecond

  /**
   * Returns a copy of this OffsetDateTime with the month-of-year altered.
   * If the resulting    { @code OffsetDateTime } is invalid, it will be resolved using    { @link DateResolvers # previousValid ( ) }.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This method does the same as    { @code withMonthOfYear ( monthOfYear, DateResolvers.previousValid ( ) ) }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear the month-of-year to set in the returned date, from 1 (January) to 12 (December)
   * @return an { @code OffsetDateTime } based on this date-time with the requested month, never null
   * @throws IllegalCalendarFieldValueException if the month-of-year value is invalid
   */
  def withMonthOfYear(monthOfYear: Int): OffsetDateTime = `with`(dateTime.withMonthOfYear(monthOfYear), offset)

  /**
   * Returns a copy of this OffsetDateTime with the month-of-year altered.
   * If the resulting    { @code OffsetDateTime } is invalid, it will be resolved using    { @link DateResolvers # previousValid ( ) }.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This method does the same as    { @code with ( monthOfYear, DateResolvers.previousValid ( ) ) }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear the month-of-year to set in the returned date, not null
   * @return an { @code OffsetDateTime } based on this date-time with the requested month, never null
   */
  def `with`(monthOfYear: MonthOfYear): OffsetDateTime = `with`(dateTime.`with`(monthOfYear), offset)

  /**
   * Compares this date-time to another date-time based on the instant
   * then local date-time.
   * <p>
   * This ordering is consistent with    { @code equals ( ) }.
   * For example, the following is the comparator order:
   * <ol>
   * <li>2008-12-03T10:30+01:00</li>
   * <li>2008-12-03T11:00+01:00</li>
   * <li>2008-12-03T12:00+02:00</li>
   * <li>2008-12-03T11:30+01:00</li>
   * <li>2008-12-03T12:00+01:00</li>
   * <li>2008-12-03T12:30+01:00</li>
   * </ol>
   * Values #2 and #3 represent the same instant on the time-line.
   * When two values represent the same instant, the local date-time is compared
   * to distinguish them. This step is needed to make the ordering
   * consistent with    { @code equals ( ) }.
   *
   * @param other the other date-time to compare to, not null
   * @return the comparator value, negative if less, positive if greater
   * @throws NullPointerException if    { @code other } is null
   */
  def compareTo(other: OffsetDateTime): Int = {
    if (offset.equals(other.offset)) return dateTime.compareTo(other.dateTime)
    else {
      var compare: Int = MathUtils.safeCompare(toEpochSeconds, other.toEpochSeconds)
      if (compare == 0) {
        compare = dateTime.compareTo(other.dateTime)
      }
      compare
    }
  }

  /**
   * Returns a copy of this OffsetDateTime with the specified period in nanoseconds subtracted.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param nanos the nanos to subtract, may be negative
   * @return an { @code OffsetDateTime } based on this date-time with the nanoseconds subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusNanos(nanos: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.minusNanos(nanos)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Gets the day-of-month field.
   * <p>
   * This method returns the primitive    { @code int } value for the day-of-month.
   *
   * @return the day-of-month, from 1 to 31
   */
  def getDayOfMonth: Int = dateTime.getDayOfMonth

  /**
   * Returns a copy of this OffsetDateTime with the hour-of-day value altered.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @return a new updated OffsetDateTime, never null
   * @throws IllegalCalendarFieldValueException if the hour value is invalid
   */
  def withHourOfDay(hourOfDay: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.withHourOfDay(hourOfDay)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Converts this date-time to an    { @code OffsetDate }.
   *
   * @return an OffsetDate representing the date and offset, never null
   */
  def toOffsetDate: OffsetDate = OffsetDate.of(dateTime, offset)

  /**
   * Gets the second-of-minute field.
   *
   * @return the second-of-minute, from 0 to 59
   */
  def getSecondOfMinute: Int = dateTime.getSecondOfMinute

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
  def getDayOfWeek: DayOfWeek = dateTime.getDayOfWeek

  /**
   * Returns a zoned date-time formed from the instant represented by this
   * date-time and the specified time-zone.
   * <p>
   * This conversion will ignore the visible local date-time and use the underlying instant instead.
   * This avoids any problems with local time-line gaps or overlaps.
   * The result might have different values for fields such as hour, minute an even day.
   * <p>
   * To attempt to retain the values of the fields, use    { @link # atZoneSimilarLocal ( TimeZone ) }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param zone the time-zone to use, not null
   * @return the zoned date-time formed from this date-time, never null
   */
  def atZoneSameInstant(zone: TimeZone): ZonedDateTime = ZonedDateTime.ofInstant(this, zone)

  /**
   * Returns a copy of this OffsetDateTime with a different local date-time.
   * <p>
   * This method changes the date-time stored to a different one.
   * No calculation is performed. The result simply represents the same
   * offset and the new date-time.
   *
   * @param dateTimeProvider the local date-time to change to, not null
   * @return a new updated OffsetDateTime, never null
   */
  def withDateTime(dateTimeProvider: DateTimeProvider): OffsetDateTime = {
    val localDateTime: LocalDateTime = LocalDateTime.of(dateTimeProvider)
    if (localDateTime.equals(this.dateTime)) this else new OffsetDateTime(localDateTime, offset)
  }

  /**
   * Returns a copy of this OffsetDateTime with the month-of-year altered.
   * If the resulting    { @code OffsetDateTime } is invalid, it will be resolved using    { @code dateResolver }.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear the month-of-year to set in the returned date, from 1 (January) to 12 (December)
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an { @code OffsetDateTime } based on this date-time with the requested month, never null
   * @throws IllegalCalendarFieldValueException if the month-of-year value is invalid
   */
  def withMonthOfYear(monthOfYear: Int, dateResolver: DateResolver): OffsetDateTime = {
    `with`(dateTime.withMonthOfYear(monthOfYear, dateResolver), offset)
  }

  /**
   * Gets the hour-of-day field.
   *
   * @return the hour-of-day, from 0 to 23
   */
  def getHourOfDay: Int = dateTime.getHourOfDay

  /**
   * Converts this date-time to an    { @code Instant }.
   *
   * @return an Instant representing the same instant, never null
   */
  override def toInstant: Instant = Instant.ofEpochSeconds(toEpochSeconds, getNanoOfSecond)

  /**
   * Returns a copy of this OffsetDateTime with the specified period in seconds added.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param seconds the seconds to add, may be negative
   * @return an { @code OffsetDateTime } based on this date-time with the seconds added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusSeconds(seconds: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.plusSeconds(seconds)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Checks whether this date-time matches the specified matcher.
   * <p>
   * Matchers can be used to query the date-time.
   * A simple matcher might simply compare one of the fields, such as the year field.
   * A more complex matcher might check if the date is the last day of the month.
   *
   * @param matcher the matcher to use, not null
   * @return true if this date-time matches the matcher, false otherwise
   */
  def matches(matcher: CalendricalMatcher): Boolean = matcher.matchesCalendrical(this)

  /**
   * Converts this date-time to a    { @code LocalDate }.
   *
   * @return a LocalDate representing the date fields of this date-time, never null
   */
  def toLocalDate: LocalDate = dateTime.toLocalDate

  /**
   * Returns a copy of this    { @code OffsetDateTime } with the specified period subtracted.
   * <p>
   * This subtracts the specified period from this date-time, returning a new date-time.
   * Before addition, the period is converted to a    { @code Period } using the
   * { @link Period # of ( PeriodProvider ) }.
   * <p>
   * The detailed rules for the subtraction have some complexity due to variable length months.
   * See    { @link LocalDateTime # minus ( PeriodProvider ) } for details.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to subtract, not null
   * @return an { @code OffsetDateTime } based on this date-time with the period subtracted, never null
   * @throws CalendricalException if the specified period cannot be converted to a    { @code Period }
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minus(periodProvider: PeriodProvider): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.minus(periodProvider)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Returns a copy of this OffsetDateTime with the year altered.
   * If the resulting    { @code OffsetDateTime } is invalid, it will be resolved using    { @link DateResolvers # previousValid ( ) }.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This method does the same as    { @code withYear ( year, DateResolvers.previousValid ( ) ) }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param year the year to set in the returned date, from MIN_YEAR to MAX_YEAR
   * @return an { @code OffsetDateTime } based on this date-time with the requested year, never null
   * @throws IllegalCalendarFieldValueException if the year value is invalid
   */
  def withYear(year: Int): OffsetDateTime = `with`(dateTime.withYear(year), offset)

  /**
   * Adjusts a time to have the value of the time part of this object.
   *
   * @param time the time to be adjusted, not null
   * @return the adjusted time, never null
   */
  override def adjustTime(time: LocalTime): LocalTime = dateTime.adjustTime(time)

  /**
   * Returns a copy of this    { @code OffsetDateTime } with the specified period added.
   * <p>
   * This adds the specified period to this date-time, returning a new date-time.
   * Before addition, the period is converted to a    { @code Period } using the
   * { @link Period # of ( PeriodProvider ) }.
   * <p>
   * The detailed rules for the addition have some complexity due to variable length months.
   * See    { @link LocalDateTime # plus ( PeriodProvider ) } for details.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to add, not null
   * @return an { @code OffsetDateTime } based on this date-time with the period added, never null
   * @throws CalendricalException if the specified period cannot be converted to a    { @code Period }
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plus(periodProvider: PeriodProvider): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.plus(periodProvider)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Converts this date-time to a    { @code LocalTime }.
   *
   * @return a LocalTime representing the time fields of this date-time, never null
   */
  def toLocalTime: LocalTime = dateTime.toLocalTime

  /**
   * Returns a copy of this OffsetDateTime with the time values altered.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @param nanoOfSecond the nano-of-second to represent, from 0 to 999,999,999
   * @return a new updated OffsetDateTime, never null
   * @throws IllegalCalendarFieldValueException if any field value is invalid
   */
  def withTime(hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int, nanoOfSecond: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.withTime(hourOfDay, minuteOfHour, secondOfMinute, nanoOfSecond)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Returns a copy of this OffsetDateTime with the specified period in months subtracted.
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
   * @return an { @code OffsetDateTime } based on this date-time with the months subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusMonths(months: Int, dateResolver: DateResolver): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.minusMonths(months)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Checks if the instant of this date-time is before that of the specified date-time.
   * <p>
   * This method differs from the comparison in    { @link # compareTo } in that it
   * compares only the instant of the date-time. This is equivalent to using
   * { @code dateTime1.toInstant ( ).isBefore ( dateTime2.toInstant ( ) ); }.
   *
   * @param other the other date-time to compare to, not null
   * @return true if this point is before the specified date-time
   * @throws NullPointerException if    { @code other } is null
   */
  def isBefore(other: OffsetDateTime): Boolean = {
    val thisEpochSecs: Long = toEpochSeconds
    val otherEpochSecs: Long = other.toEpochSeconds
    thisEpochSecs < otherEpochSecs || (thisEpochSecs == otherEpochSecs && getNanoOfSecond < other.getNanoOfSecond)
  }

  /**
   * A hash code for this date-time.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = dateTime.hashCode ^ offset.hashCode

  /**
   * Returns a new date-time based on this one, returning    { @code this } where possible.
   *
   * @param dateTime the date-time to create with, not null
   * @param offset the zone offset to create with, not null
   */
  private def `with`(dateTime: LocalDateTime, offset: ZoneOffset): OffsetDateTime = {
    if (this.dateTime == dateTime && this.offset.equals(offset)) this
    else new OffsetDateTime(dateTime, offset)
  }

  /**
   * Returns a copy of this OffsetDateTime with the nano-of-second value altered.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param nanoOfSecond the nano-of-second to represent, from 0 to 999,999,999
   * @return a new updated OffsetDateTime, never null
   * @throws IllegalCalendarFieldValueException if the nanos value is invalid
   */
  def withNanoOfSecond(nanoOfSecond: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.withNanoOfSecond(nanoOfSecond)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Returns a copy of this OffsetDateTime with the date values altered.
   * <p>
   * This method will return a new instance with the same time fields,
   * but altered date fields.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param year the year to represent, from MIN_VALUE + 1 to MAX_VALUE
   * @param monthOfYear the month-of-year to represent, from 1 (January) to 12 (December)
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @return an { @code OffsetDateTime } based on this date-time with the requested date, never null
   * @throws IllegalCalendarFieldValueException if any field value is invalid
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def withDate(year: Int, monthOfYear: Int, dayOfMonth: Int): OffsetDateTime = {
    `with`(dateTime.withDate(year, monthOfYear, dayOfMonth), offset)
  }

  /**
   * Returns a zoned date-time formed from this date-time and the specified time-zone.
   * <p>
   * Time-zone rules, such as daylight savings, mean that not every time on the
   * local time-line exists. As a result, this method can only convert the date-time
   * to the same time if the time-zone rules permit it. If not then a similar time is returned.
   * <p>
   * This method uses the    { @link ZoneResolvers # postTransition ( ) post transition } rules
   * to determine what to do when a gap or overlap occurs. These rules select the
   * date-time immediately after a gap and the later offset in overlaps.
   * <p>
   * Finer control over gaps and overlaps is available in two ways.
   * If you simply want to use the earlier offset at overlaps then call
   * { @link ZonedDateTime # withEarlierOffsetAtOverlap ( ) } immediately after this method.
   * Alternately, pass a specific resolver to    { @link # atZoneSimilarLocal ( TimeZone, ZoneResolver ) }.
   * <p>
   * To create a zoned date-time at the same instant irrespective of the local time-line,
   * use    { @link # atZoneSameInstant ( TimeZone ) }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param zone the time-zone to use, not null
   * @return the zoned date-time formed from this date and the earliest valid time for the zone, never null
   */
  def atZoneSimilarLocal(zone: TimeZone): ZonedDateTime = ZonedDateTime.of(this, zone, ZoneResolvers.postTransition)

  /**
   * Gets the value of the specified calendrical rule.
   * <p>
   * This method queries the value of the specified calendrical rule.
   * If the value cannot be returned for the rule from this date-time then
   * { @code null } will be returned.
   *
   * @param rule the rule to use, not null
   * @return the value for the rule, null if the value cannot be returned
   */
  def get[T](rule: CalendricalRule[T]): T = rule.deriveValueFor(rule, this, this)

  /**
   * Returns a copy of this OffsetDateTime with the day-of-year altered.
   * If the resulting    { @code OffsetDateTime } is invalid, an exception is thrown.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfYear the day-of-year to set in the returned date, from 1 to 365-366
   * @return an { @code OffsetDateTime } based on this date with the requested day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-year value is invalid
   * @throws InvalidCalendarFieldException if the day-of-year is invalid for the year
   */
  def withDayOfYear(dayOfYear: Int): OffsetDateTime = `with`(dateTime.withDayOfYear(dayOfYear), offset)

  /**
   * Returns a copy of this OffsetDateTime with the specified period in years subtracted.
   * <p>
   * This method subtract the specified amount to the years field in three steps:
   * <ol>
   * <li>Subtract the input years to the year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the day-of-month to the last valid day if necessary</li>
   * </ol>
   * <p>
   * For example, 2008-02-29 (leap year) minus one year would result in the
   * invalid date 2009-02-29 (standard year). Instead of returning an invalid
   * result, the last valid day of the month, 2009-02-28, is selected instead.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to subtract, may be negative
   * @return an { @code OffsetDateTime } based on this date-time with the years subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusYears(years: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.minusYears(years)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Returns a copy of this OffsetDateTime with the date values altered.
   * <p>
   * This method will return a new instance with the same time fields,
   * but altered date fields.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param year the year to represent, from MIN_YEAR to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, not null
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @return an { @code OffsetDateTime } based on this date-time with the requested date, never null
   * @throws IllegalCalendarFieldValueException if any field value is invalid
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def withDate(year: Int, monthOfYear: MonthOfYear, dayOfMonth: Int): OffsetDateTime = {
    `with`(dateTime.withDate(year, monthOfYear, dayOfMonth), offset)
  }

  /**
   * Returns a copy of this OffsetDateTime with the specified period in years added.
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
   * @return an { @code OffsetDateTime } based on this date-time with the years added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusYears(years: Int, dateResolver: DateResolver): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.plusYears(years, dateResolver)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Returns a copy of this OffsetDateTime with the time values altered.
   * <p>
   * This method will return a new instance with the same date fields,
   * but altered time fields.
   * This is a shorthand for    { @link # withTime ( int, int, int ) } and sets
   * the second field to zero.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @return a new updated OffsetDateTime, never null
   * @throws IllegalCalendarFieldValueException if any field value is invalid
   */
  def withTime(hourOfDay: Int, minuteOfHour: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.withTime(hourOfDay, minuteOfHour)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
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
  def getMonthOfYear: MonthOfYear = dateTime.getMonthOfYear

  /**
   * Returns a copy of this OffsetDateTime with the specified period in months subtracted.
   * <p>
   * This method subtract the specified amount to the months field in three steps:
   * <ol>
   * <li>Subtract the input months to the month-of-year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the day-of-month to the last valid day if necessary</li>
   * </ol>
   * <p>
   * For example, 2007-03-31 minus one month would result in the invalid date
   * 2007-04-31. Instead of returning an invalid result, the last valid day
   * of the month, 2007-04-30, is selected instead.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to subtract, may be negative
   * @return an { @code OffsetDateTime } based on this date-time with the months subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusMonths(months: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.minusMonths(months)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Outputs this date-time as a    { @code String }, such as    { @code 2007 -12-03T10:15:30+01:00 }.
   * <p>
   * The output will be one of the following formats:
   * <ul>
   * <li>   { @code yyyy -MM-dd'T'HH:mmZZZZ } </li>
   * <li>   { @code yyyy -MM-dd'T'HH:mm:ssZZZZ } </li>
   * <li>   { @code yyyy -MM-dd'T'HH:mm:ssfnnnZZZZ } </li>
   * <li>   { @code yyyy -MM-dd'T'HH:mm:ssfnnnnnnZZZZ } </li>
   * <li>   { @code yyyy -MM-dd'T'HH:mm:ssfnnnnnnnnnZZZZ } </li>
   * </ul>
   * The format used will be the shortest that outputs the full value of
   * the time where the omitted parts are implied to be zero.
   *
   * @return the formatted date-time, never null
   */
  override def toString: String = dateTime.toString + offset.toString

  /**
   * Returns a copy of this OffsetDateTime with the specified period in years added.
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
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to add, may be negative
   * @return an { @code OffsetDateTime } based on this date-time with the years added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusYears(years: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.plusYears(years)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Returns a copy of this OffsetDateTime with the specified period in hours added.
   * <p>
   * This method uses field based addition.
   * This method changes the field by the specified number of hours.
   * This may, at daylight savings cutover, result in a duration being added
   * that is more or less than the specified number of hours.
   * <p>
   * For example, consider a zone offset where the spring DST cutover means that
   * the local times 01:00 to 01:59 do not exist. Using this method, adding
   * a duration of 2 hours to 00:30 will result in 02:30, but it is important
   * to note that only a duration of 1 hour was actually added.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hours the hours to add, may be negative
   * @return an { @code OffsetDateTime } based on this date-time with the hours added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusHours(hours: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.plusHours(hours)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Gets the day-of-year field.
   * <p>
   * This method returns the primitive    { @code int } value for the day-of-year.
   *
   * @return the day-of-year, from 1 to 365, or 366 in a leap year
   */
  def getDayOfYear: Int = dateTime.getDayOfYear

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
  def getYear: Int = dateTime.getYear

  /**
   * Converts this date-time to a    { @code LocalDateTime }.
   *
   * @return a LocalDateTime representing the fields of this date-time, never null
   */
  override def toLocalDateTime: LocalDateTime = dateTime

  /**
   * Returns a copy of this OffsetDateTime with the specified period in years subtracted.
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
   * @return an { @code OffsetDateTime } based on this date-time with the years subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusYears(years: Int, dateResolver: DateResolver): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.minusYears(years, dateResolver)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Outputs this date-time as a    { @code String } using the formatter.
   *
   * @param formatter the formatter to use, not null
   * @return the formatted date-time string, never null
   * @throws UnsupportedOperationException if the formatter cannot print
   * @throws CalendricalPrintException if an error occurs during printing
   */
  def toString(formatter: DateTimeFormatter): String = {
    ISOChronology.checkNotNull(formatter, "DateTimeFormatter must not be null")
    formatter.print(this)
  }

  /**
   * Checks if the instant of this date-time is after that of the specified date-time.
   * <p>
   * This method differs from the comparison in    { @link # compareTo } in that it
   * compares the only the instant of the date-time. This is equivalent to using
   * { @code dateTime1.toInstant ( ).isAfter ( dateTime2.toInstant ( ) ); }.
   *
   * @param other the other date-time to compare to, not null
   * @return true if this is after the specified date-time
   * @throws NullPointerException if    { @code other } is null
   */
  def isAfter(other: OffsetDateTime): Boolean = {
    val thisEpochSecs: Long = toEpochSeconds
    val otherEpochSecs: Long = other.toEpochSeconds
    thisEpochSecs > otherEpochSecs || (thisEpochSecs == otherEpochSecs && getNanoOfSecond > other.getNanoOfSecond)
  }

  /**
   * Returns a copy of this OffsetDateTime with a different zone offset
   * using the same local date-time.
   * <p>
   * This method returns an OffsetDateTime that is the combination of the
   * local date-time from this instance and the specified offset.
   * No calculation is performed.
   * <p>
   * For example, if this time represents 10:30+02:00 and the offset
   * specified is +03:00, then this method will return 10:30+03:00.
   * <p>
   * To maintain the same instant on the time-line while changing offsets
   * use    { @link # withOffsetSameInstant }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param offset the zone offset to change to, not null
   * @return a new updated OffsetDateTime, never null
   */
  def withOffsetSameLocal(offset: ZoneOffset): OffsetDateTime = `with`(dateTime, offset)

  /**
   * Returns a copy of this OffsetDateTime with the day-of-month altered.
   * If the resulting    { @code OffsetDateTime } is invalid, an exception is thrown.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfMonth the day-of-month to set in the returned date, from 1 to 28-31
   * @return an { @code OffsetDateTime } based on this date-time with the requested day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-month value is invalid
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def withDayOfMonth(dayOfMonth: Int): OffsetDateTime = {
    `with`(dateTime.withDayOfMonth(dayOfMonth), offset)
  }

  /**
   * Returns a copy of this OffsetDateTime with the day-of-month altered.
   * If the resulting    { @code OffsetDateTime } is invalid, it will be resolved using    { @code dateResolver }.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfMonth the day-of-month to set in the returned date, from 1 to 31
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an { @code OffsetDateTime } based on this date-time with the requested day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-month value is invalid
   */
  def withDayOfMonth(dayOfMonth: Int, dateResolver: DateResolver): OffsetDateTime = {
    `with`(dateTime.withDayOfMonth(dayOfMonth, dateResolver), offset)
  }

  /**
   * Gets the minute-of-hour field.
   *
   * @return the minute-of-hour, from 0 to 59
   */
  def getMinuteOfHour: Int = dateTime.getMinuteOfHour

  /**
   * Returns a copy of this OffsetDateTime with the specified period in weeks added.
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
   * @return an { @code OffsetDateTime } based on this date-time with the weeks added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusWeeks(weeks: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.plusWeeks(weeks)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Returns a copy of this OffsetDateTime with the specified period in months added.
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
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to add, may be negative
   * @return an { @code OffsetDateTime } based on this date-time with the months added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusMonths(months: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.plusMonths(months)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Returns a copy of this OffsetDateTime with the specified period in minutes added.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param minutes the minutes to add, may be negative
   * @return an { @code OffsetDateTime } based on this date-time with the minutes added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusMinutes(minutes: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.plusMinutes(minutes)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Gets the zone offset.
   *
   * @return the zone offset, never null
   */
  def getOffset: ZoneOffset = offset

  /**
   * Returns a copy of this OffsetDateTime with the specified period in days subtracted.
   * <p>
   * This method subtract the specified amount to the days field incrementing the
   * month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2008-12-31 minus one day would result in the 2009-01-01.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param days the days to subtract, may be negative
   * @return an { @code OffsetDateTime } based on this date-time with the days subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusDays(days: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.minusDays(days)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Returns a copy of this OffsetDateTime with the time altered using the adjuster.
   * <p>
   * Adjusters can be used to alter the time in various ways.
   * A simple adjuster might simply set the one of the fields, such as the hour field.
   * A more complex adjuster might set the time to end of the working day.
   * <p>
   * The offset and date do not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param adjuster the adjuster to use, not null
   * @return an { @code OffsetDateTime } based on this date-time with the time adjusted, never null
   * @throws IllegalArgumentException if the adjuster returned null
   */
  def `with`(adjuster: TimeAdjuster): OffsetDateTime = `with`(dateTime.`with`(adjuster), offset)

  /**
   * Checks if the date-time extracted from the calendrical matches this.
   *
   * @param calendrical the calendrical to match, not null
   * @return true if the calendrical matches, false otherwise
   */
  override def matchesCalendrical(calendrical: Calendrical): Boolean = this.equals(calendrical.get(rule))

  /**
   * Returns a copy of this OffsetDateTime with the time values altered.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @return a new updated OffsetDateTime, never null
   * @throws IllegalCalendarFieldValueException if any field value is invalid
   */
  def withTime(hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.withTime(hourOfDay, minuteOfHour, secondOfMinute)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Gets the chronology that this date-time uses, which is the ISO calendar system.
   *
   * @return the ISO chronology, never null
   */
  def getChronology: ISOChronology = ISOChronology

  /**
   * Returns a copy of this OffsetDateTime with the specified period in minutes subtracted.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param minutes the minutes to subtract, may be negative
   * @return an { @code OffsetDateTime } based on this date-time with the minutes subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusMinutes(minutes: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.minusMinutes(minutes)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Checks if the instant of this date-time is equal to that of the specified date-time.
   * <p>
   * This method differs from the comparison in    { @link # compareTo } and    { @link # equals }
   * in that it compares only the instant of the date-time. This is equivalent to using
   * { @code dateTime1.toInstant ( ).equals ( dateTime2.toInstant ( ) ); }.
   *
   * @param other the other date-time to compare to, not null
   * @return true if this is after the specified date-time
   * @throws NullPointerException if    { @code other } is null
   */
  def equalInstant(other: OffsetDateTime): Boolean = {
    toEpochSeconds == other.toEpochSeconds && getNanoOfSecond == other.getNanoOfSecond
  }

  /**
   * Returns a copy of this OffsetDateTime with the specified period in days added.
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
   * @return an { @code OffsetDateTime } based on this date-time with the days added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusDays(days: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.plusDays(days)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Returns a copy of this OffsetDateTime with the second-of-minute value altered.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @return a new updated OffsetDateTime, never null
   * @throws IllegalCalendarFieldValueException if the second value is invalid
   */
  def withSecondOfMinute(secondOfMinute: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.withSecondOfMinute(secondOfMinute)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Returns a zoned date-time formed from this date-time and the specified time-zone
   * taking control of what occurs in time-line gaps and overlaps.
   * <p>
   * Time-zone rules, such as daylight savings, mean that not every time on the
   * local time-line exists. As a result, this method can only convert the date-time
   * to the same time if the time-zone rules permit it. If not then a similar time is returned.
   * <p>
   * This method uses the specified resolver to determine what to do when a gap or overlap occurs.
   * <p>
   * To create a zoned date-time at the same instant irrespective of the local time-line,
   * use    { @link # atZoneSameInstant ( TimeZone ) }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param zone the time-zone to use, not null
   * @param resolver the zone resolver to use for gaps and overlaps, not null
   * @return the zoned date-time formed from this date and the earliest valid time for the zone, never null
   * @throws CalendricalException if the date-time cannot be resolved
   */
  def atZoneSimilarLocal(zone: TimeZone, resolver: ZoneResolver): ZonedDateTime = ZonedDateTime.of(this, zone, resolver)

  /**
   * Gets the year field as a    { @code Year }.
   * <p>
   * This method provides access to an object representing the year field.
   * { @code Year } has methods for querying addition year-based information.
   *
   * @return the year, never null
   */
  def toYear: Year = dateTime.toYear

  /**
   * Converts this date-time to an    { @code OffsetTime }.
   *
   * @return an OffsetTime representing the time and offset, never null
   */
  def toOffsetTime: OffsetTime = OffsetTime.of(dateTime, offset)

  /**
   * Returns a copy of this OffsetDateTime with the specified period in weeks subtracted.
   * <p>
   * This method subtract the specified amount in weeks to the days field incrementing
   * the month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2008-12-31 minus one week would result in the 2009-01-07.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param weeks the weeks to subtract, may be negative
   * @return an { @code OffsetDateTime } based on this date-time with the weeks subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusWeeks(weeks: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.minusWeeks(weeks)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Adjusts a date to have the value of the date part of this object.
   *
   * @param date the date to be adjusted, not null
   * @return the adjusted date, never null
   */
  override def adjustDate(date: LocalDate): LocalDate = dateTime.adjustDate(date)

  /**
   * Returns a copy of this OffsetDateTime with the specified period in hours subtracted.
   * <p>
   * This method uses field based subtraction.
   * This method changes the field by the specified number of hours.
   * This may, at daylight savings cutover, result in a duration being subtracted
   * that is more or less than the specified number of hours.
   * <p>
   * For example, consider a zone offset where the spring DST cutover means that
   * the local times 01:00 to 01:59 do not exist. Using this method, subtracting
   * a duration of 2 hours to 00:30 will result in 02:30, but it is important
   * to note that only a duration of 1 hour was actually subtracted.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hours the hours to subtract, may be negative
   * @return an { @code OffsetDateTime } based on this date-time with the hours subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusHours(hours: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.minusHours(hours)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Returns a copy of this OffsetDateTime with the year altered.
   * If the resulting    { @code OffsetDateTime } is invalid, it will be resolved using    { @code dateResolver }.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param year the year to set in the returned date, from MIN_YEAR to MAX_YEAR
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an { @code OffsetDateTime } based on this date-time with the requested year, never null
   * @throws IllegalCalendarFieldValueException if the year value is invalid
   */
  def withYear(year: Int, dateResolver: DateResolver): OffsetDateTime = {
    `with`(dateTime.withYear(year, dateResolver), offset)
  }

  /**
   * Returns a copy of this OffsetDateTime with a different zone offset
   * adjusting the local date-time to retain the same instant.
   * <p>
   * This method returns an OffsetDateTime with the specified offset.
   * The local date-time in the result is adjusted such that the instant
   * represented by this instance and the instant represented by the
   * result are equal.
   * <p>
   * For example, if this time represents 10:30+02:00 and the offset
   * specified is +03:00, then this method will return 11:30+03:00.
   * <p>
   * This method is useful for finding the current local time in a different offset.
   * <p>
   * To change the offset while keeping the local time
   * use    { @link # withOffsetSameLocal }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param offset the zone offset to change to, not null
   * @return a new updated OffsetDateTime, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def withOffsetSameInstant(offset: ZoneOffset): OffsetDateTime = {
    if (offset.equals(this.offset)) this
    else {
      val difference: Int = offset.getAmountSeconds - this.offset.getAmountSeconds
      val adjusted: LocalDateTime = dateTime.plusSeconds(difference)
      new OffsetDateTime(adjusted, offset)
    }
  }

  /**
   * Returns a copy of this OffsetDateTime with the specified period in months added.
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
   * @return an { @code OffsetDateTime } based on this date-time with the months added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusMonths(months: Int, dateResolver: DateResolver): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.plusMonths(months)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Converts this date-time to the number of seconds from the epoch
   * of 1970-01-01T00:00:00Z.
   * <p>
   * Instants on the time-line after the epoch are positive, earlier are negative.
   *
   * @return the number of seconds from the epoch of 1970-01-01T00:00:00Z
   */
  def toEpochSeconds: Long = {
    val epochDays: Long = dateTime.toLocalDate.toEpochDays
    var secs: Long = epochDays * 60L * 60L * 24L + dateTime.toLocalTime.toSecondOfDay
    secs -= offset.getAmountSeconds
    secs
  }

  /**
   * Checks if the state of this date-time equal to that of the specified date-time.
   * <p>
   * This method returns true if the state of the two objects are equal.
   * The state consists of the local date-time and the offset.
   * <p>
   * To compare for the same instant on the time-line, use    { @link # equalInstant }.
   *
   * @param other the other date-time to compare to, null returns false
   * @return true if this point is equal to the specified date-time
   */
  override def equals(other: AnyRef): Boolean = {
    if (this == other) true
    else if (other.isInstanceOf[OffsetDateTime]) {
      val offsetDateTime: OffsetDateTime = other.asInstanceOf[OffsetDateTime]
      dateTime.equals(offsetDateTime.dateTime) && offset.equals(offsetDateTime.offset)
    }
    else false
  }

  /**
   * Returns a copy of this OffsetDateTime with the month-of-year altered.
   * If the resulting    { @code OffsetDateTime } is invalid, it will be resolved using    { @code dateResolver }.
   * The offset does not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear the month-of-year to set in the returned date, not null
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return an { @code OffsetDateTime } based on this date-time with the requested month, never null
   */
  def `with`(monthOfYear: MonthOfYear, dateResolver: DateResolver): OffsetDateTime = {
    `with`(dateTime.`with`(monthOfYear, dateResolver), offset)
  }

  /**
   * Returns a copy of this OffsetDateTime with the specified period in nanoseconds added.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param nanos the nanos to add, may be negative
   * @return an { @code OffsetDateTime } based on this date-time with the nanoseconds added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusNanos(nanos: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.plusNanos(nanos)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Returns a copy of this OffsetDateTime with the date altered using the adjuster.
   * <p>
   * Adjusters can be used to alter the date in various ways.
   * A simple adjuster might simply set the one of the fields, such as the year field.
   * A more complex adjuster might set the date to the last day of the month.
   * <p>
   * The offset and time do not affect the calculation and will be the same in the result.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param adjuster the adjuster to use, not null
   * @return an { @code OffsetDateTime } based on this date-time with the date adjusted, never null
   * @throws NullPointerException if the adjuster returned null
   */
  def `with`(adjuster: DateAdjuster): OffsetDateTime = {
    `with`(dateTime.`with`(adjuster), offset)
  }

  /**
   * Returns a copy of this OffsetDateTime with the minute-of-hour value altered.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @return a new updated OffsetDateTime, never null
   * @throws IllegalCalendarFieldValueException if the minute value is invalid
   */
  def withMinuteOfHour(minuteOfHour: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.withMinuteOfHour(minuteOfHour)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }

  /**
   * Returns a copy of this OffsetDateTime with the specified period in seconds subtracted.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param seconds the seconds to subtract, may be negative
   * @return an { @code OffsetDateTime } based on this date-time with the seconds subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusSeconds(seconds: Int): OffsetDateTime = {
    val newDT: LocalDateTime = dateTime.minusSeconds(seconds)
    (if (newDT == dateTime) this else new OffsetDateTime(newDT, offset))
  }
}