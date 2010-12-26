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

import javax.time.Duration
import javax.time.Instant
import javax.time.InstantProvider
import javax.time.calendar.format.DateTimeFormatter
import javax.time.calendar.format.DateTimeFormatters
import javax.time.calendar.zone.ZoneOffsetInfo
import javax.time.calendar.zone.ZoneRules

/**
 * A date-time with a time-zone in the ISO-8601 calendar system,
 * such as '2007-12-03T10:15:30+01:00 Europe/Paris'.
 * <p>
 * ZonedDateTime is an immutable representation of a date-time with a time-zone.
 * This class stores all date and time fields, to a precision of nanoseconds,
 * as well as a time-zone and zone offset. Thus, for example, the value
 * "2nd October 2007 at 13:45.30.123456789 +02:00 in the Europe/Paris time-zone"
 * can be stored in a ZonedDateTime.
 * <p>
 * The purpose of storing the time-zone is to distinguish the ambiguous case where
 * the local time-line overlaps, typically as a result of the end of daylight time.
 * Information about the local-time can be obtained using methods on the time-zone.
 * <p>
 * This class provides control over what happens at these cutover points
 * (typically a gap in spring and an overlap in autumn). The {@link ZoneResolver }
 * interface and implementations in {@link ZoneResolvers} provide strategies for
 * handling these cases. The methods {@link #withEarlierOffsetAtOverlap()} and
 * {@link #withLaterOffsetAtOverlap()} provide further control for overlaps.
 * <p>
 * ZonedDateTime is immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object ZonedDateTime {
  /**
   * Obtains an instance of {@code ZonedDateTime}.
   *
   * @param dateTime the date-time, not null
   * @param oldDateTime the old date-time prior to the calculation, may be null
   * @param zone the time-zone, not null
   * @param resolver the resolver from local date-time to zoned, not null
   * @return the zoned date-time, never null
   * @throws CalendricalException if the date-time cannot be resolved
   */
  private def resolve(dateTime: LocalDateTime, oldDateTime: ZonedDateTime, zone: TimeZone, resolver: ZoneResolver): ZonedDateTime = {
    ISOChronology.checkNotNull(dateTime, "LocalDateTime must not be null")
    ISOChronology.checkNotNull(zone, "TimeZone must not be null")
    ISOChronology.checkNotNull(resolver, "ZoneResolver must not be null")
    val offsetDT: OffsetDateTime = resolver.resolve(zone, dateTime, oldDateTime)
    new ZonedDateTime(offsetDT, zone)
  }

  /**
   * Gets the rule for {@code ZonedDateTime}.
   *
   * @return the rule for the date-time, never null
   */
  def rule: CalendricalRule[ZonedDateTime] = Rule

  /**
   * Obtains the current date-time from the specified clock.
   * <p>
   * This will query the specified clock to obtain the current time.
   * The zone and offset will be set based on the time-zone in the clock.
   * <p>
   * Using this method allows the use of an alternate clock for testing.
   * The alternate clock may be introduced using {@link Clock dependency injection}.
   *
   * @param clock the clock to use, not null
   * @return the current date-time, never null
   */
  def now(clock: Clock): ZonedDateTime = {
    ISOChronology.checkNotNull(clock, "Clock must not be null")
    ofInstant(clock.instant, clock.getZone)
  }

  /**
   * Obtains an instance of {@code ZonedDateTime} from year, month,
   * day, hour, minute, second, nanosecond and time-zone
   * providing a resolver to handle an invalid date-time.
   * <p>
   * The day must be valid for the year and month or an exception will be thrown.
   * <p>
   * The local date-time must be valid for the time-zone.
   * If the time is invalid for the zone, due to either being a gap or an overlap,
   * then the resolver will determine what action to take.
   * See {@link ZoneResolvers} for common resolver implementations.
   *
   * @param year the year to represent, from MIN_YEAR to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, from 1 (January) to 12 (December)
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @param nanoOfSecond the nano-of-second to represent, from 0 to 999,999,999
   * @param zone the time-zone, not null
   * @param resolver the resolver from local date-time to zoned, not null
   * @return the zoned date-time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   * @throws CalendricalException if the resolver cannot resolve an invalid local date-time
   */
  def of(year: Int, monthOfYear: Int, dayOfMonth: Int,
         hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int, nanoOfSecond: Int,
         zone: TimeZone, resolver: ZoneResolver = ZoneResolvers.strict): ZonedDateTime = {
    val dt: LocalDateTime = LocalDateTime.of(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, nanoOfSecond)
    resolve(dt, null, zone, resolver)
  }

  /**
   * Obtains an instance of {@code ZonedDateTime} from an {@code Instant}.
   * <p>
   * This factory creates a {@code ZonedDateTime} from an instant and time-zone.
   * If the instant represents a point on the time-line outside the supported year
   * range then an exception will be thrown.
   * <p>
   * If the time-zone has a floating version, then this conversion will use the latest time-zone rules.
   *
   * @param instantProvider the instant to convert, not null
   * @param zone the time-zone, not null
   * @return the zoned date-time, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def ofInstant(instantProvider: InstantProvider, zone: TimeZone): ZonedDateTime = {
    val instant: Instant = Instant.of(instantProvider)
    ISOChronology.checkNotNull(zone, "TimeZone must not be null")
    val rules: ZoneRules = zone.getRules
    val offsetDT: OffsetDateTime = OffsetDateTime.ofInstant(instant, rules.getOffset(instant))
    return new ZonedDateTime(offsetDT, zone)
  }

  /**
   * Obtains an instance of {@code ZonedDateTime} from a text string.
   * <p>
   * The following formats are accepted in ASCII:
   * <ul>
   * <li> {@code  { Year} -  { MonthOfYear} -  { DayOfMonth} T  { Hour} :  { Minute} { OffsetID }[  { ZoneId} ] }
   * <li> {@code  { Year} -  { MonthOfYear} -  { DayOfMonth} T  { Hour} :  { Minute} :  { Second} { OffsetID }[  { ZoneId} ] }
   * <li> {@code  { Year} -  { MonthOfYear} -  { DayOfMonth} T  { Hour} :  { Minute} :  { Second}.  { NanosecondFraction} { OffsetID }[  { ZoneId} ] }
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
   * The offset ID is the normalized form as defined in {@link ZoneOffset}.
   * <p>
   * The zone ID is the normalized form as defined in {@link TimeZone#getID()}.
   *
   * @param text the text to parse such as '2007-12-03T10:15:30+01:00[Europe/Paris]', not null
   * @return the parsed zoned date-time, never null
   * @throws CalendricalException if the text cannot be parsed
   */
  def parse(text: String): ZonedDateTime = DateTimeFormatters.isoZonedDateTime.parse(text, rule)

  /**
   * Obtains an instance of {@code ZonedDateTime} from an {@code OffsetDateTime }
   * ensuring that the offset provided is valid for the time-zone.
   * <p>
   * This factory creates a {@code ZonedDateTime} from an offset date-time and time-zone.
   * If the date-time is invalid for the zone due to a time-line gap then an exception is thrown.
   * Otherwise, the offset is checked against the zone to ensure it is valid.
   * <p>
   * If the time-zone has a floating version, then this conversion will use the
   * latest time-zone rules that are valid for the input date-time.
   * <p>
   * An alternative to this method is {@link #ofInstant ( OffsetDateTime, TimeZone )}.
   * This method will retain the date and time and throw an exception if
   * the offset is invalid. The {@code fromInstant} method will change the
   * date and time if necessary to retain the same instant.
   *
   * @param dateTime the offset date-time to use, not null
   * @param zone the time-zone, not null
   * @return the zoned date-time, never null
   * @throws CalendricalException if no rules can be found for the zone
   * @throws CalendricalException if the date-time is invalid due to a gap in the local time-line
   * @throws CalendricalException if the offset is invalid for the time-zone at the date-time
   */
  def of(dateTime: OffsetDateTime, zone: TimeZone): ZonedDateTime = {
    ISOChronology.checkNotNull(dateTime, "OffsetDateTime must not be null")
    ISOChronology.checkNotNull(zone, "TimeZone must not be null")
    val inputOffset: ZoneOffset = dateTime.getOffset
    val rules: ZoneRules = zone.getRules
    val info: ZoneOffsetInfo = rules.getOffsetInfo(dateTime.toLocalDateTime)
    if (info.isValidOffset(inputOffset) == false) {
      if (info.isTransition && info.getTransition.isGap) {
        throw new CalendarConversionException("The local time " + dateTime.toLocalDateTime + " does not exist in time-zone " + zone + " due to a daylight savings gap")
      }
      throw new CalendarConversionException("The offset in the date-time " + dateTime + " is invalid for time-zone " + zone)
    }
    return new ZonedDateTime(dateTime, zone)
  }

  /**
   * Obtains an instance of {@code ZonedDateTime} from year, month,
   * day, hour, minute, second, nanosecond and time-zone
   * providing a resolver to handle an invalid date-time.
   * <p>
   * The day must be valid for the year and month or an exception will be thrown.
   * <p>
   * The local date-time must be valid for the time-zone.
   * If the time is invalid for the zone, due to either being a gap or an overlap,
   * then the resolver will determine what action to take.
   * See {@link ZoneResolvers} for common resolver implementations.
   *
   * @param year the year to represent, from MIN_YEAR to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, not null
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @param nanoOfSecond the nano-of-second to represent, from 0 to 999,999,999
   * @param zone the time-zone, not null
   * @param resolver the resolver from local date-time to zoned, not null
   * @return the zoned date-time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   * @throws CalendricalException if the resolver cannot resolve an invalid local date-time
   */
  def of(year: Int, monthOfYear: MonthOfYear, dayOfMonth: Int,
         hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int, nanoOfSecond: Int,
         zone: TimeZone, resolver: ZoneResolver = ZoneResolvers.strict): ZonedDateTime = {
    val dt: LocalDateTime = LocalDateTime.of(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, nanoOfSecond)
    resolve(dt, null, zone, resolver)
  }

  /**
   * Obtains an instance of {@code ZonedDateTime} from a local date-time
   * providing a resolver to handle an invalid date-time.
   * <p>
   * This factory creates a {@code ZonedDateTime} from a date-time and time-zone.
   * If the time is invalid for the zone, due to either being a gap or an overlap,
   * then the resolver will determine what action to take.
   * See {@link ZoneResolvers} for common resolver implementations.
   *
   * @param dateTimeProvider the date-time provider to use, not null
   * @param zone the time-zone, not null
   * @param resolver the resolver from local date-time to zoned, not null
   * @return the zoned date-time, never null
   * @throws CalendricalException if the resolver cannot resolve an invalid local date-time
   */
  def of(dateTimeProvider: DateTimeProvider, zone: TimeZone, resolver: ZoneResolver = ZoneResolvers.strict): ZonedDateTime = {
    val dt: LocalDateTime = LocalDateTime.of(dateTimeProvider)
    resolve(dt, null, zone, resolver)
  }

  /**
   * Obtains an instance of {@code ZonedDateTime} using seconds from the
   * epoch of 1970-01-01T00:00:00Z.
   * <p>
   * The nanosecond field is set to zero.
   *
   * @param epochSeconds the number of seconds from the epoch of 1970-01-01T00:00:00Z
   * @param zone the time-zone, not null
   * @return the zoned date-time, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def ofEpochSeconds(epochSeconds: Long, zone: TimeZone): ZonedDateTime = {
    ISOChronology.checkNotNull(zone, "TimeZone must not be null")
    ZonedDateTime.ofInstant(Instant.ofEpochSeconds(epochSeconds, 0), zone)
  }

  /**
   * Obtains an instance of {@code ZonedDateTime} from the instant of an {@code OffsetDateTime}.
   * <p>
   * This factory creates a {@code ZonedDateTime} from an offset date-time and time-zone.
   * This is an optimized implementation of:
   * <pre>
   * ZonedDateTime.ofInstant(offsetDateTime.toInstant(), zone);
   * </pre>
   * If the offset date-time is in the wrong offset for the zone at the gap, then the
   * date, time and offset will be adjusted to ensure that the result has the same instant.
   * <p>
   * If the time-zone has a floating version, then this conversion will use the latest time-zone rules.
   * <p>
   * An alternative to this method is {@link #of ( OffsetDateTime, TimeZone )}.
   * The {@code fromInstant} method will change the date and time if necessary to
   * retain the same instant. The {@code dateTime} method will retain the date and
   * time and throw an exception if the offset is invalid.
   *
   * @param dateTime the offset date-time to use, not null
   * @param zone the time-zone, not null
   * @return the zoned date-time, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def ofInstant(_dateTime: OffsetDateTime, zone: TimeZone): ZonedDateTime = {
    var dateTime = _dateTime
    ISOChronology.checkNotNull(dateTime, "OffsetDateTime must not be null")
    ISOChronology.checkNotNull(zone, "TimeZone must not be null")
    val rules: ZoneRules = zone.getRules
    if (rules.isValidDateTime(dateTime) == false) {
      val offsetForInstant: ZoneOffset = rules.getOffset(dateTime)
      dateTime = dateTime.withOffsetSameInstant(offsetForInstant)
    }
    return new ZonedDateTime(dateTime, zone)
  }

  /**
   * Obtains an instance of {@code ZonedDateTime} from a text string using a specific formatter.
   * <p>
   * The text is parsed using the formatter, returning a date-time.
   *
   * @param text the text to parse, not null
   * @param formatter the formatter to use, not null
   * @return the parsed zoned date-time, never null
   * @throws UnsupportedOperationException if the formatter cannot parse
   * @throws CalendricalException if the text cannot be parsed
   */
  def parse(text: String, formatter: DateTimeFormatter): ZonedDateTime = {
    ISOChronology.checkNotNull(formatter, "DateTimeFormatter must not be null")
    formatter.parse(text, rule)
  }

  /**
   * Obtains the current date-time from the system clock in the default time-zone.
   * <p>
   * This will query the system clock in the default time-zone to obtain the current time.
   * The zone and offset will be set based on the time-zone in the clock.
   * <p>
   * Using this method will prevent the ability to use an alternate clock for testing
   * because the clock is hard-coded.
   *
   * @return the current date-time using the system clock, never null
   */
  def now: ZonedDateTime = now(Clock.systemDefaultZone)

  /**
   * Obtains an instance of {@code ZonedDateTime} from a local date and time
   * providing a resolver to handle an invalid date-time.
   * <p>
   * This factory creates a {@code ZonedDateTime} from a date, time and time-zone.
   * If the time is invalid for the zone, due to either being a gap or an overlap,
   * then the resolver will determine what action to take.
   * See {@link ZoneResolvers} for common resolver implementations.
   *
   * @param dateProvider the date provider to use, not null
   * @param timeProvider the time provider to use, not null
   * @param zone the time-zone, not null
   * @param resolver the resolver from local date-time to zoned, not null
   * @return the zoned date-time, never null
   * @throws CalendricalException if the resolver cannot resolve an invalid local date-time
   */
  def of(dateProvider: DateProvider, timeProvider: TimeProvider, zone: TimeZone, resolver: ZoneResolver = ZoneResolvers.strict): ZonedDateTime = {
    val dt: LocalDateTime = LocalDateTime.of(dateProvider, timeProvider)
    resolve(dt, null, zone, resolver)
  }

  /**
   * Rule implementation.
   */
  private[calendar] object Rule extends Rule

  @SerialVersionUID(1L)
  private[calendar] sealed class Rule
    extends CalendricalRule[ZonedDateTime](classOf[ZonedDateTime], ISOChronology, "ZonedDateTime", ISOChronology.periodNanos, null)
    with Serializable {
    private def readResolve: AnyRef = Rule
  }

}


/**
 * Constructor.
 *
 * @param dateTime the date-time, validated as not null
 * @param zone the time-zone, validated as not null
 */
@SerialVersionUID(-456761901L)
final class ZonedDateTime private(val dateTime: OffsetDateTime, val zone: TimeZone)
  extends InstantProvider with DateTimeProvider with Calendrical with CalendricalMatcher with Ordered[ZonedDateTime] with Serializable {

  import ZonedDateTime._

  /**
   * Converts this {@code ZonedDateTime} to a {@code LocalTime}.
   *
   * @return a LocalTime representing the time fields of this date-time, never null
   */
  def toLocalTime: LocalTime = dateTime.toLocalTime

  /**
   * Converts this {@code ZonedDateTime} to a {@code OffsetDateTime}.
   *
   * @return a OffsetDateTime representing the fields of this date-time, never null
   */
  def toOffsetDateTime: OffsetDateTime = dateTime

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period in days added.
   * <p>
   * This method adds the specified amount to the days field incrementing the
   * month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2008-12-31 plus one day would result in the 2009-01-01.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param days the days to add, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the days added, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def plusDays(days: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.plusDays(days)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this ZonedDateTime with a different time-zone,
   * retaining the local date-time if possible.
   * <p>
   * This method changes the time-zone and retains the local date-time.
   * The local date-time is only changed if it is invalid for the new zone.
   * In that case, the {@link ZoneResolvers#retainOffset ( ) retain offset} resolver is used.
   * <p>
   * To change the zone and adjust the local date-time,
   * use {@link #withZoneSameInstant ( TimeZone )}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param zone the time-zone to change to, not null
   * @return a {@code ZonedDateTime} based on this date-time with the requested zone, never null
   */
  def withZoneSameLocal(zone: TimeZone): ZonedDateTime = withZoneSameLocal(zone, ZoneResolvers.retainOffset)

  /**
   * Returns a copy of this ZonedDateTime changing the zone offset to the
   * earlier of the two valid offsets at a local time-line overlap.
   * <p>
   * This method only has any effect when the local time-line overlaps, such as
   * at an autumn daylight savings cutover. In this scenario, there are two
   * valid offsets for the local date-time. Calling this method will return
   * a zoned date-time with the earlier of the two selected.
   * <p>
   * If this method is called when it is not an overlap, {@code this }
   * is returned.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @return a {@code ZonedDateTime} based on this date-time with the earlier offset, never null
   * @throws CalendricalException if no rules can be found for the zone
   * @throws CalendricalException if no rules are valid for this date-time
   */
  def withEarlierOffsetAtOverlap: ZonedDateTime = {
    val info: ZoneOffsetInfo = getApplicableRules.getOffsetInfo(toLocalDateTime)
    if (info.isTransition) {
      val offset: ZoneOffset = info.getTransition.getOffsetBefore
      if (offset.equals(getOffset) == false) {
        val newDT: OffsetDateTime = dateTime.withOffsetSameLocal(offset)
        return new ZonedDateTime(newDT, zone)
      }
    }
    return this
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the time values altered.
   * <p>
   * This method will return a new instance with the same date fields,
   * but altered time fields.
   * This is a shorthand for {@link #withTime ( int, int, int )} and sets
   * the second field to zero.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @return a {@code ZonedDateTime} based on this date-time with the requested time, never null
   * @throws IllegalCalendarFieldValueException if any field value is invalid
   */
  def withTime(hourOfDay: Int, minuteOfHour: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.withTime(hourOfDay, minuteOfHour)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Gets the month-of-year field, which is an enum {@code MonthOfYear}.
   * <p>
   * This method returns the enum {@link MonthOfYear} for the month.
   * This avoids confusion as to what {@code int} values mean.
   * If you need access to the primitive {@code int} value then the enum
   * provides the {@link MonthOfYear#getValue ( ) int value}.
   * <p>
   * Additional information can be obtained from the {@code MonthOfYear}.
   * This includes month lengths, textual names and access to the quarter-of-year
   * and month-of-quarter values.
   *
   * @return the month-of-year, never null
   */
  def getMonthOfYear: MonthOfYear = dateTime.getMonthOfYear

  /**
   * Converts this {@code ZonedDateTime} to a {@code LocalDateTime}.
   *
   * @return a LocalDateTime representing the fields of this date-time, never null
   */
  override def toLocalDateTime: LocalDateTime = dateTime.toLocalDateTime

  /**
   * Returns a copy of this {@code ZonedDateTime} with the nano-of-second value altered.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param nanoOfSecond the nano-of-second to represent, from 0 to 999,999,999
   * @return a {@code ZonedDateTime} based on this date-time with the requested nanosecond, never null
   * @throws IllegalCalendarFieldValueException if the nanos value is invalid
   */
  def withNanoOfSecond(nanoOfSecond: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.withNanoOfSecond(nanoOfSecond)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period in days subtracted.
   * <p>
   * This method subtracts the specified amount to the days field incrementing the
   * month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2008-12-31 minus one day would result in the 2009-01-01.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param days the days to subtract, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the days subtracted, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def minusDays(days: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.minusDays(days)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the time values altered.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @return a {@code ZonedDateTime} based on this date-time with the requested time, never null
   * @throws IllegalCalendarFieldValueException if any field value is invalid
   */
  def withTime(hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.withTime(hourOfDay, minuteOfHour, secondOfMinute)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Gets the second-of-minute field.
   *
   * @return the second-of-minute, from 0 to 59
   */
  def getSecondOfMinute: Int = dateTime.getSecondOfMinute

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified duration subtracted.
   * <p>
   * This method {@link PeriodFields#toDuration ( ) converts} the period to a duration
   * based on the {@code ISOChronology} seconds and nanoseconds units.
   * The duration is then subtracted from the {@link #toInstant ( ) instant} equivalent of this instance.
   * <p>
   * Subtracting a duration differs from subtracting a period as gaps and overlaps in
   * the local time-line are taken into account. For example, if there is a
   * gap in the local time-line of one hour from 01:00 to 02:00, then subtracting a
   * duration of one hour from 02:30 will yield 00:30.
   * <p>
   * The subtraction of a duration is always absolute and zone-resolvers are not required.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to subtract, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the duration subtracted, never null
   * @throws ArithmeticException if the calculation exceeds the capacity of {@code Instant }
   * @throws CalendricalException if the result exceeds the supported range
   */
  def minusDuration(periodProvider: PeriodProvider): ZonedDateTime = {
    val period: PeriodFields = PeriodFields.of(periodProvider)
    val duration: Duration = period.toDuration
    if (duration.isZero) this else ofInstant(toInstant.minus(duration), zone)
  }

  /**
   * A hash code for this {@code ZonedDateTime}.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = dateTime.hashCode ^ zone.hashCode

  /**
   * Returns a copy of this ZonedDateTime with a different local date-time.
   * <p>
   * This method changes the offset date-time stored to a different one.
   * The local date-time is checked against the zone rules, and the retain
   * offset resolver used if necessary.
   *
   * @param dateTimeProvider the local date-time to change to, not null
   * @return a {@code ZonedDateTime} based on this date-time with the requested date-time, never null
   */
  def withDateTime(dateTimeProvider: DateTimeProvider): ZonedDateTime = {
    val localDateTime: LocalDateTime = LocalDateTime.of(dateTimeProvider)
    if (localDateTime.equals(this.dateTime.toLocalDateTime)) this else ZonedDateTime.resolve(localDateTime, this, zone, ZoneResolvers.retainOffset)
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period in years subtracted.
   * <p>
   * This method subtracts the specified amount to the years field in four steps:
   * <ol>
   * <li>Add the input years to the year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the day-of-month to the last valid day if necessary</li>
   * <li>Resolve the date-time using {@link ZoneResolvers#retainOffset()} </li>
   * </ol>
   * <p>
   * For example, 2008-02-29 (leap year) minus one year would result in the
   * invalid date 2009-02-29 (standard year). Instead of returning an invalid
   * result, the last valid day of the month, 2009-02-28, is selected instead.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to subtract, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the years subtracted, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def minusYears(years: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.minusYears(years)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period in years added.
   * <p>
   * This method add the specified amount to the years field in four steps:
   * <ol>
   * <li>Add the input years to the year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the day-of-month to the last valid day if necessary</li>
   * <li>Resolve the date-time using {@link ZoneResolvers#retainOffset()} </li>
   * </ol>
   * <p>
   * For example, 2008-02-29 (leap year) plus one year would result in the
   * invalid date 2009-02-29 (standard year). Instead of returning an invalid
   * result, the last valid day of the month, 2009-02-28, is selected instead.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to add, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the years added, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def plusYears(years: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.plusYears(years)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period in hours added.
   * <p>
   * This method uses field based addition.
   * This method changes the field by the specified number of hours.
   * This may, at daylight savings cutover, result in a duration being added
   * that is more or less than the specified number of hours.
   * <p>
   * For example, consider a time-zone where the spring DST cutover means that
   * the local times 01:00 to 01:59 do not exist. Using this method, adding
   * a period of 2 hours to 00:30 will result in 02:30, but it is important
   * to note that the change in duration was only 1 hour.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hours the hours to add, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the hours added, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def plusHours(hours: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.plusHours(hours)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the year value altered.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param year the year to represent, from MIN_YEAR to MAX_YEAR
   * @return a {@code ZonedDateTime} based on this date-time with the requested year, never null
   * @throws IllegalCalendarFieldValueException if the year value is invalid
   */
  def withYear(year: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.withYear(year)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the second-of-minute value altered.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @return a {@code ZonedDateTime} based on this date-time with the requested second, never null
   * @throws IllegalCalendarFieldValueException if the second value is invalid
   */
  def withSecondOfMinute(secondOfMinute: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.withSecondOfMinute(secondOfMinute)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this ZonedDateTime with a different time-zone,
   * retaining the instant.
   * <p>
   * This method changes the time-zone and retains the instant.
   * This normally results in a change to the local date-time.
   * <p>
   * This method is based on retaining the same instant, thus gaps and overlaps
   * in the local time-line have no effect on the result.
   * <p>
   * To change the offset while keeping the local time,
   * use {@link #withZoneSameLocal ( TimeZone )}.
   *
   * @param zone the time-zone to change to, not null
   * @return a {@code ZonedDateTime} based on this date-time with the requested zone, never null
   * @throws CalendarConversionException if the result exceeds the supported date range
   */
  def withZoneSameInstant(zone: TimeZone): ZonedDateTime = if (zone == this.zone) this else ofInstant(dateTime, zone)

  /**
   * Gets the chronology that this date-time uses, which is the ISO calendar system.
   *
   * @return the ISO chronology, never null
   */
  def getChronology: ISOChronology = ISOChronology

  /**
   * Checks if the instant of this date-time is equal to that of the specified date-time.
   * <p>
   * This method differs from the comparison in {@link #compareTo} and {@link #equals }
   * in that it compares only the instant of the date-time. This is equivalent to using
   * {@code dateTime1.toInstant ( ).equals ( dateTime2.toInstant ( ) );}.
   *
   * @param other the other date-time to compare to, not null
   * @return true if this is after the specified date-time
   * @throws NullPointerException if {@code other} is null
   */
  def equalInstant(other: ZonedDateTime): Boolean = toEpochSeconds == other.toEpochSeconds && getNanoOfSecond == other.getNanoOfSecond

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period in nanoseconds subtracted.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param nanos the nanos to subtract, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the nanoseconds subtracted, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def minusNanos(nanos: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.minusNanos(nanos)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Checks if this {@code ZonedDateTime} is equal to the specified date-time.
   * <p>
   * This compares the date-time and the offset.
   *
   * @param other the other date-time to compare to, null returns false
   * @return true if this point is equal to the specified date-time
   */
  override def equals(other: AnyRef): Boolean = {
    if (this eq other) true
    else if (other.isInstanceOf[ZonedDateTime]) {
      val zonedDateTime: ZonedDateTime = other.asInstanceOf[ZonedDateTime]
      dateTime.equals(zonedDateTime.dateTime) && zone.equals(zonedDateTime.zone)
    }
    else false
  }

  /**
   * Gets the minute-of-hour field.
   *
   * @return the minute-of-hour, from 0 to 59
   */
  def getMinuteOfHour: Int = dateTime.getMinuteOfHour

  /**
   * Gets the year field.
   * <p>
   * This method returns the primitive {@code int} value for the year.
   * <p>
   * Additional information about the year can be obtained via {@link #toYear}.
   * This returns a {@code Year} object which includes information on whether
   * this is a leap year and its length in days.
   *
   * @return the year, from MIN_YEAR to MAX_YEAR
   */
  def getYear: Int = dateTime.getYear

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period in minutes added.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param minutes the minutes to add, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the minutes added, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def plusMinutes(minutes: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.plusMinutes(minutes)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the time altered using the adjuster.
   * <p>
   * Adjusters can be used to alter the time in various ways.
   * A simple adjuster might simply set the one of the fields, such as the hour field.
   * A more complex adjuster might set the time to end of the working day.
   * <p>
   * If the adjusted date results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param adjuster the adjuster to use, not null
   * @return a {@code ZonedDateTime} based on this date-time with the time adjusted, never null
   * @throws IllegalArgumentException if the adjuster returned null
   */
  def `with`(adjuster: TimeAdjuster): ZonedDateTime = `with`(adjuster, ZoneResolvers.retainOffset)

  /**
   * Converts this {@code ZonedDateTime} to the number of seconds from the epoch
   * of 1970-01-01T00:00:00Z.
   * <p>
   * Instants on the time-line after the epoch are positive, earlier are negative.
   *
   * @return the number of seconds from the epoch of 1970-01-01T00:00:00Z
   */
  def toEpochSeconds: Long = dateTime.toEpochSeconds

  /**
   * Converts this {@code ZonedDateTime} to a {@code OffsetDate}.
   *
   * @return a OffsetDate representing the date fields of this date-time, never null
   */
  def toOffsetDate: OffsetDate = dateTime.toOffsetDate

  /**
   * Outputs this date-time as a {@code String} using the formatter.
   *
   * @param formatter the formatter to use, not null
   * @return the formatted date-time string, never null
   * @throws UnsupportedOperationException if the formatter cannot print
   * @throws CalendricalPrintException if an error occurs during printing
   */
  def toString(formatter: DateTimeFormatter): String = {
    ISOChronology.checkNotNull(formatter, "DateTimeFormatter must not be null")
    return formatter.print(this)
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the minute-of-hour value altered.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @return a {@code ZonedDateTime} based on this date-time with the requested minute, never null
   * @throws IllegalCalendarFieldValueException if the minute value is invalid
   */
  def withMinuteOfHour(minuteOfHour: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.withMinuteOfHour(minuteOfHour)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the day-of-year altered.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfYear the day-of-year to set in the returned date, from 1 to 365-366
   * @return a {@code ZonedDateTime} based on this date with the requested day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-year value is invalid
   * @throws InvalidCalendarFieldException if the day-of-year is invalid for the year
   */
  def withDayOfYear(dayOfYear: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.withDayOfYear(dayOfYear)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period in nanoseconds added.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param nanos the nanos to add, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the nanoseconds added, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def plusNanos(nanos: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.plusNanos(nanos)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period subtracted.
   * <p>
   * This subtracts the specified period from this date-time, returning a new date-time.
   * Before subtraction, the period is converted to a {@code Period} using the
   * {@link Period#of ( PeriodProvider )}.
   * <p>
   * The subtraction occurs based on the local date-time.
   * After the calculation, the local date-time may be in a gap or overlap.
   * If so, then the specified resolver is used.
   * <p>
   * The detailed rules for the subtraction have some complexity due to variable length months.
   * See {@link LocalDateTime#minus ( PeriodProvider )} for details.
   * <p>
   * See {@link #minusDuration ( PeriodProvider )} for a similar method that performs
   * the subtraction in a different manner, taking into account gaps and overlaps.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to subtract, not null
   * @return a {@code ZonedDateTime} based on this date-time with the period subtracted, never null
   * @throws CalendricalException if the specified period cannot be converted to a {@code Period }
   * @throws CalendricalException if the result exceeds the supported range
   */
  def minus(periodProvider: PeriodProvider, resolver: ZoneResolver): ZonedDateTime = {
    ISOChronology.checkNotNull(periodProvider, "PeriodProvider must not be null")
    ISOChronology.checkNotNull(resolver, "ZoneResolver must not be null")
    var newDT: LocalDateTime = dateTime.toLocalDateTime.minus(periodProvider)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, resolver))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period in months subtracted.
   * <p>
   * This method subtracts the specified amount to the months field in four steps:
   * <ol>
   * <li>Add the input months to the month-of-year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the day-of-month to the last valid day if necessary</li>
   * <li>Resolve the date-time using {@link ZoneResolvers#retainOffset()} </li>
   * </ol>
   * <p>
   * For example, 2007-03-31 minus one month would result in the invalid date
   * 2007-04-31. Instead of returning an invalid result, the last valid day
   * of the month, 2007-04-30, is selected instead.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to subtract, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the months subtracted, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def minusMonths(months: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.minusMonths(months)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this ZonedDateTime changing the zone offset to the
   * later of the two valid offsets at a local time-line overlap.
   * <p>
   * This method only has any effect when the local time-line overlaps, such as
   * at an autumn daylight savings cutover. In this scenario, there are two
   * valid offsets for the local date-time. Calling this method will return
   * a zoned date-time with the later of the two selected.
   * <p>
   * If this method is called when it is not an overlap, {@code this }
   * is returned.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @return a {@code ZonedDateTime} based on this date-time with the later offset, never null
   * @throws CalendricalException if no rules can be found for the zone
   * @throws CalendricalException if no rules are valid for this date-time
   */
  def withLaterOffsetAtOverlap: ZonedDateTime = {
    val info: ZoneOffsetInfo = getApplicableRules.getOffsetInfo(toLocalDateTime)
    if (info.isTransition) {
      val offset: ZoneOffset = info.getTransition.getOffsetAfter
      if (offset.equals(getOffset) == false) {
        val newDT: OffsetDateTime = dateTime.withOffsetSameLocal(offset)
        return new ZonedDateTime(newDT, zone)
      }
    }
    return this
  }

  /**
   * Checks if the instant of this date-time is after that of the specified date-time.
   * <p>
   * This method differs from the comparison in {@link #compareTo} in that it
   * compares the only the instant of the date-time. This is equivalent to using
   * {@code dateTime1.toInstant ( ).isAfter ( dateTime2.toInstant ( ) );}.
   *
   * @param other the other date-time to compare to, not null
   * @return true if this is after the specified date-time
   * @throws NullPointerException if {@code other} is null
   */
  def isAfter(other: ZonedDateTime): Boolean = {
    var thisEpochSecs: Long = toEpochSeconds
    var otherEpochSecs: Long = other.toEpochSeconds
    return thisEpochSecs > otherEpochSecs || (thisEpochSecs == otherEpochSecs && getNanoOfSecond > other.getNanoOfSecond)
  }

  /**
   * Gets the year field as a {@code Year}.
   * <p>
   * This method provides access to an object representing the year field.
   * {@code Year} has methods for querying addition year-based information.
   *
   * @return the year, never null
   */
  def toYear: Year = dateTime.toYear

  /**
   * Returns a copy of this {@code ZonedDateTime} with the date altered using the
   * adjuster, providing a resolver to handle an invalid date-time.
   * <p>
   * Adjusters can be used to alter the date in various ways.
   * A simple adjuster might simply set the one of the fields, such as the year field.
   * A more complex adjuster might set the date to the last day of the month.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param adjuster the adjuster to use, not null
   * @param resolver the resolver to use, not null
   * @return a {@code ZonedDateTime} based on this date-time with the date adjusted, never null
   * @throws IllegalArgumentException if the adjuster returned null
   * @throws IllegalCalendarFieldValueException if the resolver cannot resolve the date-time
   */
  def `with`(adjuster: DateAdjuster, resolver: ZoneResolver): ZonedDateTime = {
    ISOChronology.checkNotNull(adjuster, "DateAdjuster must not be null")
    ISOChronology.checkNotNull(resolver, "ZoneResolver must not be null")
    var newDT: LocalDateTime = dateTime.toLocalDateTime.`with`(adjuster)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, resolver))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period added.
   * <p>
   * This adds the specified period to this date-time, returning a new date-time.
   * Before addition, the period is converted to a {@code Period} using the
   * {@link Period#of ( PeriodProvider )}.
   * <p>
   * The addition occurs based on the local date-time.
   * After the calculation, the local date-time may be in a gap or overlap.
   * If so, then the specified resolver is used.
   * <p>
   * The detailed rules for the addition have some complexity due to variable length months.
   * See {@link LocalDateTime#plus ( PeriodProvider )} for details.
   * <p>
   * See {@link #plusDuration ( PeriodProvider )} for a similar method that performs
   * the addition in a different manner, taking into account gaps and overlaps.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to add, not null
   * @return a {@code ZonedDateTime} based on this date-time with the period added, never null
   * @throws CalendricalException if the specified period cannot be converted to a {@code Period }
   * @throws CalendricalException if the result exceeds the supported range
   */
  def plus(periodProvider: PeriodProvider, resolver: ZoneResolver = ZoneResolvers.retainOffset): ZonedDateTime = {
    ISOChronology.checkNotNull(periodProvider, "PeriodProvider must not be null")
    ISOChronology.checkNotNull(resolver, "ZoneResolver must not be null")
    var newDT: LocalDateTime = dateTime.toLocalDateTime.plus(periodProvider)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, resolver))
  }

  def +(periodProvider: PeriodProvider): ZonedDateTime = plus(periodProvider)

  /**
   * Converts this {@code ZonedDateTime} to an {@code Instant}.
   *
   * @return an Instant representing the same instant, never null
   */
  override def toInstant: Instant = dateTime.toInstant

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period in weeks subtracted.
   * <p>
   * This method subtracts the specified amount in weeks to the days field incrementing
   * the month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2008-12-31 minus one week would result in the 2009-01-07.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param weeks the weeks to subtract, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the weeks subtracted, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def minusWeeks(weeks: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.minusWeeks(weeks)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the time altered using the
   * adjuster, providing a resolver to handle an invalid date-time.
   * <p>
   * Adjusters can be used to alter the time in various ways.
   * A simple adjuster might simply set the one of the fields, such as the hour field.
   * A more complex adjuster might set the time to end of the working day.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param adjuster the adjuster to use, not null
   * @param resolver the resolver to use, not null
   * @return a {@code ZonedDateTime} based on this date-time with the time adjusted, never null
   * @throws IllegalArgumentException if the adjuster returned null
   * @throws IllegalCalendarFieldValueException if the resolver cannot resolve the date-time
   */
  def `with`(adjuster: TimeAdjuster, resolver: ZoneResolver): ZonedDateTime = {
    ISOChronology.checkNotNull(adjuster, "TimeAdjuster must not be null")
    ISOChronology.checkNotNull(resolver, "ZoneResolver must not be null")
    var newDT: LocalDateTime = dateTime.toLocalDateTime.`with`(adjuster)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, resolver))
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
   * Outputs this date-time as a {@code String}, such as
   * {@code 2007 -12-03T10:15:30+01:00[Europe/Paris]}.
   * <p>
   * The output will be one of the following formats:
   * <ul>
   * <li> {@code yyyy -MM-dd'T'HH:mmZZZZ'['I']'} </li>
   * <li> {@code yyyy -MM-dd'T'HH:mm:ssZZZZ'['I']'} </li>
   * <li> {@code yyyy -MM-dd'T'HH:mm:ssfnnnZZZZ'['I']'} </li>
   * <li> {@code yyyy -MM-dd'T'HH:mm:ssfnnnnnnZZZZ'['I']'} </li>
   * <li> {@code yyyy -MM-dd'T'HH:mm:ssfnnnnnnnnnZZZZ'['I']'} </li>
   * </ul>
   * The format used will be the shortest that outputs the full value of
   * the time where the omitted parts are implied to be zero.
   *
   * @return the formatted date-time, never null
   */
  override def toString: String = dateTime.toString + '[' + zone.toString + ']'

  /**
   * Returns a copy of this ZonedDateTime with a different time-zone,
   * retaining the local date-time if possible.
   * <p>
   * This method changes the time-zone and retains the local date-time.
   * The local date-time is only changed if it is invalid for the new zone.
   * In that case, the specified resolver is used.
   * <p>
   * To change the zone and adjust the local date-time,
   * use {@link #withZoneSameInstant ( TimeZone )}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param zone the time-zone to change to, not null
   * @param resolver the resolver to use, not null
   * @return a {@code ZonedDateTime} based on this date-time with the requested zone, never null
   */
  def withZoneSameLocal(zone: TimeZone, resolver: ZoneResolver): ZonedDateTime = {
    ISOChronology.checkNotNull(zone, "TimeZone must not be null")
    ISOChronology.checkNotNull(resolver, "ZoneResolver must not be null")
    return if (zone == this.zone) this else resolve(dateTime.toLocalDateTime, this, zone, resolver)
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified duration added.
   * <p>
   * Adding a duration differs from adding a period as gaps and overlaps in
   * the local time-line are taken into account. For example, if there is a
   * gap in the local time-line of one hour from 01:00 to 02:00, then adding a
   * duration of one hour to 00:30 will yield 02:30.
   * <p>
   * The addition of a duration is always absolute and zone-resolvers are not required.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hours the hours to add, positive or negative
   * @param minutes the minutes to add, positive or negative
   * @param seconds the seconds to add, positive or negative
   * @param nanos the nanos to add, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the duration added, never null
   * @throws ArithmeticException if the calculation exceeds the capacity of {@code Instant }
   * @throws CalendricalException if the result exceeds the supported range
   */
  def plusDuration(hours: Int, minutes: Int, seconds: Int, nanos: Long): ZonedDateTime = {
    if ((hours | minutes | seconds | nanos) == 0) return this
    val instant: Instant = toInstant.plusSeconds(hours * 3600L + minutes * 60L + seconds).plusNanos(nanos)
    return ofInstant(instant, zone)
  }

  /**
   * Converts this {@code ZonedDateTime} to a {@code LocalDate}.
   *
   * @return a LocalDate representing the date fields of this date-time, never null
   */
  def toLocalDate: LocalDate = dateTime.toLocalDate

  /**
   * Gets the zone offset, such as '+01:00'.
   *
   * @return the zone offset, never null
   */
  def getOffset: ZoneOffset = dateTime.getOffset

  /**
   * Returns a copy of this {@code ZonedDateTime} with the day-of-month value altered.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @return a {@code ZonedDateTime} based on this date-time with the requested day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-month value is invalid
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def withDayOfMonth(dayOfMonth: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.withDayOfMonth(dayOfMonth)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified duration subtracted.
   * <p>
   * Subtracting a duration differs from subtracting a period as gaps and overlaps in
   * the local time-line are taken into account. For example, if there is a
   * gap in the local time-line of one hour from 01:00 to 02:00, then subtracting a
   * duration of one hour from 02:30 will yield 00:30.
   * <p>
   * The subtraction of a duration is always absolute and zone-resolvers are not required.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hours the hours to subtract, positive or negative
   * @param minutes the minutes to subtract, positive or negative
   * @param seconds the seconds to subtract, positive or negative
   * @param nanos the nanos to subtract, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the duration subtracted, never null
   * @throws ArithmeticException if the calculation exceeds the capacity of {@code Instant }
   * @throws CalendricalException if the result exceeds the supported range
   */
  def minusDuration(hours: Int, minutes: Int, seconds: Int, nanos: Long): ZonedDateTime = {
    if ((hours | minutes | seconds | nanos) == 0) return this
    val instant: Instant = toInstant.minusSeconds(hours * 3600L + minutes * 60L + seconds).minusNanos(nanos)
    return ofInstant(instant, zone)
  }

  /**
   * Calculates the zone rules applicable for this date-time.
   * <p>
   * The rules provide the information on how the zone offset changes over time.
   * This usually includes historical and future information.
   * The rules are determined using {@link TimeZone#getRulesValidFor ( OffsetDateTime ) }
   * which finds the best matching set of rules for this date-time.
   * <p>
   * This method can throw an exception if the time-zone is invalid for this JVM.
   *
   * @return the time-zone rules, never null
   * @throws CalendricalException if no rules can be found for the zone
   * @throws CalendricalException if no rules are valid for this date-time
   */
  def getApplicableRules: ZoneRules = zone.getRulesValidFor(dateTime)

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified duration added.
   * <p>
   * This method {@link PeriodFields#toDuration ( ) converts} the period to a duration
   * based on the {@code ISOChronology} seconds and nanoseconds units.
   * The duration is then added to the {@link #toInstant ( ) instant} equivalent of this instance.
   * <p>
   * Adding a duration differs from adding a period as gaps and overlaps in
   * the local time-line are taken into account. For example, if there is a
   * gap in the local time-line of one hour from 01:00 to 02:00, then adding a
   * duration of one hour to 00:30 will yield 02:30.
   * <p>
   * The addition of a duration is always absolute and zone-resolvers are not required.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to add, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the duration added, never null
   * @throws ArithmeticException if the calculation exceeds the capacity of {@code Instant }
   * @throws CalendricalException if the result exceeds the supported range
   */
  def plusDuration(periodProvider: PeriodProvider): ZonedDateTime = {
    val period: PeriodFields = PeriodFields.of(periodProvider)
    val duration: Duration = period.toDuration
    if (duration.isZero) this else ofInstant(toInstant.plus(duration), zone)
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period in seconds subtracted.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param seconds the seconds to subtract, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the seconds subtracted, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def minusSeconds(seconds: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.minusSeconds(seconds)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Gets the hour-of-day field.
   *
   * @return the hour-of-day, from 0 to 23
   */
  def getHourOfDay: Int = dateTime.getHourOfDay

  /**
   * Calculates the applicable versioned time-zone, such as 'Europe/Paris#2009b'.
   * <p>
   * The time-zone stored by this {@code ZonedDateTime} can have either a
   * fixed or a floating version. This method returns the time-zone with
   * a version, calculating the best matching version if necessary.
   * <p>
   * For a floating time-zone, the applicable version is the latest version
   * for which the offset date-time contained in this object would be valid.
   * <p>
   * This method can throw an exception if the time-zone is invalid for this JVM.
   *
   * @return the time-zone complete with version, never null
   * @throws CalendricalException if no rules can be found for the zone
   * @throws CalendricalException if no rules are valid for this date-time
   */
  def getApplicableZone: TimeZone = {
    if (zone.isFloatingVersion) zone.withLatestVersionValidFor(dateTime)
    else zone
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period in seconds added.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param seconds the seconds to add, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the seconds added, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def plusSeconds(seconds: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.plusSeconds(seconds)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the time values altered.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @param nanoOfSecond the nano-of-second to represent, from 0 to 999,999,999
   * @return a {@code ZonedDateTime} based on this date-time with the requested time, never null
   * @throws IllegalCalendarFieldValueException if any field value is invalid
   */
  def withTime(hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int, nanoOfSecond: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.withTime(hourOfDay, minuteOfHour, secondOfMinute, nanoOfSecond)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period in hours subtracted.
   * <p>
   * This method uses field based subtraction.
   * This method changes the field by the specified number of hours.
   * This may, at daylight savings cutover, result in a duration being subtracted
   * that is more or less than the specified number of hours.
   * <p>
   * For example, consider a time-zone where the spring DST cutover means that
   * the local times 01:00 to 01:59 do not exist. Using this method, subtracting
   * a period of 2 hours from 02:30 will result in 00:30, but it is important
   * to note that the change in duration was only 1 hour.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hours the hours to subtract, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the hours subtracted, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def minusHours(hours: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.minusHours(hours)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period in weeks added.
   * <p>
   * This method adds the specified amount in weeks to the days field incrementing
   * the month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2008-12-31 plus one week would result in the 2009-01-07.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param weeks the weeks to add, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the weeks added, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def plusWeeks(weeks: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.plusWeeks(weeks)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period in minutes subtracted.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param minutes the minutes to subtract, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the minutes subtracted, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def minusMinutes(minutes: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.minusMinutes(minutes)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Gets the time-zone, such as 'Europe/Paris'.
   *
   * @return the time-zone, never null
   */
  def getZone: TimeZone = zone

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period in months added.
   * <p>
   * This method adds the specified amount to the months field in four steps:
   * <ol>
   * <li>Add the input months to the month-of-year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the day-of-month to the last valid day if necessary</li>
   * <li>Resolve the date-time using {@link ZoneResolvers#retainOffset()} </li>
   * </ol>
   * <p>
   * For example, 2007-03-31 plus one month would result in the invalid date
   * 2007-04-31. Instead of returning an invalid result, the last valid day
   * of the month, 2007-04-30, is selected instead.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to add, positive or negative
   * @return a {@code ZonedDateTime} based on this date-time with the months added, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def plusMonths(months: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.plusMonths(months)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the specified period subtracted.
   * <p>
   * This subtracts the specified period from this date-time, returning a new date-time.
   * Before subtraction, the period is converted to a {@code Period} using the
   * {@link Period#of ( PeriodProvider )}.
   * <p>
   * The subtraction occurs based on the local date-time.
   * After the calculation, the local date-time may be in a gap or overlap.
   * If so, then the {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * The detailed rules for the subtraction have some complexity due to variable length months.
   * See {@link LocalDateTime#minus ( PeriodProvider )} for details.
   * <p>
   * See {@link #minusDuration ( PeriodProvider )} for a similar method that performs
   * the subtraction in a different manner, taking into account gaps and overlaps.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to subtract, not null
   * @return a {@code ZonedDateTime} based on this date-time with the period subtracted, never null
   * @throws CalendricalException if the specified period cannot be converted to a {@code Period }
   * @throws CalendricalException if the result exceeds the supported range
   */
  def minus(periodProvider: PeriodProvider): ZonedDateTime = minus(periodProvider, ZoneResolvers.retainOffset)

  /**
   * Compares this {@code ZonedDateTime} to another date-time based on the UTC
   * equivalent date-times then time-zone unique key.
   * <p>
   * The ordering is consistent with equals as it takes into account
   * the date-time, offset and zone.
   *
   * @param other the other date-time to compare to, not null
   * @return the comparator value, negative if less, positive if greater
   * @throws NullPointerException if {@code other} is null
   */
  def compare(other: ZonedDateTime): Int = {
    var compare: Int = dateTime.compareTo(other.dateTime)
    if (compare == 0) {
      compare = zone.getID.compareTo(other.zone.getID)
    }
    return compare
  }

  /**
   * Gets the nano-of-second field.
   *
   * @return the nano-of-second, from 0 to 999,999,999
   */
  def getNanoOfSecond: Int = dateTime.getNanoOfSecond

  /**
   * Gets the day-of-month field.
   * <p>
   * This method returns the primitive {@code int} value for the day-of-month.
   *
   * @return the day-of-month, from 1 to 31
   */
  def getDayOfMonth: Int = dateTime.getDayOfMonth

  /**
   * Returns a copy of this {@code ZonedDateTime} with the month-of-year value altered.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear the month-of-year to represent, from 1 (January) to 12 (December)
   * @return a {@code ZonedDateTime} based on this date-time with the requested month, never null
   * @throws IllegalCalendarFieldValueException if the month value is invalid
   */
  def withMonthOfYear(monthOfYear: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.withMonthOfYear(monthOfYear)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Gets the day-of-week field, which is an enum {@code DayOfWeek}.
   * <p>
   * This method returns the enum {@link DayOfWeek} for the day-of-week.
   * This avoids confusion as to what {@code int} values mean.
   * If you need access to the primitive {@code int} value then the enum
   * provides the {@link DayOfWeek#getValue ( ) int value}.
   * <p>
   * Additional information can be obtained from the {@code DayOfWeek}.
   * This includes textual names of the values.
   *
   * @return the day-of-week, never null
   */
  def getDayOfWeek: DayOfWeek = dateTime.getDayOfWeek

  /**
   * Checks if the date-time extracted from the calendrical matches this.
   *
   * @param calendrical the calendrical to match, not null
   * @return true if the calendrical matches, false otherwise
   */
  override def matchesCalendrical(calendrical: Calendrical): Boolean = this.equals(calendrical.get(rule))

  /**
   * Returns a copy of this {@code ZonedDateTime} with the date values altered.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This method will return a new instance with the same time fields,
   * but altered date fields.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param year the year to represent, from MIN_YEAR to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, from 1 (January) to 12 (December)
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @return a {@code ZonedDateTime} based on this date-time with the requested date, never null
   * @throws IllegalCalendarFieldValueException if the any field value is invalid
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def withDate(year: Int, monthOfYear: Int, dayOfMonth: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.withDate(year, monthOfYear, dayOfMonth)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }

  /**
   * Returns a copy of this {@code ZonedDateTime} with the date altered using the adjuster.
   * <p>
   * Adjusters can be used to alter the date in various ways.
   * A simple adjuster might simply set the one of the fields, such as the year field.
   * A more complex adjuster might set the date to the last day of the month.
   * <p>
   * If the adjusted date results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param adjuster the adjuster to use, not null
   * @return a {@code ZonedDateTime} based on this date-time with the date adjusted, never null
   * @throws IllegalArgumentException if the adjuster returned null
   */
  def `with`(adjuster: DateAdjuster): ZonedDateTime = `with`(adjuster, ZoneResolvers.retainOffset)

  /**
   * Converts this {@code ZonedDateTime} to a {@code OffsetTime}.
   *
   * @return a OffsetTime representing the time fields of this date-time, never null
   */
  def toOffsetTime: OffsetTime = dateTime.toOffsetTime

  /**
   * Gets the day-of-year field.
   * <p>
   * This method returns the primitive {@code int} value for the day-of-year.
   *
   * @return the day-of-year, from 1 to 365, or 366 in a leap year
   */
  def getDayOfYear: Int = dateTime.getDayOfYear

  /**
   * Checks if the instant of this date-time is before that of the specified date-time.
   * <p>
   * This method differs from the comparison in {@link #compareTo} in that it
   * compares only the instant of the date-time. This is equivalent to using
   * {@code dateTime1.toInstant ( ).isBefore ( dateTime2.toInstant ( ) );}.
   *
   * @param other the other date-time to compare to, not null
   * @return true if this point is before the specified date-time
   * @throws NullPointerException if {@code other} is null
   */
  def isBefore(other: ZonedDateTime): Boolean = {
    var thisEpochSecs: Long = toEpochSeconds
    var otherEpochSecs: Long = other.toEpochSeconds
    return thisEpochSecs < otherEpochSecs || (thisEpochSecs == otherEpochSecs && getNanoOfSecond < other.getNanoOfSecond)
  }

  /**
   * Gets the value of the specified calendrical rule.
   * <p>
   * This method queries the value of the specified calendrical rule.
   * If the value cannot be returned for the rule from this date-time then
   * {@code null} will be returned.
   *
   * @param rule the rule to use, not null
   * @return the value for the rule, null if the value cannot be returned
   */
  //  def get[T](rule: CalendricalRule[T]): Option[T] = Some(rule.deriveValueFor(rule, this, this, ISOChronology)) //FIXME
  def get[T](rule: CalendricalRule[T]): Option[T] = None

  /**
   * Returns a copy of this {@code ZonedDateTime} with the hour-of-day value altered.
   * <p>
   * If the adjustment results in a date-time that is invalid, then the
   * {@link ZoneResolvers#retainOffset()} resolver is used.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @return a {@code ZonedDateTime} based on this date-time with the requested hour, never null
   * @throws IllegalCalendarFieldValueException if the hour value is invalid
   */
  def withHourOfDay(hourOfDay: Int): ZonedDateTime = {
    var newDT: LocalDateTime = dateTime.toLocalDateTime.withHourOfDay(hourOfDay)
    return (if (newDT == dateTime.toLocalDateTime) this else resolve(newDT, this, zone, ZoneResolvers.retainOffset))
  }
}