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
import javax.time.CalendricalException
import javax.time.Instant
import javax.time.MathUtils
import javax.time.calendar.format.DateTimeFormatter
import javax.time.calendar.format.DateTimeFormatters

/**
 * A time without time-zone in the ISO-8601 calendar system,
 * such as '10:15:30'.
 * <p>
 * LocalTime is an immutable calendrical that represents a time, often
 * viewed as hour-minute-second.
 * <p>
 * This class stores all time fields, to a precision of nanoseconds.
 * It does not store or represent a date or time-zone. Thus, for example, the
 * value "13:45.30.123456789" can be stored in a LocalTime.
 * <p>
 * LocalTime is immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object LocalTime {
  /**
   * Obtains the current time from the specified clock.
   * <p>
   * This will query the specified clock to obtain the current time.
   * Using this method allows the use of an alternate clock for testing.
   * The alternate clock may be introduced using    { @link Clock dependency injection }.
   *
   * @param clock the clock to use, not null
   * @return the current time, never null
   */
  def now(clock: Clock): LocalTime = {
    ISOChronology.checkNotNull(clock, "Clock must not be null")
    val instant: Instant = clock.instant
    val offset: ZoneOffset = clock.getZone.getRules.getOffset(instant)
    var secsOfDay: Long = instant.getEpochSeconds % ISOChronology.SECONDS_PER_DAY
    secsOfDay = (secsOfDay + offset.getAmountSeconds) % ISOChronology.SECONDS_PER_DAY
    if (secsOfDay < 0) {
      secsOfDay += ISOChronology.SECONDS_PER_DAY
    }
    return LocalTime.ofSecondOfDay(secsOfDay, instant.getNanoOfSecond)
  }

  /**
   * Constants for the local time of each hour.
   */
  private val HOURS: Array[LocalTime] = Array[LocalTime](new LocalTime(0, 0, 0, 0),
    new LocalTime(1, 0, 0, 0),
    new LocalTime(2, 0, 0, 0),
    new LocalTime(3, 0, 0, 0),
    new LocalTime(4, 0, 0, 0),
    new LocalTime(5, 0, 0, 0),
    new LocalTime(6, 0, 0, 0),
    new LocalTime(7, 0, 0, 0),
    new LocalTime(8, 0, 0, 0),
    new LocalTime(9, 0, 0, 0),
    new LocalTime(10, 0, 0, 0),
    new LocalTime(11, 0, 0, 0),
    new LocalTime(12, 0, 0, 0),
    new LocalTime(13, 0, 0, 0),
    new LocalTime(14, 0, 0, 0),
    new LocalTime(15, 0, 0, 0),
    new LocalTime(16, 0, 0, 0),
    new LocalTime(17, 0, 0, 0),
    new LocalTime(18, 0, 0, 0),
    new LocalTime(19, 0, 0, 0),
    new LocalTime(20, 0, 0, 0),
    new LocalTime(21, 0, 0, 0),
    new LocalTime(22, 0, 0, 0),
    new LocalTime(23, 0, 0, 0))
  /**
   * Constant for the local time of midnight, 00:00.
   */
  val MIDNIGHT: LocalTime = HOURS(0)
  /**
   * Constant for the local time of midday, 12:00.
   */
  val MIDDAY: LocalTime = HOURS(12)

  /**Seconds per minute. */
  private val SECONDS_PER_MINUTE: Int = 60

  /**
   * Rule implementation.
   */
  private[calendar] object Rule extends Rule

  @SerialVersionUID(1L)
  private[calendar] sealed class Rule
    extends CalendricalRule[LocalTime](classOf[LocalTime], ISOChronology, "LocalTime", ISOChronology.periodNanos, ISOChronology.periodDays)
    with Serializable {
    protected override def derive(calendrical: Calendrical): LocalTime = {
      val ldt: LocalDateTime = calendrical.get(LocalDateTime.rule)
      if (ldt != null) return ldt.toLocalTime
      val ot: OffsetTime = calendrical.get(OffsetTime.rule)
      if (ot != null) ot.toLocalTime
      return null
    }

    private def readResolve: AnyRef = Rule
  }

  /**Hours per minute. */
  private val HOURS_PER_DAY: Int = 24
  /**Nanos per second. */
  private val NANOS_PER_SECOND: Long = 1000000000L
  /**Seconds per hour. */
  private val SECONDS_PER_HOUR: Int = SECONDS_PER_MINUTE * MINUTES_PER_HOUR
  /**Seconds per day. */
  private val SECONDS_PER_DAY: Int = SECONDS_PER_HOUR * HOURS_PER_DAY
  /**
   * Obtains an instance of    { @code LocalTime } from an hour and minute.
   * <p>
   * The second and nanosecond fields will be set to zero by this factory method.
   * <p>
   * This factory may return a cached value, but applications must not rely on this.
   *
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @return the local time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   */
  def of(hourOfDay: Int, minuteOfHour: Int): LocalTime = {
    ISOChronology.hourOfDayRule.checkValue(hourOfDay)
    if (minuteOfHour == 0) {
      return HOURS(hourOfDay)
    }
    ISOChronology.minuteOfHourRule.checkValue(minuteOfHour)
    return new LocalTime(hourOfDay, minuteOfHour, 0, 0)
  }

  /**
   * Obtains an instance of    { @code LocalTime } from a text string using a specific formatter.
   * <p>
   * The text is parsed using the formatter, returning a time.
   *
   * @param text the text to parse, not null
   * @param formatter the formatter to use, not null
   * @return the parsed local time, never null
   * @throws UnsupportedOperationException if the formatter cannot parse
   * @throws CalendricalException if the text cannot be parsed
   */
  def parse(text: String, formatter: DateTimeFormatter): LocalTime = {
    ISOChronology.checkNotNull(formatter, "DateTimeFormatter must not be null")
    return formatter.parse(text, rule)
  }

  /**
   * Obtains an instance of    { @code LocalTime } from an hour, minute, second and nanosecond.
   * <p>
   * This factory may return a cached value, but applications must not rely on this.
   *
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @param nanoOfSecond the nano-of-second to represent, from 0 to 999,999,999
   * @return the local time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   */
  def of(hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int, nanoOfSecond: Int): LocalTime = {
    ISOChronology.hourOfDayRule.checkValue(hourOfDay)
    ISOChronology.minuteOfHourRule.checkValue(minuteOfHour)
    ISOChronology.secondOfMinuteRule.checkValue(secondOfMinute)
    ISOChronology.nanoOfSecondRule.checkValue(nanoOfSecond)
    return create(hourOfDay, minuteOfHour, secondOfMinute, nanoOfSecond)
  }

  /**
   * Obtains an instance of    { @code LocalTime } from an hour, minute and second.
   * <p>
   * The nanosecond field will be set to zero by this factory method.
   * <p>
   * This factory may return a cached value, but applications must not rely on this.
   *
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @return the local time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   */
  def of(hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int): LocalTime = {
    ISOChronology.hourOfDayRule.checkValue(hourOfDay)
    if ((minuteOfHour | secondOfMinute) == 0) {
      return HOURS(hourOfDay)
    }
    ISOChronology.minuteOfHourRule.checkValue(minuteOfHour)
    ISOChronology.secondOfMinuteRule.checkValue(secondOfMinute)
    return new LocalTime(hourOfDay, minuteOfHour, secondOfMinute, 0)
  }

  /**Nanos per hour. */
  private val NANOS_PER_HOUR: Long = NANOS_PER_MINUTE * MINUTES_PER_HOUR
  /**
   * Obtains an instance of    { @code LocalTime } from a second-of-day value.
   * <p>
   * This factory may return a cached value, but applications must not rely on this.
   *
   * @param secondOfDay the second-of-day, from    { @code 0 } to    { @code 24 * 60 * 60 - 1 }
   * @return the local time, never null
   * @throws IllegalCalendarFieldValueException if the second-of-day value is invalid
   */
  def ofSecondOfDay(_secondOfDay: Long): LocalTime = {
    var secondOfDay = _secondOfDay
    ISOChronology.secondOfDayRule.checkValue(secondOfDay)
    val hours: Int = (secondOfDay / SECONDS_PER_HOUR).toInt
    secondOfDay -= hours * SECONDS_PER_HOUR
    val minutes: Int = (secondOfDay / SECONDS_PER_MINUTE).toInt
    secondOfDay -= minutes * SECONDS_PER_MINUTE
    return create(hours, minutes, secondOfDay.toInt, 0)
  }

  /**
   * Obtains the current time from the system clock in the default time-zone.
   * <p>
   * This will query the system clock in the default time-zone to obtain the current time.
   * Using this method will prevent the ability to use an alternate clock for testing
   * because the clock is hard-coded.
   *
   * @return the current time using the system clock, never null
   */
  def nowSystemClock: LocalTime = now(Clock.systemDefaultZone)

  /**Minutes per day. */
  private val MINUTES_PER_DAY: Int = MINUTES_PER_HOUR * HOURS_PER_DAY
  /**Nanos per minute. */
  private val NANOS_PER_MINUTE: Long = NANOS_PER_SECOND * SECONDS_PER_MINUTE
  /**
   * Obtains an instance of    { @code LocalTime } from a time provider.
   * <p>
   * The purpose of this method is to convert a    { @code TimeProvider }
   * to a    { @code LocalTime } in the safest possible way. Specifically,
   * the means checking whether the input parameter is null and
   * whether the result of the provider is null.
   * <p>
   * This factory may return a cached value, but applications must not rely on this.
   *
   * @param timeProvider the time provider to use, not null
   * @return the local time, never null
   */
  def of(timeProvider: TimeProvider): LocalTime = {
    ISOChronology.checkNotNull(timeProvider, "TimeProvider must not be null")
    val result: LocalTime = timeProvider.toLocalTime
    ISOChronology.checkNotNull(result, "TimeProvider implementation must not return null")
    result
  }

  /**
   * Obtains an instance of    { @code LocalTime } from a nanos-of-day value.
   * <p>
   * This factory may return a cached value, but applications must not rely on this.
   *
   * @param nanoOfDay the nano of day, from    { @code 0 } to    { @code 24 * 60 * 60 * 1,000,000,000 - 1 }
   * @return the local time, never null
   * @throws CalendricalException if the nanos of day value is invalid
   */
  def ofNanoOfDay(_nanoOfDay: Long): LocalTime = {
    var nanoOfDay = _nanoOfDay
    if (nanoOfDay < 0) {
      throw new CalendricalException("Cannot create LocalTime from nanos of day as value " + nanoOfDay + " must not be negative")
    }
    if (nanoOfDay >= NANOS_PER_DAY) {
      throw new CalendricalException("Cannot create LocalTime from nanos of day as value " + nanoOfDay + " must be less than " + NANOS_PER_DAY)
    }
    val hours: Int = (nanoOfDay / NANOS_PER_HOUR).toInt
    nanoOfDay -= hours * NANOS_PER_HOUR
    val minutes: Int = (nanoOfDay / NANOS_PER_MINUTE).toInt
    nanoOfDay -= minutes * NANOS_PER_MINUTE
    val seconds: Int = (nanoOfDay / NANOS_PER_SECOND).toInt
    nanoOfDay -= seconds * NANOS_PER_SECOND
    create(hours, minutes, seconds, nanoOfDay.toInt)
  }

  /**Minutes per hour. */
  private val MINUTES_PER_HOUR: Int = 60
  /**Nanos per day. */
  private val NANOS_PER_DAY: Long = NANOS_PER_HOUR * HOURS_PER_DAY
  /**
   * Obtains an instance of    { @code LocalTime } from a second-of-day value, with
   * associated nanos of second.
   * <p>
   * This factory may return a cached value, but applications must not rely on this.
   *
   * @param secondOfDay the second-of-day, from    { @code 0 } to    { @code 24 * 60 * 60 - 1 }
   * @param nanoOfSecond the nano-of-second, from 0 to 999,999,999
   * @return the local time, never null
   * @throws IllegalCalendarFieldValueException if the either input value is invalid
   */
  def ofSecondOfDay(_secondOfDay: Long, nanoOfSecond: Int): LocalTime = {
    var secondOfDay = _secondOfDay
    ISOChronology.secondOfDayRule.checkValue(secondOfDay)
    ISOChronology.nanoOfSecondRule.checkValue(nanoOfSecond)
    val hours: Int = (secondOfDay / SECONDS_PER_HOUR).toInt
    secondOfDay -= hours * SECONDS_PER_HOUR
    val minutes: Int = (secondOfDay / SECONDS_PER_MINUTE).toInt
    secondOfDay -= minutes * SECONDS_PER_MINUTE
    return create(hours, minutes, secondOfDay.toInt, nanoOfSecond)
  }

  /**
   * Creates a local time from the hour, minute, second and nanosecond fields.
   * <p>
   * This factory may return a cached value, but applications must not rely on this.
   *
   * @param hourOfDay the hour-of-day to represent, validated from 0 to 23
   * @param minuteOfHour the minute-of-hour to represent, validated from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, validated from 0 to 59
   * @param nanoOfSecond the nano-of-second to represent, validated from 0 to 999,999,999
   * @return the local time, never null
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  private def create(hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int, nanoOfSecond: Int): LocalTime = {
    if ((minuteOfHour | secondOfMinute | nanoOfSecond) == 0) HOURS(hourOfDay)
    else new LocalTime(hourOfDay, minuteOfHour, secondOfMinute, nanoOfSecond)
  }

  /**
   * Obtains an instance of    { @code LocalTime } from a string.
   * <p>
   * The following formats are accepted in ASCII:
   * <ul>
   * <li>   { @code   { Hour } :   { Minute } }
   * <li>   { @code   { Hour } :   { Minute } :   { Second } }
   * <li>   { @code   { Hour } :   { Minute } :   { Second }.   { NanosecondFraction } }
   * </ul>
   * <p>
   * The hour has 2 digits with values from 0 to 23.
   * The minute has 2 digits with values from 0 to 59.
   * The second has 2 digits with values from 0 to 59.
   * The nanosecond fraction has from 1 to 9 digits with values from 0 to 999,999,999.
   *
   * @param text the text to parse such as '10:15:30', not null
   * @return the parsed local time, never null
   * @throws CalendricalException if the text cannot be parsed
   */
  def parse(text: String): LocalTime = DateTimeFormatters.isoLocalTime.parse(text, rule)

  /**
   * The result of addition to a    { @code LocalTime } allowing the expression of
   * any overflow in days.
   */


  /**
   * Constructor.
   *
   * @param time the    { @code LocalTime } after the addition, not null
   * @param days the overflow in days
   */
  final class Overflow private[LocalTime](val time: LocalTime, val days: Int) {
    /**
     * Returns a string description of this instance.
     *
     * @return the string, never null
     */
    override def toString: String = getResultTime.toString + " + P" + days + "D"

    /**
     * Compares this object to another.
     *
     * @param obj the object to compare to
     * @return true if equal
     */
    override def equals(obj: AnyRef): Boolean = {
      if (this == obj) true
      else if (obj.isInstanceOf[LocalTime.Overflow]) {
        val other: LocalTime.Overflow = obj.asInstanceOf[LocalTime.Overflow]
        time.equals(other.time) && days == other.days
      }
      else false
    }

    /**
     * Returns a suitable hash code.
     *
     * @return the hash code
     */
    override def hashCode: Int = time.hashCode + days

    /**
     * Gets the days overflowing from the calculation.
     *
     * @return the overflow days
     */
    def getOverflowDays: Int = days

    /**
     * Gets the time that was the result of the calculation.
     *
     * @return the time, never null
     */
    def getResultTime: LocalTime = time

    /**
     * Creates a    { @code LocalDateTime } from the specified date and this instance.
     *
     * @param date the date to use, not null
     * @return the combination of the date, time and overflow in days, never null
     */
    def toLocalDateTime(date: LocalDate): LocalDateTime = {
      LocalDateTime.of(date.plusDays(getOverflowDays), time)
    }
  }

  /**
   * Gets the rule for    { @code LocalTime }.
   *
   * @return the rule for the time, never null
   */
  def rule: CalendricalRule[LocalTime] = Rule
}

@SerialVersionUID(1L)
final class LocalTime private(val hour: Byte, val minute: Byte, val second: Byte, val nano: Int)
  extends Calendrical with TimeProvider with CalendricalMatcher
  with TimeAdjuster with Comparable[LocalTime] with Serializable {

  import LocalTime._

  /**
   * Checks if this    { @code LocalTime } is before the specified time.
   * <p>
   * The comparison is based on the time-line position of the time within a day.
   *
   * @param other the other time to compare to, not null
   * @return true if this point is before the specified time
   * @throws NullPointerException if    { @code other } is null
   */
  def isBefore(other: LocalTime): Boolean = compareTo(other) < 0

  /**
   * Constructor, previously validated.
   *
   * @param hour the hour-of-day to represent, validated from 0 to 23
   * @param minute the minute-of-hour to represent, validated from 0 to 59
   * @param second the second-of-minute to represent, validated from 0 to 59
   * @param nano the nano-of-second to represent, validated from 0 to 999,999,999
   */
  private def this(hour: Int, minute: Int, second: Int, nano: Int) {
    this (hour.toByte, minute.toByte, second.toByte, nano)
  }

  /**
   * Returns a copy of this    { @code LocalTime } with the time altered using the adjuster.
   * <p>
   * Adjusters can be used to alter the time in various ways.
   * A simple adjuster might simply set the one of the fields, such as the hour field.
   * A more complex adjuster might set the time to end of the working day.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param adjuster the adjuster to use, not null
   * @return a { @code LocalTime } based on this time adjusted as necessary, never null
   */
  def `with`(adjuster: TimeAdjuster): LocalTime = {
    var time: LocalTime = adjuster.adjustTime(this)
    if (time == null) {
      throw new NullPointerException("The implementation of TimeAdjuster must not return null")
    }
    time
  }

  /**
   * Returns this time wrapped as an days-overflow.
   * <p>
   * This method will generally only be needed by those writing low-level date
   * and time code that handles days-overflow. An overflow happens when adding
   * or subtracting to a time and the result overflows the range of a time.
   * The number of days later (or earlier) of the result is recorded in the overflow.
   *
   * @param daysOverflow the number of days to store
   * @return the days-overflow, never null
   */
  def toOverflow(daysOverflow: Int): LocalTime.Overflow = new LocalTime.Overflow(this, daysOverflow)

  /**
   * Returns a copy of this    { @code LocalTime } with the specified period in nanoseconds added.
   * <p>
   * If the resulting hour is lesser than 0 or greater than 23, the hour field <b>rolls</b>.
   * For instance, 24 becomes 0 and -1 becomes 23.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param nanos the nanos to add, may be negative
   * @return a { @code LocalTime } based on this time with the nanoseconds added, never null
   */
  def plusNanos(nanos: Long): LocalTime = {
    if (nanos == 0) return this
    var nofd: Long = toNanoOfDay
    var newNofd: Long = ((nanos % NANOS_PER_DAY) + nofd + NANOS_PER_DAY) % NANOS_PER_DAY
    if (nofd == newNofd) return this
    val newHour: Int = (newNofd / NANOS_PER_HOUR).asInstanceOf[Int]
    val newMinute: Int = ((newNofd / NANOS_PER_MINUTE) % MINUTES_PER_HOUR).toInt
    val newSecond: Int = ((newNofd / NANOS_PER_SECOND) % SECONDS_PER_MINUTE).toInt
    val newNano: Int = (newNofd % NANOS_PER_SECOND).toInt
    return create(newHour, newMinute, newSecond, newNano)
  }

  /**
   * Outputs this time as a    { @code String }, such as    { @code 10 :15 }.
   * <p>
   * The output will be one of the following formats:
   * <ul>
   * <li>   { @code HH :mm } </li>
   * <li>   { @code HH :mm:ss } </li>
   * <li>   { @code HH :mm:ssfnnn } </li>
   * <li>   { @code HH :mm:ssfnnnnnn } </li>
   * <li>   { @code HH :mm:ssfnnnnnnnnn } </li>
   * </ul>
   * The format used will be the shortest that outputs the full value of
   * the time where the omitted parts are implied to be zero.
   *
   * @return the formatted time, never null
   */
  override def toString: String = {
    val buf: StringBuilder = new StringBuilder(18)
    val hourValue: Int = hour
    val minuteValue: Int = minute
    val secondValue: Int = second
    val nanoValue: Int = nano
    buf.append(if (hourValue < 10) "0" else "").append(hourValue).append(if (minuteValue < 10) ":0" else ":").append(minuteValue)
    if (secondValue > 0 || nanoValue > 0) {
      buf.append(if (secondValue < 10) ":0" else ":").append(secondValue)
      if (nanoValue > 0) {
        buf.append('.')
        if (nanoValue % 1000000 == 0) {
          buf.append(Integer.toString((nanoValue / 1000000) + 1000).substring(1))
        }
        else if (nanoValue % 1000 == 0) {
          buf.append(Integer.toString((nanoValue / 1000) + 1000000).substring(1))
        }
        else {
          buf.append(Integer.toString((nanoValue) + 1000000000).substring(1))
        }
      }
    }
    return buf.toString
  }

  /**
   * Returns a copy of this    { @code LocalTime } with the specified period added,
   * returning the new time with any overflow in days.
   * <p>
   * This method returns an    { @link Overflow } instance with the result of the
   * addition and any overflow in days.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hours the hours to add, may be negative
   * @param minutes the minutes to add, may be negative
   * @param seconds the seconds to add, may be negative
   * @param nanos the nanos to add, may be negative
   * @return an { @code Overflow } instance with the resulting time and overflow, never null
   */
  private def plusWithOverflow(hours: Int, minutes: Int, seconds: Int, nanos: Long, sign: Int): LocalTime.Overflow = {
    var totDays: Int = (nanos / NANOS_PER_DAY).asInstanceOf[Int] + seconds / SECONDS_PER_DAY + minutes / MINUTES_PER_DAY + hours / HOURS_PER_DAY
    totDays *= sign
    var totNanos: Long = nanos % NANOS_PER_DAY + (seconds % SECONDS_PER_DAY) * NANOS_PER_SECOND + (minutes % MINUTES_PER_DAY) * NANOS_PER_MINUTE + (hours % HOURS_PER_DAY) * NANOS_PER_HOUR
    if (totNanos == 0) {
      return new LocalTime.Overflow(this, totDays)
    }
    var thisNanos: Long = toNanoOfDay
    totNanos = totNanos * sign + thisNanos
    totDays += MathUtils.floorDiv(totNanos, NANOS_PER_DAY).asInstanceOf[Int]
    totNanos = MathUtils.floorMod(totNanos, NANOS_PER_DAY)
    val newTime: LocalTime = (if (totNanos == thisNanos) this else ofNanoOfDay(totNanos))
    return new LocalTime.Overflow(newTime, totDays)
  }

  /**
   * Returns an offset time formed from this time and the specified offset.
   * <p>
   * This merges the two objects -    { @code this } and the specified offset -
   * to form an instance of    { @code OffsetTime }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param offset the offset to use, not null
   * @return the offset time formed from this time and the specified offset, never null
   */
  def atOffset(offset: ZoneOffset): OffsetTime = OffsetTime.of(this, offset)

  /**
   * Returns a copy of this    { @code LocalTime } with the specified period added,
   * returning the new time with any overflow in days.
   * <p>
   * This method returns an    { @link Overflow } instance with the result of the
   * addition and any overflow in days.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hours the hours to add, may be negative
   * @param minutes the minutes to add, may be negative
   * @param seconds the seconds to add, may be negative
   * @param nanos the nanos to add, may be negative
   * @return an { @code Overflow } instance with the resulting time and overflow, never null
   */
  def plusWithOverflow(hours: Int, minutes: Int, seconds: Int, nanos: Long): LocalTime.Overflow = {
    plusWithOverflow(hours, minutes, seconds, nanos, 1)
  }

  /**
   * Returns a copy of this    { @code LocalTime } with the specified period subtracted.
   * <p>
   * This subtracts the specified period from this time, returning a new time.
   * Before subtraction, the period is converted to a time-based    { @code Period } using
   * { @link Period # ofTimeFields ( PeriodProvider ) }.
   * That factory ignores any date-based ISO fields, thus subtracting a date-based
   * period from this time will have no effect.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to subtract, not null
   * @return a { @code LocalTime } based on this time with the period subtracted, never null
   * @throws CalendricalException if the specified period cannot be converted to a    { @code Period }
   * @throws ArithmeticException if the period overflows during conversion to hours/minutes/seconds/nanos
   */
  def minus(periodProvider: PeriodProvider): LocalTime = {
    val period: Period = Period.ofTimeFields(periodProvider).normalizedWith24HourDays
    val periodHours: Long = period.getHours
    val periodMinutes: Long = period.getMinutes
    val periodSeconds: Long = period.getSeconds
    val periodNanos: Long = period.getNanos
    val totNanos: Long = periodNanos % NANOS_PER_DAY + (periodSeconds % SECONDS_PER_DAY) * NANOS_PER_SECOND + (periodMinutes % MINUTES_PER_DAY) * NANOS_PER_MINUTE + (periodHours % HOURS_PER_DAY) * NANOS_PER_HOUR
    minusNanos(totNanos)
  }

  /**
   * Handle singletons on deserialization.
   * @return the resolved object.
   */
  private def readResolve: AnyRef = create(hour, minute, second, nano)

  /**
   * Returns a copy of this    { @code LocalTime } with the specified period in hours subtracted.
   * <p>
   * If the resulting hour is lesser than 0 or greater than 23, the field <b>rolls</b>.
   * For instance, 24 becomes 0 and -1 becomes 23.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hours the hours to subtract, may be negative
   * @return a { @code LocalTime } based on this time with the hours subtracted, never null
   */
  def minusHours(hours: Long): LocalTime = {
    if (hours == 0) this
    else {
    val newHour: Int = (-(hours % HOURS_PER_DAY).asInstanceOf[Int] + hour + HOURS_PER_DAY) % HOURS_PER_DAY
    create(newHour, minute, second, nano)
    }
  }

  /**
   * Compares this    { @code LocalTime } to another time.
   * <p>
   * The comparison is based on the time-line position of the times within a day.
   *
   * @param other the other time to compare to, not null
   * @return the comparator value, negative if less, positive if greater
   * @throws NullPointerException if    { @code other } is null
   */
  def compareTo(other: LocalTime): Int = {
    var cmp: Int = MathUtils.safeCompare(hour, other.hour)
    if (cmp == 0) {
      cmp = MathUtils.safeCompare(minute, other.minute)
      if (cmp == 0) {
        cmp = MathUtils.safeCompare(second, other.second)
        if (cmp == 0) {
          cmp = MathUtils.safeCompare(nano, other.nano)
        }
      }
    }
    return cmp
  }

  /**
   * Returns a copy of this    { @code LocalTime } with the specified period subtracted,
   * returning the new time with any overflow in days.
   * <p>
   * This method returns an    { @link Overflow } instance with the result of the
   * subtraction and any overflow in days.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hours the hours to subtract, may be negative
   * @param minutes the minutes to subtract, may be negative
   * @param seconds the seconds to subtract, may be negative
   * @param nanos the nanos to subtract, may be negative
   * @return an { @code Overflow } instance with the resulting time and overflow, never null
   */
  def minusWithOverflow(hours: Int, minutes: Int, seconds: Int, nanos: Long): LocalTime.Overflow = {
    plusWithOverflow(hours, minutes, seconds, nanos, -1)
  }

  /**
   * Checks whether this time matches the specified matcher.
   * <p>
   * Matchers can be used to query the time.
   * A simple matcher might simply compare one of the fields, such as the hour field.
   * A more complex matcher might check if the time is the last second of the day.
   *
   * @param matcher the matcher to use, not null
   * @return true if this time matches the matcher, false otherwise
   */
  def matches(matcher: CalendricalMatcher): Boolean = matcher.matchesCalendrical(this)

  /**
   * Returns a copy of this    { @code LocalTime } with the specified period added.
   * <p>
   * This adds the specified period to this time, returning a new time.
   * Before addition, the period is converted to a time-based    { @code Period } using
   * { @link Period # ofTimeFields ( PeriodProvider ) }.
   * That factory ignores any date-based ISO fields, thus adding a date-based
   * period to this time will have no effect.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to add, not null
   * @return a { @code LocalTime } based on this time with the period added, never null
   * @throws CalendricalException if the specified period cannot be converted to a    { @code Period }
   * @throws ArithmeticException if the period overflows during conversion to hours/minutes/seconds/nanos
   */
  def plus(periodProvider: PeriodProvider): LocalTime = {
    val period: Period = Period.ofTimeFields(periodProvider).normalizedWith24HourDays
    val periodHours: Long = period.getHours
    val periodMinutes: Long = period.getMinutes
    val periodSeconds: Long = period.getSeconds
    val periodNanos: Long = period.getNanos
    val totNanos: Long = periodNanos % NANOS_PER_DAY + (periodSeconds % SECONDS_PER_DAY) * NANOS_PER_SECOND + (periodMinutes % MINUTES_PER_DAY) * NANOS_PER_MINUTE + (periodHours % HOURS_PER_DAY) * NANOS_PER_HOUR
    plusNanos(totNanos)
  }

  /**
   * Adjusts a time to have the value of this time.
   *
   * @param time the time to be adjusted, not null
   * @return the adjusted time, never null
   */
  override def adjustTime(time: LocalTime): LocalTime = {
    ISOChronology.checkNotNull(time, "LocalTime must not be null")
    if (this.equals(time)) time else this
  }

  /**
   * Extracts the time as seconds of day,
   * from    { @code 0 } to    { @code 24 * 60 * 60 - 1 }.
   *
   * @return the second-of-day equivalent to this time
   */
  def toSecondOfDay: Int = {
    var total: Int = hour * SECONDS_PER_HOUR
    total += minute * SECONDS_PER_MINUTE
    total += second
    total
  }

  /**
   * A hash code for this    { @code LocalTime }.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = {
    val nod: Long = toNanoOfDay
    (nod ^ (nod >>> 32)).toInt
  }

  /**
   * Returns a copy of this    { @code LocalTime } with the specified period in hours added.
   * <p>
   * If the resulting hour is lesser than 0 or greater than 23, the field <b>rolls</b>.
   * For instance, 24 becomes 0 and -1 becomes 23.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hours the hours to add, may be negative
   * @return a { @code LocalTime } based on this time with the hours added, never null
   */
  def plusHours(hours: Long): LocalTime = {
    if (hours == 0) this
    else {
      val newHour: Int = ((hours % HOURS_PER_DAY).asInstanceOf[Int] + hour + HOURS_PER_DAY) % HOURS_PER_DAY
      create(newHour, minute, second, nano)
    }
  }

  /**
   * Returns a copy of this    { @code LocalTime } with the second-of-minute value altered.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @return a { @code LocalTime } based on this time with the requested second, never null
   * @throws IllegalCalendarFieldValueException if the second value is invalid
   */
  def withSecondOfMinute(secondOfMinute: Int): LocalTime = {
    if (secondOfMinute == second) this
    else {ISOChronology.secondOfMinuteRule.checkValue(secondOfMinute)
    create(hour, minute, secondOfMinute, nano)}
  }

  /**
   * Returns a copy of this    { @code LocalTime } with the specified period in seconds subtracted.
   * <p>
   * If the resulting hour is lesser than 0 or greater than 23, the hour field <b>rolls</b>.
   * For instance, 24 becomes 0 and -1 becomes 23.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param seconds the seconds to subtract, may be negative
   * @return a { @code LocalTime } based on this time with the seconds subtracted, never null
   */
  def minusSeconds(seconds: Long): LocalTime = {
    if (seconds == 0) return this
    var sofd: Int = hour * SECONDS_PER_HOUR + minute * SECONDS_PER_MINUTE + second
    var newSofd: Int = (-(seconds % SECONDS_PER_DAY).toInt + sofd + SECONDS_PER_DAY) % SECONDS_PER_DAY
    if (sofd == newSofd) return this
    val newHour: Int = newSofd / SECONDS_PER_HOUR
    val newMinute: Int = (newSofd / SECONDS_PER_MINUTE) % MINUTES_PER_HOUR
    val newSecond: Int = newSofd % SECONDS_PER_MINUTE
    return create(newHour, newMinute, newSecond, nano)
  }

  /**
   * Returns a copy of this    { @code LocalTime } with the specified period in minutes added.
   * <p>
   * If the resulting hour is lesser than 0 or greater than 23, the hour field <b>rolls</b>.
   * For instance, 24 becomes 0 and -1 becomes 23.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param minutes the minutes to add, may be negative
   * @return a { @code LocalTime } based on this time with the minutes added, never null
   */
  def plusMinutes(minutes: Long): LocalTime = {
    if (minutes == 0) return this
    var mofd: Int = hour * MINUTES_PER_HOUR + minute
    var newMofd: Int = ((minutes % MINUTES_PER_DAY).toInt + mofd + MINUTES_PER_DAY) % MINUTES_PER_DAY
    if (mofd == newMofd) return this
    val newHour: Int = newMofd / MINUTES_PER_HOUR
    val newMinute: Int = newMofd % MINUTES_PER_HOUR
    return create(newHour, newMinute, second, nano)
  }

  /**
   * Gets the hour-of-day field.
   *
   * @return the hour-of-day, from 0 to 23
   */
  def getHourOfDay: Int = hour

  /**
   * Checks if this    { @code LocalTime } is equal to the specified time.
   * <p>
   * The comparison is based on the time-line position of the time within a day.
   *
   * @param other the other time to compare to, null returns false
   * @return true if this point is equal to the specified time
   */
  override def equals(other: AnyRef): Boolean = {
    if (this == other) true
    else if (other.isInstanceOf[LocalTime]) {
      val localTime: LocalTime = other.asInstanceOf[LocalTime]
      hour == localTime.hour && minute == localTime.minute && second == localTime.second && nano == localTime.nano
    }
    else false
  }

  /**
   * Gets the value of the specified calendrical rule.
   * <p>
   * This method queries the value of the specified calendrical rule.
   * If the value cannot be returned for the rule from this time then
   * { @code null } will be returned.
   *
   * @param rule the rule to use, not null
   * @return the value for the rule, null if the value cannot be returned
   */
  def get[T](rule: CalendricalRule[T]): T = rule.deriveValueFor(rule, this, this)

  /**
   * Outputs this time as a    { @code String } using the formatter.
   *
   * @param formatter the formatter to use, not null
   * @return the formatted time string, never null
   * @throws UnsupportedOperationException if the formatter cannot print
   * @throws CalendricalPrintException if an error occurs during printing
   */
  def toString(formatter: DateTimeFormatter): String = {
    ISOChronology.checkNotNull(formatter, "DateTimeFormatter must not be null")
    formatter.print(this)
  }

  /**
   * Returns a copy of this    { @code LocalTime } with the specified period in minutes subtracted.
   * <p>
   * If the resulting hour is lesser than 0 or greater than 23, the hour field <b>rolls</b>.
   * For instance, 24 becomes 0 and -1 becomes 23.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param minutes the minutes to subtract, may be negative
   * @return a { @code LocalTime } based on this time with the minutes subtracted, never null
   */
  def minusMinutes(minutes: Long): LocalTime = {
    if (minutes == 0) return this
    var mofd: Int = hour * MINUTES_PER_HOUR + minute
    var newMofd: Int = (-(minutes % MINUTES_PER_DAY).toInt + mofd + MINUTES_PER_DAY) % MINUTES_PER_DAY
    if (mofd == newMofd) return this
    val newHour: Int = newMofd / MINUTES_PER_HOUR
    val newMinute: Int = newMofd % MINUTES_PER_HOUR
    return create(newHour, newMinute, second, nano)
  }

  /**
   * Gets the second-of-minute field.
   *
   * @return the second-of-minute, from 0 to 59
   */
  def getSecondOfMinute: Int = second

  /**
   * Checks if this    { @code LocalTime } is after the specified time.
   * <p>
   * The comparison is based on the time-line position of the time within a day.
   *
   * @param other the other time to compare to, not null
   * @return true if this is after the specified time
   * @throws NullPointerException if    { @code other } is null
   */
  def isAfter(other: LocalTime): Boolean = compareTo(other) > 0

  /**
   * Gets the minute-of-hour field.
   *
   * @return the minute-of-hour, from 0 to 59
   */
  def getMinuteOfHour: Int = minute

  /**
   * Returns a copy of this    { @code LocalTime } with the nano-of-second value altered.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param nanoOfSecond the nano-of-second to represent, from 0 to 999,999,999
   * @return a { @code LocalTime } based on this time with the requested nanosecond, never null
   * @throws IllegalCalendarFieldValueException if the nanos value is invalid
   */
  def withNanoOfSecond(nanoOfSecond: Int): LocalTime = {
    if (nanoOfSecond == nano) return this
    ISOChronology.nanoOfSecondRule.checkValue(nanoOfSecond)
    return create(hour, minute, second, nanoOfSecond)
  }

  /**
   * Gets the chronology that this time uses, which is the ISO calendar system.
   *
   * @return the ISO chronology, never null
   */
  def getChronology: ISOChronology = ISOChronology

  /**
   * Gets the nano-of-second field.
   *
   * @return the nano-of-second, from 0 to 999,999,999
   */
  def getNanoOfSecond: Int = nano

  /**
   * Converts this time to a    { @code LocalTime }, trivially
   * returning    { @code this }.
   *
   * @return { @code this }, never null
   */
  override def toLocalTime: LocalTime = this

  /**
   * Returns a copy of this    { @code LocalTime } with the hour-of-day value altered.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay the hour-of-day to represent, from 0 to 23
   * @return a { @code LocalTime } based on this time with the requested hour, never null
   * @throws IllegalCalendarFieldValueException if the hour value is invalid
   */
  def withHourOfDay(hourOfDay: Int): LocalTime = {
    if (hourOfDay == hour) this
    else {
    ISOChronology.hourOfDayRule.checkValue(hourOfDay)
    create(hourOfDay, minute, second, nano)
    }
  }

  /**
   * Returns a copy of this    { @code LocalTime } with the minute-of-hour value altered.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param minuteOfHour the minute-of-hour to represent, from 0 to 59
   * @return a { @code LocalTime } based on this time with the requested minute, never null
   * @throws IllegalCalendarFieldValueException if the minute value is invalid
   */
  def withMinuteOfHour(minuteOfHour: Int): LocalTime = {
    if (minuteOfHour == minute) this
    else {
    ISOChronology.minuteOfHourRule.checkValue(minuteOfHour)
    create(hour, minuteOfHour, second, nano)
    }
  }

  /**
   * Checks if the time extracted from the calendrical matches this.
   *
   * @param calendrical the calendrical to match, not null
   * @return true if the calendrical matches, false otherwise
   */
  override def matchesCalendrical(calendrical: Calendrical): Boolean = this.equals(calendrical.get(rule))

  /**
   * Extracts the time as nanos of day,
   * from    { @code 0 } to    { @code 24 * 60 * 60 * 1,000,000,000 - 1 }.
   *
   * @return the nano of day equivalent to this time
   */
  def toNanoOfDay: Long = {
    var total: Long = hour * NANOS_PER_HOUR
    total += minute * NANOS_PER_MINUTE
    total += second * NANOS_PER_SECOND
    total += nano
    total
  }

  /**
   * Returns a copy of this    { @code LocalTime } with the specified period in seconds added.
   * <p>
   * If the resulting hour is lesser than 0 or greater than 23, the hour field <b>rolls</b>.
   * For instance, 24 becomes 0 and -1 becomes 23.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param seconds the seconds to add, may be negative
   * @return a { @code LocalTime } based on this time with the seconds added, never null
   */
  def plusSeconds(seconds: Long): LocalTime = {
    if (seconds == 0) return this
    var sofd: Int = hour * SECONDS_PER_HOUR + minute * SECONDS_PER_MINUTE + second
    var newSofd: Int = ((seconds % SECONDS_PER_DAY).toInt + sofd + SECONDS_PER_DAY) % SECONDS_PER_DAY
    if (sofd == newSofd) return this
    val newHour: Int = newSofd / SECONDS_PER_HOUR
    val newMinute: Int = (newSofd / SECONDS_PER_MINUTE) % MINUTES_PER_HOUR
    val newSecond: Int = newSofd % SECONDS_PER_MINUTE
    return create(newHour, newMinute, newSecond, nano)
  }

  /**
   * Returns a copy of this    { @code LocalTime } with the specified period in nanoseconds subtracted.
   * <p>
   * If the resulting hour is lesser than 0 or greater than 23, the hour field <b>rolls</b>.
   * For instance, 24 becomes 0 and -1 becomes 23.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param nanos the nanos to subtract, may be negative
   * @return a { @code LocalTime } based on this time with the nanoseconds subtracted, never null
   */
  def minusNanos(nanos: Long): LocalTime = {
    if (nanos == 0) return this
    var nofd: Long = toNanoOfDay
    var newNofd: Long = (-(nanos % NANOS_PER_DAY) + nofd + NANOS_PER_DAY) % NANOS_PER_DAY
    if (nofd == newNofd) return this
    val newHour: Int = (newNofd / NANOS_PER_HOUR).toInt
    val newMinute: Int = ((newNofd / NANOS_PER_MINUTE) % MINUTES_PER_HOUR).toInt
    val newSecond: Int = ((newNofd / NANOS_PER_SECOND) % SECONDS_PER_MINUTE).toInt
    val newNano: Int = (newNofd % NANOS_PER_SECOND).toInt
    return create(newHour, newMinute, newSecond, newNano)
  }
}