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
package javax.time

import java.io.Serializable
import javax.time.calendar.LocalDate
import javax.time.UTCRules._

/**
 * An instantaneous point on the time-line measured in the UTC time-scale,
 * handling leap seconds.
 * <p>
 * Most of the Time Framework for Java works on the assumption that the time-line is
 * simple, there are no leap-seconds and there are always 24 * 60 * 60 seconds in a day.
 * However, the Earth's rotation is not straightforward, and a solar day does not match
 * this definition.
 * <p>
 * This class is an alternative representation based on the UTC time-scale which
 * includes leap-seconds. Leap-seconds are additional seconds that are inserted into the
 * year-month-day-hour-minute-second time-line in order to keep UTC in line with the solar day.
 * When a leap second occurs, an accurate clock will show the time {@code 23 :59:60} just before midnight.
 * <p>
 * Leap-seconds are announced in advance, typically at least six months.
 * The {@link UTCRules } class models which dates have leap-seconds.
 * Alternative implementations of the rules may be supplied.
 * <p>
 * The default rules implementation fixes the start point of UTC as 1972.
 * This date was chosen as UTC was more complex before 1972.
 * <p>
 * The duration between two points on the UTC time-scale is calculated solely using this class.
 * Do not use the {@code between} method on {@code Duration} as that will lose information.
 * Instead use {@link #durationUntil(UTCInstant)} on this class.
 * <p>
 * It is intended that most applications will use the {@code Instant} class
 * which uses the UTC-SLS mapping from UTC to guarantee 86400 seconds per day.
 * Specialist applications with access to an accurate time-source may find this class useful.
 *
 * <h4>Time-scale</h4>
 * <p>
 * The length of the solar day is the standard way that humans measure time.
 * As the Earth's rotation changes, the length of the day varies.
 * In general, a solar day is slightly longer than 86400 seconds.
 * The actual length is not predictable and can only be determined by measurement.
 * The UT1 time-scale captures these measurements.
 * <p>
 * The UTC time-scale is a standard approach to bundle up all the additional fractions
 * of a second from UT1 into whole seconds, known as <i>leap-seconds</i>.
 * A leap-second may be added or removed depending on the Earth's rotational changes.
 * If it is removed, then the relevant date will have no time of 23:59:59.
 * If it is added, then the relevant date will have an extra second of 23:59:60.
 * <p>
 * The modern UTC time-scale was introduced in 1972, introducing the concept of whole leap-seconds.
 * Between 1958 and 1972, the definition of UTC was complex, with minor sub-second leaps and
 * alterations to the length of seconds. The default rules only implement UTC from 1972.
 * Prior to that date, the default rules fix the UTC-TAI offset at 10 seconds.
 * While not historically accurate, it is a simple, easy definition, suitable for this library.
 * <p>
 * The standard Java epoch of 1970-01-01 is prior to the introduction of whole leap-seconds into UTC in 1972.
 * As such, the Time Framework for Java needs to define what the 1970 epoch actually means.
 * The chosen definition follows the UTC definition given above, such that 1970-01-01 is 10 seconds
 * offset from TAI.
 * <p>
 * UTCInstant is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object UTCInstant {

  /**
   * Constant for seconds per day.
   */
  private val SecondsPerDay: Long = 24 * 60 * 60
  /**
   * Constant for nanos per second.
   */
  private val NanosPerSecond: Long = 1000000000

  /**
   * Obtains an instance of {@code UTCInstant } from a TAI instant
   * using the system default leap second rules.
   * <p>
   * This method converts from the TAI to the UTC time-scale using the
   * system default leap-second rules. This conversion does not lose information
   * and the UTC instant may safely be converted back to a {@code TAIInstant }.
   *
   * @param taiInstant the TAI instant to convert, not null
   * @return the UTC instant, never null
   */
  def of(taiInstant: TAIInstant): UTCInstant = of(taiInstant, UTCRules.system)

  /**
   * Obtains an instance of {@code UTCInstant } from a TAI instant
   * using the specified leap second rules.
   * <p>
   * This method converts from the TAI to the UTC time-scale using the
   * specified leap-second rules. This conversion does not lose information
   * and the UTC instant may safely be converted back to a {@code TAIInstant }.
   *
   * @param taiInstant the TAI instant to convert, not null
   * @param rules the leap second rules, not null
   * @return the UTC instant, never null
   */
  def of(taiInstant: TAIInstant, rules: UTCRules): UTCInstant = rules.convertToUTC(taiInstant)

  /**
   * Obtains an instance of {@code UTCInstant } from a Modified Julian Day with
   * a nanosecond fraction of second using the specified leap second rules.
   * <p>
   * This factory creates an instance of a UTC instant.
   * The nanosecond of day value includes any leap second and has a valid range from
   * { @code 0 } to {@code 86, 400, 000, 000, 000 - 1 } on days other than leap-second-days
   * and other lengths on leap-second-days.
   * <p>
   * The nanosecond value must be positive even for negative values of Modified
   * Julian Days. One nanosecond before Modified Julian Day zero will be
   * { @code -1 } days and the maximum nanosecond value.
   *
   * @param mjDay the date as a Modified Julian Day (number of days from the epoch of 1858-11-17)
   * @param nanoOfDay the nanoseconds within the day, including leap seconds
   * @return the UTC instant, never null
   * @throws IllegalArgumentException if nanoOfDay is out of range
   */
  def ofModifiedJulianDays(mjDay: Long, nanoOfDay: Long, rules: UTCRules): UTCInstant = {
    Instant.checkNotNull(rules, "LeapSecondRules must not be null")
    val leapSecs: Long = rules.getLeapSecondAdjustment(mjDay)
    val maxNanos: Long = (SecondsPerDay + leapSecs) * NanosPerSecond
    if (nanoOfDay < 0 || nanoOfDay >= maxNanos) {
      throw new IllegalArgumentException("Nanosecond-of-day must be between 0 and " + maxNanos + " on date " + mjDay)
    }
    return new UTCInstant(mjDay, nanoOfDay, rules)
  }

  /**
   * Obtains an instance of {@code UTCInstant } from a provider of instants
   * using the system default leap second rules.
   * <p>
   * This method converts from the UTC-SLS to the UTC time-scale using the
   * system default leap-second rules. This conversion will lose information
   * around a leap second in accordance with UTC-SLS.
   * Converting back to an {@code Instant } may result in a slightly different instant.
   *
   * @param instant the instant to convert, not null
   * @return the UTC instant, never null
   */
  def of(instant: Instant): UTCInstant = of(instant, UTCRules.system)

  /**
   * Obtains an instance of {@code UTCInstant } from a provider of instants
   * using the specified leap second rules.
   * <p>
   * This method converts from the UTC-SLS to the UTC time-scale using the
   * specified leap-second rules. This conversion will lose information
   * around a leap second in accordance with UTC-SLS.
   * Converting back to an {@code Instant } may result in a slightly different instant.
   *
   * @param instant the instant to convert, not null
   * @param rules the leap second rules, not null
   * @return the UTC instant, never null
   */
  def of(instant: Instant, rules: UTCRules): UTCInstant = rules.convertToUTC(instant)

  /**
   * Obtains an instance of {@code UTCInstant } from a Modified Julian Day with
   * a nanosecond fraction of second using the system default leap second rules.
   * <p>
   * This factory creates an instance of a UTC instant.
   * The nanosecond of day value includes any leap second and has a valid range from
   * { @code 0 } to {@code 86, 400, 000, 000, 000 - 1 } on days other than leap-second-days
   * and other lengths on leap-second-days.
   * <p>
   * The nanosecond value must be positive even for negative values of Modified
   * Julian Days. One nanosecond before Modified Julian Day zero will be
   * { @code -1 } days and the maximum nanosecond value.
   *
   * @param mjDay the date as a Modified Julian Day (number of days from the epoch of 1858-11-17)
   * @param nanoOfDay the nanoseconds within the day, including leap seconds
   * @return the UTC instant, never null
   * @throws IllegalArgumentException if nanoOfDay is out of range
   */
  def ofModifiedJulianDays(mjDay: Long, nanoOfDay: Long): UTCInstant =
    ofModifiedJulianDays(mjDay, nanoOfDay, UTCRules.system)
}


/**
 * Constructs an instance.
 *
 * @param mjDay the date as a Modified Julian Day (number of days from the epoch of 1858-11-17)
 * @param nanoOfDay the nanoseconds within the day, including leap seconds
 * @param rules the leap second rules, not null
 */

@SerialVersionUID(1L)
final case class UTCInstant(mjDay: Long, nanos: Long, rules: UTCRules) extends Comparable[UTCInstant] with Serializable {

  /**
   * Compares this instant to another based on the time-line, then the name
   * of the rules.
   * <p>
   * The comparison is based on the positions on the time-line and the rules.
   * This definition means that two instants representing the same instant on
   * the time-line will differ if the rules differ. To compare the time-line
   * instant, convert both instants to a {@code TAIInstant }.
   *
   * @param otherInstant the other instant to compare to, not null
   * @return the comparator value, negative if less, positive if greater
   */
  def compareTo(otherInstant: UTCInstant): Int = {
    val cmp: Int = MathUtils.safeCompare(mjDay, otherInstant.mjDay)
    if (cmp != 0) cmp
    else MathUtils.safeCompare(nanos, otherInstant.nanos)
  }

  /**
   * A string representation of this instant.
   * <p>
   * The string is formatted using ISO-8601.
   *
   * @return a representation of this instant, never null
   */
  override def toString: String = {
    val date: LocalDate = LocalDate.ofModifiedJulianDays(mjDay)
    val buf: StringBuilder = new StringBuilder(18)
    val sod: Int = (nanos / NanosPerSecond).toInt
    var hourValue: Int = sod / (60 * 60)
    var minuteValue: Int = (sod / 60) % 60
    var secondValue: Int = sod % 60
    if (hourValue == 24) {
      hourValue = 23
      minuteValue = 59
      secondValue += 60
    }
    val nanoValue: Int = (nanos % NanosPerSecond).toInt
    buf.append(date).append('T').append(if (hourValue < 10) "0" else "").append(hourValue).append(if (minuteValue < 10) ":0" else ":").append(minuteValue).append(if (secondValue < 10) ":0" else ":").append(secondValue)
    val pos: Int = buf.length
    buf.append(nanoValue + NanosPerSecond)
    buf.setCharAt(pos, '.')
    buf.append("(UTC)")
    return buf.toString
  }

  /**
   * Converts this instant to an {@code Instant } using the system default
   * leap second rules.
   * <p>
   * This method converts this instant from the UTC to the UTC-SLS time-scale using the
   * stored leap-second rules.
   * This conversion will lose information around a leap second in accordance with UTC-SLS.
   * Converting back to a {@code UTCInstant } may result in a slightly different instant.
   *
   * @return an { @code Instant } representing the best approximation of this instant, never null
   * @throws ArithmeticException if the calculation exceeds the supported range
   */
  def toInstant: Instant = rules.convertToInstant(this)

  /**
   * Gets the Modified Julian Day (MJD).
   * <p>
   * The Modified Julian Day count is a simple incrementing count of days
   * where day 0 is 1858-11-17.
   * The nanosecond part of the day is returned by {@code getNanosOfDay }.
   * <p>
   * A Modified Julian Day varies in length, being one second longer on a leap day.
   *
   * @return the Modified Julian Day based on the epoch 1858-11-17
   */
  def getModifiedJulianDays: Long = mjDay

  /**
   * Checks if this instant is equal to the specified {@code UTCInstant }.
   * <p>
   * The comparison is based on the positions on the time-line and the rules.
   * This definition means that two instants representing the same instant on
   * the time-line will differ if the rules differ. To compare the time-line
   * instant, convert both instants to a {@code TAIInstant }.
   *
   * @param otherInstant the other instant, null returns false
   * @return true if the other instant is equal to this one
   */
  // override def equals(otherInstant: AnyRef): Boolean

  /**
   * Returns a hash code for this instant.
   *
   * @return a suitable hash code
   */
  //override def hashCode: Int = ((mjDay ^ (mjDay >>> 32)).toInt) + 51 * ((nanos ^ (nanos >>> 32)).toInt) + rules.hashCode

  /**
   * Checks if the instant is within a leap second.
   * <p>
   * This method returns true when an accurate clock would return a seconds
   * field of 60.
   *
   * @return true if this instant is within a leap second
   */
  def isLeapSecond: Boolean = nanos > SecondsPerDay * NanosPerSecond

  /**
   * Returns the duration between this instant and the specified instant.
   * <p>
   * This calculates the duration between this instant and another based on
   * the UTC time-scale. Any leap seconds that occur will be included in the duration.
   * Adding the duration to this instant using {@link # plus } will always result
   * in an instant equal to the specified instant.
   *
   * @param utcInstant the instant to calculate the duration until, not null
   * @return the duration until the specified instant, may be negative, never null
   * @throws ArithmeticException if the calculation exceeds the supported range
   */
  def durationUntil(utcInstant: UTCInstant): Duration = {
    val thisTAI: TAIInstant = toTAIInstant
    val otherTAI: TAIInstant = utcInstant.toTAIInstant
    return thisTAI.durationUntil(otherTAI)
  }

  /**
   * Returns a copy of this instant with the specified duration added.
   * <p>
   * The duration is added using simple addition of the seconds and nanoseconds
   * in the duration to the seconds and nanoseconds of this instant.
   * As a result, the duration is treated as being measured in TAI compatible seconds
   * for the purpose of this method.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param duration the duration to add, not null
   * @return a { @code UTCInstant } with the duration added, never null
   * @throws ArithmeticException if the calculation exceeds the supported range
   */
  def plus(duration: Duration): UTCInstant = UTCInstant.of(toTAIInstant.plus(duration), rules)

  def +(duration: Duration): UTCInstant = plus(duration)

  /**
   * Returns a copy of this instant with the specified duration subtracted.
   * <p>
   * The duration is subtracted using simple subtraction of the seconds and nanoseconds
   * in the duration from the seconds and nanoseconds of this instant.
   * As a result, the duration is treated as being measured in TAI compatible seconds
   * for the purpose of this method.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param duration the duration to subtract, not null
   * @return a { @code UTCInstant } with the duration subtracted, never null
   * @throws ArithmeticException if the calculation exceeds the supported range
   */
  def minus(duration: Duration): UTCInstant = UTCInstant.of(toTAIInstant.minus(duration), rules)

  def -(duration: Duration): UTCInstant = minus(duration)

  /**
   * Converts this instant to a {@code TAIInstant } using the stored
   * leap second rules.
   * <p>
   * This method converts from the UTC to the TAI time-scale using the stored leap-second rules.
   * Conversion to a {@code TAIInstant } retains the same point on the time-line
   * but loses the stored rules. If the TAI instant is converted back to a UTC instant
   * with different or updated rules then the calculated UTC instant may be different.
   *
   * @return a { @code TAIInstant } representing the same instant, never null
   * @throws ArithmeticException if the calculation exceeds the supported range
   */
  def toTAIInstant: TAIInstant = rules.convertToTAI(this)

  /**
   * Gets the number of nanoseconds, later along the time-line, from the start
   * of the Modified Julian Day.
   * <p>
   * The nanosecond-of-day value measures the total number of nanoseconds from
   * the Modified Julian Day returned by {@code getModifiedJulianDay}.
   * This value will include any additional leap seconds.
   *
   * @return the nanoseconds within the day, including leap seconds
   */
  def getNanoOfDay: Long = nanos
}