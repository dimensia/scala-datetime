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
package javax.time

import java.io.Serializable
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeUnit._
import javax.time.calendar.format.CalendricalParseException

/**
 * A duration between two instants on the time-line.
 * <p>
 * The Time Framework for Java models time as a series of instantaneous events,
 * known as     { @link Instant instants }, along a single time-line.
 * This class represents the duration between two of those instants.
 * The model is of a directed duration, meaning that the duration may be negative.
 * <p>
 * A physical instant is an instantaneous event.
 * However, for practicality the API and this class uses a precision of nanoseconds.
 * <p>
 * A physical duration could be of infinite length.
 * However, for practicality the API and this class limits the length to the
 * number of seconds that can be held in a     { @code long }.
 * <p>
 * In order to represent the data a 96 bit number is required. To achieve this the
 * data is stored as seconds, measured using a     { @code long }, and nanoseconds,
 * measured using an     { @code int }. The nanosecond part will always be between
 * 0 and 999,999,999 representing the nanosecond part of the second.
 * For example, the negative duration of     { @code PT -0.1S } is represented as
 * -1 second and 900,000,000 nanoseconds.
 * <p>
 * In this API, the unit of "seconds" only has a precise meaning when applied to an instant.
 * This is because it is the instant that defines the time scale used, not the duration.
 * For example, the simplified UTC time scale used by     { @code Instant } ignores leap seconds,
 * which alters the effective length of a second. By comparison, the TAI time scale follows
 * the international scientific definition of a second exactly.
 * For most applications, this subtlety will be irrelevant.
 * <p>
 * Duration is immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object Duration {

  /**
   * Constant for maximum long.
   */
  private val BigIntLongMaxValue: BigInt = BigInt(Long.MaxValue)

  /**
   * Constant for minimum long.
   */
  private val BigIntLongMinValue: BigInt = BigInt(Long.MinValue)

  /**
   * Constant for nanos per microsecond.
   */
  private val BigIntNanosPerMicro: BigInt = BigInt(1000L)

  /**
   * Constant for nanos per millisecond.
   */

  private val BigIntNanosPerMilli: BigInt = BigInt(1000000L)
  /**
   * Constant for nanos per second.
   */

  private val BigIntNanosPerSecond: BigInt = Instant.Billion

  /**
   * Constant for nanos per minute.
   */
  private val BigIntNanosPerMinute: BigInt = BigInt(60L * 1000000000L)

  /**
   * Constant for nanos per hour.
   */
  private val BigIntNanosPerHour: BigInt = BigInt(60L * 60L * 1000000000L)

  /**
   * Constant for nanos per day.
   */
  private val BigIntNanosPerDay: BigInt = BigInt(24L * 60L * 60L * 1000000000L)

  /**
   * Constant for a duration of zero.
   */
  val Zero: Duration = new Duration(0, 0)

  /**
   * Constant for nanos per second.
   */
  private val NanosPerSecond: Int = 1000000000

  /**
   * Obtains an instance of     { @code Duration } from a number of milliseconds.
   * <p>
   * The seconds and nanoseconds are extracted from the specified milliseconds.
   *
   * @param millis the number of milliseconds, positive or negative
   * @return a { @code Duration }, never null
   */
  def ofMillis(millis: Long): Duration = {
    var secs: Long = millis / 1000
    var mos: Int = (millis % 1000).toInt
    if (mos < 0) {
      mos += 1000
      secs -= 1;
    }
    create(secs, mos * 1000000)
  }

  /**
   * Obtains an instance of     { @code Duration } from a number of seconds.
   * <p>
   * The nanosecond in second field is set to zero.
   *
   * @param seconds the number of seconds, positive or negative
   * @return a { @code Duration }, never null
   */
  def ofSeconds(seconds: Long): Duration = create(seconds, 0)

  /**
   * Obtains an instance of     { @code Duration } from a duration in a specified time unit.
   * <p>
   * The duration amount is measured in terms of the specified unit. For example:
   * <pre>
   *  Duration.of(3, TimeUnit.SECONDS);
   *  Duration.of(465, TimeUnit.MICROSECONDS);
   * </pre>
   *
   * @param amount the amount of the duration, positive or negative
   * @param unit the unit that the duration is measured in, not null
   * @return a { @code Duration }, never null
   * @throws ArithmeticException if the input amount exceeds the capacity of     { @code Duration }
   *  which can only occur for units MINUTES, HOURS and DAYS
   */
  def of(amount: Long, unit: TimeUnit): Duration = {
    Instant.checkNotNull(unit, "TimeUnit must not be null")
    val nanos: Long = unit.toNanos(amount)
    if (unit == TimeUnit.NANOSECONDS || (nanos > Long.MaxValue && nanos < Long.MinValue)) {
      return ofNanos(nanos)
    }
    val calc: BigInt = BigInt(amount)
    unit match {
      case MICROSECONDS =>
        return ofNanos(calc * BigIntNanosPerMicro)
      case MILLISECONDS =>
        return ofNanos(calc * BigIntNanosPerMilli)
      case SECONDS =>
        return ofNanos(calc * BigIntNanosPerSecond)
      case MINUTES =>
        return ofNanos(calc * BigIntNanosPerMinute)
      case HOURS =>
        return ofNanos(calc * BigIntNanosPerHour)
      case DAYS =>
        return ofNanos(calc * BigIntNanosPerDay)
      case _ =>
        throw new InternalError("Unreachable")
    }
  }

  /**
   * Obtains an instance of {@code Duration} from a number of standard length minutes.
   * <p>
   * The seconds are calculated based on the standard definition of a minute,
   * where each minute is 60 seconds.
   * The nanosecond in second field is set to zero.
   *
   * @param minutes the number of minutes, positive or negative
   * @return a { @code Duration }, never null
   * @throws ArithmeticException if the input minutes exceeds the capacity of     { @code Duration }
   */
  def ofStandardMinutes(minutes: Long): Duration = create(MathUtils.safeMultiply(minutes, 60), 0)

  /**
   * Obtains an instance of {@code Duration} from a number of standard length hours.
   * <p>
   * The seconds are calculated based on the standard definition of an hour,
   * where each hour is 3600 seconds.
   * The nanosecond in second field is set to zero.
   *
   * @param hours the number of hours, positive or negative
   * @return a { @code Duration }, never null
   * @throws ArithmeticException if the input hours exceeds the capacity of     { @code Duration }
   */
  def ofStandardHours(hours: Long): Duration = create(MathUtils.safeMultiply(hours, 3600), 0)

  /**
   * Obtains an instance of {@code Duration} using seconds and nanoseconds.
   *
   * @param seconds the length of the duration in seconds, positive or negative
   * @param nanoAdjustment the nanosecond adjustment within the second, from 0 to 999,999,999
   */
  private def create(seconds: Long, nanoAdjustment: Int): Duration = {
    if ((seconds | nanoAdjustment) == 0) Zero
    else new Duration(seconds, nanoAdjustment)
  }

  /**
   * Obtains an instance of {@code Duration} representing the duration between two instants.
   * <p>
   * A {@code Duration} represents a directed distance between two points on the time-line.
   * As such, this method will return a negative duration if the end is before the start.
   * To guarantee to obtain a positive duration call {@link# a b s ( )} on the result of this factory.
   *
   * @param startInclusive the start instant, inclusive, not null
   * @param endExclusive the end instant, exclusive, not null
   * @return a { @code Duration }, never null
   * @throws ArithmeticException if the calculation exceeds the capacity of     { @code Duration }
   */
  def between(startInclusive: InstantProvider, endExclusive: InstantProvider): Duration = {
    val start: Instant = Instant.of(startInclusive)
    val end: Instant = Instant.of(endExclusive)
    var secs: Long = MathUtils.safeSubtract(end.getEpochSeconds, start.getEpochSeconds)
    var nanos: Int = end.getNanoOfSecond - start.getNanoOfSecond
    if (nanos < 0) {
      nanos += NanosPerSecond
      secs = MathUtils.safeDecrement(secs)
    }
    return create(secs, nanos)
  }

  /**
   * Obtains an instance of     { @code Duration } from a number of standard length days.
   * <p>
   * The seconds are calculated based on the standard definition of a day,
   * where each day is 86400 seconds which implies a 24 hour day.
   * The nanosecond in second field is set to zero.
   *
   * @param days the number of days, positive or negative
   * @return a { @code Duration }, never null
   * @throws ArithmeticException if the input days exceeds the capacity of     { @code Duration }
   */
  def ofStandardDays(days: Long): Duration = create(MathUtils.safeMultiply(days, 86400), 0)

  /**
   * Obtains an instance of     { @code Duration } from a number of nanoseconds.
   * <p>
   * The seconds and nanoseconds are extracted from the specified     { @code BigInteger }.
   * If the resulting seconds value is larger than     { @code Long.MAX_VALUE } then an
   * exception is thrown.
   *
   * @param nanos the number of nanoseconds, positive or negative, not null
   * @return a { @code Duration }, never null
   * @throws ArithmeticException if the input nanoseconds exceeds the capacity of     { @code Duration }
   */
  def ofNanos(nanos: BigInt): Duration = {
    Instant.checkNotNull(nanos, "Nanos must not be null")
    val divRem = nanos /% BigIntNanosPerSecond
    if (divRem._1.bitLength > 63) {
      throw new ArithmeticException("Exceeds capacity of Duration: " + nanos)
    }
    return ofSeconds(divRem._1.longValue, divRem._2.intValue)
  }

  /**
   * Obtains an instance of     { @code Duration } from a number of seconds.
   * <p>
   * The seconds and nanoseconds are extracted from the specified     { @code BigDecimal }.
   * If the decimal is larger than     { @code Long.MAX_VALUE } or has more than 9 decimal
   * places then an exception is thrown.
   *
   * @param seconds the number of seconds, up to scale 9, positive or negative
   * @return a { @code Duration }, never null
   * @throws ArithmeticException if the input seconds exceeds the capacity of a     { @code Duration }
   */
  def ofSeconds(seconds: BigDecimal): Duration = {
    Instant.checkNotNull(seconds, "Seconds must not be null")
    //return ofNanos(seconds.movePointRight(9).toBigIntegerExact)
    ofNanos((seconds * 1000000000).toBigIntExact.get)
  }

  /**
   * Obtains an instance of     { @code Duration } from a number of nanoseconds.
   * <p>
   * The seconds and nanoseconds are extracted from the specified nanoseconds.
   *
   * @param nanos the number of nanoseconds, positive or negative
   * @return a { @code Duration }, never null
   */
  def ofNanos(nanos: Long): Duration = {
    var secs: Long = nanos / NanosPerSecond
    var nos: Int = (nanos % NanosPerSecond).toInt
    if (nos < 0) {
      nos += NanosPerSecond
      secs -= 1;
    }
    return create(secs, nos)
  }

  /**
   * Obtains an instance of     { @code Duration } by parsing a string.
   * <p>
   * This will parse the string produced by     { @link # toString ( ) } which is
   * the ISO-8601 format     { @code PTnS } where     { @code n } is
   * the number of seconds with optional decimal part.
   * The number must consist of ASCII numerals.
   * There must only be a negative sign at the start of the number and it can
   * only be present if the value is less than zero.
   * There must be at least one digit before any decimal point.
   * There must be between 1 and 9 inclusive digits after any decimal point.
   * The letters (P, T and S) will be accepted in upper or lower case.
   * The decimal point may be either a dot or a comma.
   *
   * @param text the text to parse, not null
   * @return a { @code Duration }, never null
   * @throws CalendricalParseException if the text cannot be parsed to a     { @code Duration }
   */
  def parse(text: String): Duration = {
    Instant.checkNotNull(text, "Text to parse must not be null")
    var len: Int = text.length
    if (len < 4 || (text.charAt(0) != 'P' && text.charAt(0) != 'p') || (text.charAt(1) != 'T' && text.charAt(1) != 't') || (text.charAt(len - 1) != 'S' && text.charAt(len - 1) != 's') || (len == 5 && text.charAt(2) == '-' && text.charAt(3) == '0')) {
      throw new CalendricalParseException("Duration could not be parsed: " + text, text, 0)
    }
    var numberText: String = text.substring(2, len - 1).replace(',', '.')
    var dot: Int = numberText.indexOf('.')
    try {
      if (dot == -1) {
        if (numberText.startsWith("-0")) {
          throw new CalendricalParseException("Duration could not be parsed: " + text, text, 2)
        }
        return create(numberText.toLong, 0)
      }
      var negative: Boolean = false
      if (numberText.charAt(0) == '-') {
        negative = true
      }
      val secs: Long = numberText.substring(0, dot).toLong
      numberText = numberText.substring(dot + 1)
      len = numberText.length
      if (len == 0 || len > 9 || numberText.charAt(0) == '-') {
        throw new CalendricalParseException("Duration could not be parsed: " + text, text, 2)
      }
      var nanos: Int = numberText.toInt
      len match {
        case 1 => nanos *= 100000000
        case 2 => nanos *= 10000000
        case 3 => nanos *= 1000000
        case 4 => nanos *= 100000
        case 5 => nanos *= 10000
        case 6 => nanos *= 1000
        case 7 => nanos *= 100
        case 8 => nanos *= 10
      }
      return if (negative) ofSeconds(secs, -nanos) else create(secs, nanos)
    }
    catch {
      case ex: ArithmeticException => {
        throw new CalendricalParseException("Duration could not be parsed: " + text, text, 2, ex)
      }
      case ex: NumberFormatException => {
        throw new CalendricalParseException("Duration could not be parsed: " + text, text, 2, ex)
      }
    }
  }

  /**
   * Obtains an instance of     { @code Duration } from a number of seconds
   * and an adjustment in nanoseconds.
   * <p>
   * This method allows an arbitrary number of nanoseconds to be passed in.
   * The factory will alter the values of the second and nanosecond in order
   * to ensure that the stored nanosecond is in the range 0 to 999,999,999.
   * For example, the following will result in the exactly the same duration:
   * <pre>
   *  Duration.ofSeconds(3, 1);
   *  Duration.ofSeconds(4, -999999999);
   *  Duration.ofSeconds(2, 1000000001);
   * </pre>
   *
   * @param seconds the number of seconds, positive or negative
   * @param nanoAdjustment the nanosecond adjustment to the number of seconds, positive or negative
   * @return a {@code Duration}, never null
   * @throws ArithmeticException if the adjustment causes the seconds to exceed the capacity of     { @code Duration }
   */
  def ofSeconds(seconds: Long, nanoAdjustment: Long): Duration = {
    var secs: Long = MathUtils.safeAdd(seconds, nanoAdjustment / NanosPerSecond)
    var nos: Int = (nanoAdjustment % NanosPerSecond).asInstanceOf[Int]
    if (nos < 0) {
      nos += NanosPerSecond
      secs = MathUtils.safeDecrement(secs)
    }
    return create(secs, nos)
  }
}

/**
 * Constructs an instance of     { @code Duration } using seconds and nanoseconds.
 *
 * @param seconds the length of the duration in seconds, positive or negative
 * @param nanos the nanoseconds within the second, from 0 to 999,999,999
 */
@SerialVersionUID(1L)
final case class Duration private(seconds: Long, nanos: Int) extends Comparable[Duration] with Serializable {

  import Duration._

  /**
   * Gets the number of seconds in this duration.
   * <p>
   * The length of the duration is stored using two fields - seconds and nanoseconds.
   * The nanoseconds part is a value from 0 to 999,999,999 that is an adjustment to
   * the length in seconds.
   * The total duration is defined by calling this method and  { @link # getNanoOfSecond ( ) }.
   * <p>
   * A  { @code Duration } represents a directed distance between two points on the time-line.
   * A negative duration is expressed by the negative sign of the seconds part.
   * A duration of -1 nanosecond is stored as -1 seconds plus 999,999,999 nanoseconds.
   *
   * @return the whole seconds part of the length of the duration, positive or negative
   */
  def getSeconds: Long = seconds

  /**
   * Gets the number of nanoseconds within the second in this duration.
   * <p>
   * The length of the duration is stored using two fields - seconds and nanoseconds.
   * The nanoseconds part is a value from 0 to 999,999,999 that is an adjustment to
   * the length in seconds.
   * The total duration is defined by calling this method and  { @link # getSeconds ( ) }.
   * <p>
   * A  { @code Duration } represents a directed distance between two points on the time-line.
   * A negative duration is expressed by the negative sign of the seconds part.
   * A duration of -1 nanosecond is stored as -1 seconds plus 999,999,999 nanoseconds.
   *
   * @return the nanoseconds within the second part of the length of the duration, from 0 to 999,999,999
   */
  def getNanoOfSecond: Int = nanos

  /**
   * Returns a copy of this duration with the specified number of seconds subtracted.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param secondsToSubtract the seconds to subtract, positive or negative
   * @return a { @code Duration } based on this duration with the specified seconds subtracted, never null
   * @throws ArithmeticException if the calculation exceeds the capacity of     { @code Duration }
   */
  def minusSeconds(secondsToSubtract: Long): Duration = {
    if (secondsToSubtract == 0) return this
    val secs: Long = MathUtils.safeSubtract(seconds, secondsToSubtract)
    return create(secs, nanos)
  }

  /**
   * Checks if this duration is positive or zero.
   * <p>
   * A     { @code Duration } represents a directed distance between two points on
   * the time-line and can therefore be positive, zero or negative.
   * This method checks whether the length is greater than or equal to zero.
   *
   * @return true if this duration has a total length greater than or equal zero
   */
  def isPositiveOrZero: Boolean = seconds >= 0

  /**
   * Returns a copy of this duration with the specified number of nanoseconds added.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param nanosToAdd the nanoseconds to add, positive or negative
   * @return a { @code Duration } based on this duration with the specified nanoseconds added, never null
   * @throws ArithmeticException if the calculation exceeds the capacity of     { @code Duration }
   */
  def plusNanos(nanosToAdd: Long): Duration = {
    if (nanosToAdd == 0) return this
    var secondsToAdd: Long = nanosToAdd / NanosPerSecond
    var nos: Int = (nanosToAdd % NanosPerSecond).asInstanceOf[Int]
    nos += nanos
    if (nos < 0) {
      nos += NanosPerSecond
      secondsToAdd -= 1;
    }
    else if (nos >= NanosPerSecond) {
      nos -= NanosPerSecond
      secondsToAdd += 1;
    }
    return create(MathUtils.safeAdd(seconds, secondsToAdd), nos)
  }

  /**
   * Gets the duration in terms of the specified unit.
   * <p>
   * This method returns the duration converted to the unit, truncating
   * excess precision.
   * If the conversion would overflow, the result will saturate to
   * { @code Long.MAX_VALUE } or     { @code Long.MIN_VALUE }.
   *
   * @return the duration in the specified unit, saturated at     { @code Long.MAX_VALUE }
   * and     { @code Long.MIN_VALUE }, positive or negative
   */
  def get(unit: TimeUnit): Long = {
    Instant.checkNotNull(unit, "TimeUnit must not be null")
    var nanos: BigInt = toNanos
    unit match {
      case NANOSECONDS =>
      case MICROSECONDS => nanos = nanos / BigIntNanosPerMicro
      case MILLISECONDS => nanos = nanos / BigIntNanosPerMilli
      case SECONDS => nanos = nanos / BigIntNanosPerSecond
      case MINUTES => nanos = nanos / BigIntNanosPerMinute
      case HOURS => nanos = nanos / BigIntNanosPerHour
      case DAYS => nanos = nanos / BigIntNanosPerDay
      case _ => throw new InternalError("Unreachable")
    }
    nanos.min(BigIntLongMaxValue).max(BigIntLongMinValue).toLong
  }

  /**
   * Returns a copy of this duration with the specified     { @code Duration } added.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param duration the duration to add, positive or negative, not null
   * @return a { @code Duration } based on this duration with the specified duration added, never null
   * @throws ArithmeticException if the calculation exceeds the capacity of     { @code Duration }
   */
  def plus(duration: Duration): Duration = {
    var secsToAdd: Long = duration.seconds
    var nanosToAdd: Int = duration.nanos
    if (secsToAdd == 0 && nanosToAdd == 0) return this
    var secs: Long = MathUtils.safeAdd(seconds, secsToAdd)
    var nos: Int = nanos + nanosToAdd
    if (nos >= NanosPerSecond) {
      nos -= NanosPerSecond
      secs = MathUtils.safeIncrement(secs)
    }
    return create(secs, nos)
  }

  def +(duration: Duration): Duration = plus(duration)

  /**
   * Returns a copy of this duration multiplied by the scalar.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param multiplicand the value to multiply the duration by, positive or negative
   * @return a { @code Duration } based on this duration multiplied by the specified scalar, never null
   * @throws ArithmeticException if the calculation exceeds the capacity of     { @code Duration }
   */
  def multipliedBy(multiplicand: Long): Duration = {
    if (multiplicand == 0) return Zero
    if (multiplicand == 1) return this
    var nanos: BigInt = toNanos
    nanos = nanos * BigInt(multiplicand)
    val divRem = nanos /% BigIntNanosPerSecond
    if (divRem._1.bitLength > 63) throw new ArithmeticException("Multiplication result exceeds capacity of Duration: " + this + " * " + multiplicand)
    return ofSeconds(divRem._1.toLong, divRem._2.toInt)
  }

  def *(multiplicand: Long): Duration = multipliedBy(multiplicand)

  /**
   * Resolves singletons.
   *
   * @return the resolved instance, never null
   */
  private def readResolve: AnyRef = if ((seconds | nanos) == 0) Zero else this

  /**
   * Checks if this duration is zero length.
   * <p>
   * A     { @code Duration } represents a directed distance between two points on
   * the time-line and can therefore be positive, zero or negative.
   * This method checks whether the length is zero.
   *
   * @return true if this duration has a total length equal to zero
   */
  def isZero: Boolean = (seconds | nanos) == 0

  /**
   * Converts this duration to the total length in seconds and
   * fractional nanoseconds expressed as a     { @code BigDecimal }.
   *
   * @return the total length of the duration in seconds, with a scale of 9, never null
   */
  def toSeconds: BigDecimal = BigDecimal(seconds) + BigDecimal(nanos, 9)

  /**
   * Returns a copy of this duration divided by the specified value.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param divisor the value to divide the duration by, positive or negative, not zero
   * @return a { @code Duration } based on this duration divided by the specified divisor, never null
   * @throws ArithmeticException if the divisor is zero
   * @throws ArithmeticException if the calculation exceeds the capacity of     { @code Duration }
   */
  def dividedBy(divisor: Long): Duration = {
    if (divisor == 0) throw new ArithmeticException("Cannot divide by zero")
    if (divisor == 1) return this
    var nanos: BigInt = toNanos
    nanos = nanos / BigInt(divisor)
    val divRem = nanos /% BigIntNanosPerSecond
    return ofSeconds(divRem._1.longValue, divRem._2.intValue)
  }

  def /(divisor: Long): Duration = dividedBy(divisor)

  /**
   * Checks if this duration is positive, excluding zero.
   * <p>
   * A     { @code Duration } represents a directed distance between two points on
   * the time-line and can therefore be positive, zero or negative.
   * This method checks whether the length is greater than zero.
   *
   * @return true if this duration has a total length greater than zero
   */
  def isPositive: Boolean = seconds >= 0 && ((seconds | nanos) != 0)

  /**
   * Checks if this duration is equal to the specified     { @code Duration }.
   * <p>
   * The comparison is based on the total length of the durations.
   *
   * @param otherDuration the other duration, null returns false
   * @return true if the other duration is equal to this one
   */
  // override def equals(otherDuration: AnyRef): Boolean

  /**
   * Checks if this duration is greater than the specified {@code Duration}.
   * <p>
   * The comparison is based on the total length of the durations.
   *
   * @param otherDuration the other duration to compare to, not null
   * @return true if this duration is greater than the specified duration
   */
  def isGreaterThan(otherDuration: Duration): Boolean = compareTo(otherDuration) > 0

  /**
   * Returns a copy of this duration with the specified duration added.
   * <p>
   * The duration to be added is measured in terms of the specified unit.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param amount the duration to add, positive or negative
   * @param unit the unit that the duration is measured in, not null
   * @return a { @code Duration } based on this duration with the specified duration added, never null
   * @throws ArithmeticException if the calculation exceeds the capacity of     { @code Duration }
   */
  def plus(amount: Long, unit: TimeUnit): Duration = {
    if (unit == TimeUnit.SECONDS) plusSeconds(amount)
    else if (unit == TimeUnit.MILLISECONDS) plusMillis(amount)
    else if (unit == TimeUnit.NANOSECONDS) plusNanos(amount)
    else plus(of(amount, unit))
  }

  /**
   * A string representation of this duration using ISO-8601 seconds
   * based representation, such as {@code PT12.345S}.
   * <p>
   * The format of the returned string will be {@code PTnS} where n is
   * the seconds and fractional seconds of the duration.
   *
   * @return an ISO-8601 representation of this duration, never null
   */
  override def toString: String = {
    val buf: StringBuilder = new StringBuilder(24)
    buf.append("PT")
    if (seconds < 0 && nanos > 0) {
      if (seconds == -1) {
        buf.append("-0")
      }
      else {
        buf.append(seconds + 1)
      }
    }
    else {
      buf.append(seconds)
    }
    if (nanos > 0) {
      val pos: Int = buf.length
      if (seconds < 0) buf.append(2 * NanosPerSecond - nanos)
      else buf.append(nanos + NanosPerSecond)
      while (buf.charAt(buf.length - 1) == '0') buf.setLength(buf.length - 1)
      buf.setCharAt(pos, '.')
    }
    buf.append('S')
    return buf.toString
  }

  /**
   * Returns a copy of this duration with the specified duration subtracted.
   * <p>
   * The duration to be subtracted is measured in terms of the specified unit.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param amount the duration to subtract, positive or negative
   * @param unit the unit that the duration is measured in, not null
   * @return a { @code Duration } based on this duration with the specified duration subtracted, never null
   * @throws ArithmeticException if the calculation exceeds the capacity of {@code Duration}
   */
  def minus(amount: Long, unit: TimeUnit): Duration = {
    if (unit == TimeUnit.SECONDS) return minusSeconds(amount)
    else if (unit == TimeUnit.MILLISECONDS) minusMillis(amount)
    else if (unit == TimeUnit.NANOSECONDS) minusNanos(amount)
    else minus(of(amount, unit))
  }

  /**
   * Returns a copy of this duration with the specified number of nanoseconds subtracted.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param nanosToSubtract the nanoseconds to subtract, positive or negative
   * @return a {@code Duration} based on this duration with the specified nanoseconds subtracted, never null
   * @throws ArithmeticException if the calculation exceeds the capacity of     { @code Duration }
   */
  def minusNanos(nanosToSubtract: Long): Duration = {
    if (nanosToSubtract == 0) {
      return this
    }
    var secondsToSubtract: Long = nanosToSubtract / NanosPerSecond
    var nos: Int = (nanosToSubtract % NanosPerSecond).toInt
    nos = nanos - nos
    if (nos < 0) {
      nos += NanosPerSecond
      secondsToSubtract += 1;
    }
    else if (nos >= NanosPerSecond) {
      nos -= NanosPerSecond
      secondsToSubtract -= 1;
    }
    return create(MathUtils.safeSubtract(seconds, secondsToSubtract), nos)
  }

  /**
   * A hash code for this duration.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = ((seconds ^ (seconds >>> 32)).toInt) + (51 * nanos)

  /**
   * Returns a copy of this duration with a positive length.
   * <p>
   * This method returns a positive duration by effectively removing the sign from any negative total length.
   * For example,     { @code PT -1.3S } will be returned as     { @code PT1.3S }.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @return a { @code Duration } based on this period with an absolute length, never null
   * @throws ArithmeticException if the seconds part of the length is     { @code Long.MIN_VALUE }
   */
  def abs: Duration = if (isNegative) negated else this

  /**
   * Checks if this duration is negative or zero.
   * <p>
   * A     { @code Duration } represents a directed distance between two points on
   * the time-line and can therefore be positive, zero or negative.
   * This method checks whether the length is less than or equal to zero.
   *
   * @return true if this duration has a total length less than or equal to zero
   */
  def isNegativeOrZero: Boolean = seconds < 0 || ((seconds | nanos) == 0)

  /**
   * Converts this duration to the total length in nanoseconds expressed as a     { @code BigInteger }.
   *
   * @return the total length of the duration in nanoseconds, never null
   */
  def toNanos: BigInt = BigInt(seconds) * (BigIntNanosPerSecond) + BigInt(nanos)

  /**
   * Checks if this duration is negative, excluding zero.
   * <p>
   * A     { @code Duration } represents a directed distance between two points on
   * the time-line and can therefore be positive, zero or negative.
   * This method checks whether the length is less than zero.
   *
   * @return true if this duration has a total length less than zero
   */
  def isNegative: Boolean = seconds < 0

  /**
   * Returns a copy of this duration with the length negated.
   * <p>
   * This method swaps the sign of the total length of this duration.
   * For example, {@code PT1.3S} will be returned as {@code PT -1.3S}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @return a { @code Duration } based on this period with the amount negated, never null
   * @throws ArithmeticException if the seconds part of the length is {@code Long.MinValue}
   */
  def negated: Duration = multipliedBy(-1)

  //TODO: TEST
  def unary_- : Duration = multipliedBy(-1)

  /**
   * Converts this duration to the total length in nanoseconds expressed as a {@code long}.
   * <p>
   * If this duration is too large to fit in a {@code long} nanoseconds, then an
   * exception is thrown.
   *
   * @return the total length of the duration in nanoseconds
   * @throws ArithmeticException if the length exceeds the capacity of a {@code long}
   */
  def toNanosLong: Long = {
    val millis: Long = MathUtils.safeMultiply(seconds, 1000000000)
    MathUtils.safeAdd(millis, nanos)
  }

  /**
   * Converts this duration to the total length in milliseconds.
   * <p>
   * If this duration is too large to fit in a {@code long} milliseconds, then an
   * exception is thrown.
   * <p>
   * If this duration has greater than millisecond precision, then the conversion
   * will drop any excess precision information as though the amount in nanoseconds
   * was subject to integer division by one million.
   *
   * @return the total length of the duration in milliseconds
   * @throws ArithmeticException if the length exceeds the capacity of a {@code long}
   */
  def toMillisLong: Long = {
    val millis: Long = MathUtils.safeMultiply(seconds, 1000)
    MathUtils.safeAdd(millis, nanos / 1000000)
  }

  /**
   * Compares this duration to the specified     { @code Duration }.
   * <p>
   * The comparison is based on the total length of the durations.
   *
   * @param otherDuration the other duration to compare to, not null
   * @return the comparator value, negative if less, positive if greater
   */
  def compareTo(otherDuration: Duration): Int = {
    val cmp: Int = MathUtils.safeCompare(seconds, otherDuration.seconds)
    if (cmp != 0) return cmp
    return MathUtils.safeCompare(nanos, otherDuration.nanos)
  }

  /**
   * Returns a copy of this duration with the specified number of milliseconds subtracted.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param millisToSubtract the milliseconds to subtract, positive or negative
   * @return a { @code Duration } based on this duration with the specified milliseconds subtracted, never null
   * @throws ArithmeticException if the calculation exceeds the capacity of     { @code Duration }
   */
  def minusMillis(millisToSubtract: Long): Duration = {
    if (millisToSubtract == 0) return this
    var secondsToSubtract: Long = millisToSubtract / 1000
    var nos: Int = ((millisToSubtract % 1000).asInstanceOf[Int]) * 1000000
    nos = nanos - nos
    if (nos < 0) {
      nos += NanosPerSecond
      secondsToSubtract += 1;

    }
    else if (nos >= NanosPerSecond) {
      nos -= NanosPerSecond
      secondsToSubtract -= 1;
    }
    return create(MathUtils.safeSubtract(seconds, secondsToSubtract), nos)
  }

  /**
   * Returns a copy of this duration with the specified {@code Duration} subtracted.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param duration the duration to subtract, positive or negative, not null
   * @return a {@code Duration} based on this duration with the specified duration subtracted, never null
   * @throws ArithmeticException if the calculation exceeds the capacity of {@code Duration}
   */
  def minus(duration: Duration): Duration = {
    var secsToSubtract: Long = duration.seconds
    var nanosToSubtract: Int = duration.nanos
    if (secsToSubtract == 0 && nanosToSubtract == 0) {
      return this
    }
    var secs: Long = MathUtils.safeSubtract(seconds, secsToSubtract)
    var nos: Int = nanos - nanosToSubtract
    if (nos < 0) {
      nos += NanosPerSecond
      secs = MathUtils.safeDecrement(secs)
    }
    return create(secs, nos)
  }

  /**
   * Returns a copy of this duration with the specified number of seconds added.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param secondsToAdd the seconds to add, positive or negative
   * @return a {@code Duration} based on this duration with the specified seconds added, never null
   * @throws ArithmeticException if the calculation exceeds the capacity of {@code Duration}
   */
  def plusSeconds(secondsToAdd: Long): Duration = {
    if (secondsToAdd == 0) return this
    val secs: Long = MathUtils.safeAdd(seconds, secondsToAdd)
    return create(secs, nanos)
  }

  /**
   * Returns a copy of this duration with the specified number of milliseconds added.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param millisToAdd the milliseconds to add, positive or negative
   * @return a {@code Duration} based on this duration with the specified milliseconds added, never null
   * @throws ArithmeticException if the calculation exceeds the capacity of {@code Duration}
   */
  def plusMillis(millisToAdd: Long): Duration = {
    if (millisToAdd == 0) return this
    var secondsToAdd: Long = millisToAdd / 1000
    var nos: Int = ((millisToAdd % 1000).asInstanceOf[Int]) * 1000000
    nos += nanos
    if (nos < 0) {
      nos += NanosPerSecond
      secondsToAdd -= 1;
    }
    else if (nos >= NanosPerSecond) {
      nos -= NanosPerSecond
      secondsToAdd += 1;
    }
    return create(MathUtils.safeAdd(seconds, secondsToAdd), nos)
  }

  /**
   * Checks if this duration is less than the specified     { @code Duration }.
   * <p>
   * The comparison is based on the total length of the durations.
   *
   * @param otherDuration the other duration to compare to, not null
   * @return true if this duration is less than the specified duration
   */
  def isLessThan(otherDuration: Duration): Boolean = compareTo(otherDuration) < 0
}

