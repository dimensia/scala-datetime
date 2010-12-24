/*
 * Copyright (c) 2008-2010, Stephen Colebourne & Michael Nascimento Santos
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

import java.util.TreeMap
import javax.time.CalendricalException
import javax.time.Duration
import javax.time.MathUtils

/**
 * An immutable period consisting of the ISO-8601 year, month, day, hour,
 * minute, second and nanosecond units, such as '3 Months, 4 Days and 7 Hours'.
 * <p>
 * A period is a human-scale description of an amount of time.
 * This class represents the 7 standard definitions from {@link ISOChronology}.
 * The period units used are 'Years', 'Months', 'Days', 'Hours', 'Minutes',
 * 'Seconds' and 'Nanoseconds'.
 * <p>
 * The {@code ISOChronology} defines a relationship between some of the units:
 * <ul>
 * <li>12 months in a year</li>
 * <li>24 hours in a day (ignoring time-zones)</li>
 * <li>60 minutes in an hour</li>
 * <li>60 seconds in a minute</li>
 * <li>1,000,000,000 nanoseconds in a second</li>
 * </ul>
 * The 24 hours in a day connection is not always true, due to time-zone changes.
 * As such, methods on this class make it clear when the that connection is being used.
 * <p>
 * Period is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object Period {

  /**
   * A constant for a period of zero.
   */
  object Zero extends Period(0, 0, 0, 0, 0, 0, 0)

  /**
   * The ISO period units, trusted to not be altered.
   */
  private val Units: Array[PeriodUnit] = Array[PeriodUnit](
    ISOChronology.periodYears,
    ISOChronology.periodMonths,
    ISOChronology.periodDays,
    ISOChronology.periodHours,
    ISOChronology.periodMinutes,
    ISOChronology.periodSeconds,
    ISOChronology.periodNanos)

  /**
   * Obtains a {@code Period} from time-based fields.
   * <p>
   * This creates an instance based on hours, minutes, seconds and nanoseconds.
   *
   * @param hours the amount of hours, may be negative
   * @param minutes the amount of minutes, may be negative
   * @param seconds the amount of seconds, may be negative
   * @param nanos the amount of nanos, may be negative
   * @return the period, never null
   */
  def ofTimeFields(hours: Int, minutes: Int, seconds: Int, nanos: Long): Period =
    of(0, 0, 0, hours, minutes, seconds, nanos)

  /**
   * Obtains a {@code Period} consisting of the number of years between two dates.
   * <p>
   * The start date is included, but the end date is not. Only whole years count.
   * For example, from {@code 2010 -01-15} to {@code 2012 -01-15} is two years,
   * whereas from {@code 2010 -01-15} to {@code 2012 -01-14} is only one year.
   * <p>
   * The result of this method can be a negative period if the end is before the start.
   *
   * @param startDateProvider the start date, inclusive, not null
   * @param endDateProvider the end date, exclusive, not null
   * @return the period in days, never null
   * @throws ArithmeticException if the period exceeds the supported range
   */
  def yearsBetween(startDateProvider: DateProvider, endDateProvider: DateProvider): Period = {
    val startDate: LocalDate = LocalDate.of(startDateProvider)
    val endDate: LocalDate = LocalDate.of(endDateProvider)
    val startMonth: Long = startDate.getYear * 12L + startDate.getMonthOfYear.ordinal
    val endMonth: Long = endDate.getYear * 12L + endDate.getMonthOfYear.ordinal
    var years: Long = (endMonth - startMonth) / 12
    if (endDate.getMonthOfYear == startDate.getMonthOfYear) {
      if (years > 0 && endDate.getDayOfMonth < startDate.getDayOfMonth) {
        years -= 1;
      }
      else if (years < 0 && endDate.getDayOfMonth > startDate.getDayOfMonth) {
        years += 1;
      }
    }
    return ofYears(MathUtils.safeToInt(years))
  }

  /**
   * Obtains a {@code Period} from a number of years.
   *
   * @param years the amount of years, may be negative
   * @return the period, never null
   */
  def ofYears(years: Int): Period = {
    if (years == 0) Zero
    else new Period(years, 0, 0, 0, 0, 0, 0)
  }

  /**
   * Obtains a {@code Period} from a number of minutes.
   *
   * @param minutes the amount of minutes, may be negative
   * @return the period, never null
   */
  def ofMinutes(minutes: Int): Period = {
    if (minutes == 0) Zero
    else new Period(0, 0, 0, 0, minutes, 0, 0)
  }

  /**
   * Obtains a {@code Period} from a number of days.
   *
   * @param days the amount of days, may be negative
   * @return the period, never null
   */
  def ofDays(days: Int): Period = {
    if (days == 0) Zero
    else new Period(0, 0, days, 0, 0, 0, 0)
  }

  /**
   * Obtains a {@code Period} from a number of seconds.
   *
   * @param seconds the amount of seconds, may be negative
   * @return the period, never null
   */
  def ofSeconds(seconds: Int): Period = {
    if (seconds == 0) Zero
    else new Period(0, 0, 0, 0, 0, seconds, 0)
  }

  /**
   * Obtains a {@code Period} from a provider of periods.
   * <p>
   * A {@code Period} supports 7 units, ISO years, months, days, hours,
   * minutes, seconds and nanoseconds. Any period that contains amounts in
   * these units, or in units that can be converted to these units will be
   * accepted. If the provider contains any other unit, an exception is thrown.
   *
   * @param periodProvider a provider of period information, not null
   * @return the period, never null
   * @throws CalendricalException if the provided period cannot be converted to the supported units
   * @throws ArithmeticException if any provided amount, exceeds the supported range
   */
  def of(periodProvider: PeriodProvider): Period = {
    PeriodFields.checkNotNull(periodProvider, "PeriodProvider must not be null")
    if (periodProvider.isInstanceOf[Period]) {
      return periodProvider.asInstanceOf[Period]
    }
    var periodFields: PeriodFields = PeriodFields.of(periodProvider)
    periodFields = periodFields.toEquivalent(Units)
    val years: Int = periodFields.getAmountInt(ISOChronology.periodYears)
    val months: Int = periodFields.getAmountInt(ISOChronology.periodMonths)
    val days: Int = periodFields.getAmountInt(ISOChronology.periodDays)
    val hours: Int = periodFields.getAmountInt(ISOChronology.periodHours)
    val minutes: Int = periodFields.getAmountInt(ISOChronology.periodMinutes)
    val seconds: Int = periodFields.getAmountInt(ISOChronology.periodSeconds)
    val nanos: Long = periodFields.getAmount(ISOChronology.periodNanos)
    return of(years, months, days, hours, minutes, seconds, nanos)
  }

  /**
   * Obtains a {@code Period} from a {@code Duration}.
   * <p>
   * The created period will have normalized values for the hours, minutes,
   * seconds and nanoseconds fields. The years, months and days fields will be zero.
   * <p>
   * To populate the days field, call {@link #normalizedWith24HourDays()} on the created period.
   *
   * @param duration the duration to create from, not null
   * @return the {@code PeriodFields} instance, never null
   * @throws ArithmeticException if the result exceeds the supported period range
   */
  def of(duration: Duration): Period = {
    PeriodFields.checkNotNull(duration, "Duration must not be null")
    if (duration.isZero) {
      return Zero
    }
    val hours: Int = MathUtils.safeToInt(duration.getSeconds / 3600)
    val amount: Int = (duration.getSeconds.asInstanceOf[Int] % 3600)
    return new Period(0, 0, 0, hours, amount / 60, amount % 60, duration.getNanoOfSecond)
  }

  /**
   * Obtains a {@code Period} from a text string such as {@code PnYnMnDTnHnMn.nS}.
   * <p>
   * This will parse the string produced by {@code toString()} which is
   * a subset of the ISO8601 period format {@code PnYnMnDTnHnMn.nS}.
   * <p>
   * The string consists of a series of numbers with a suffix identifying their meaning.
   * The values, and suffixes, must be in the sequence year, month, day, hour, minute, second.
   * Any of the number/suffix pairs may be omitted providing at least one is present.
   * If the period is zero, the value is normally represented as {@code PT0S}.
   * The numbers must consist of ASCII digits.
   * Any of the numbers may be negative. Negative zero is not accepted.
   * The number of nanoseconds is expressed as an optional fraction of the seconds.
   * There must be at least one digit before any decimal point.
   * There must be between 1 and 9 inclusive digits after any decimal point.
   * The letters will all be accepted in upper or lower case.
   * The decimal point may be either a dot or a comma.
   *
   * @param text the text to parse, not null
   * @return the parsed period, never null
   * @throws CalendricalParseException if the text cannot be parsed to a Period
   */
  def parse(text: String): Period = {
    PeriodFields.checkNotNull(text, "Text to parse must not be null")
    PeriodParser.parse(text)
  }

  /**
   * Obtains a {@code Period} from a number of hours.
   *
   * @param hours the amount of hours, may be negative
   * @return the period, never null
   */
  def ofHours(hours: Int): Period = {
    if (hours == 0) Zero
    else new Period(0, 0, 0, hours, 0, 0, 0)
  }

  /**
   * Obtains a {@code Period} from a number of months.
   *
   * @param months the amount of months, may be negative
   * @return the period, never null
   */
  def ofMonths(months: Int): Period = {
    if (months == 0) Zero
    else new Period(0, months, 0, 0, 0, 0, 0)
  }

  /**
   * Obtains a {@code Period} from a number of nanoseconds.
   *
   * @param nanos the amount of nanos, may be negative
   * @return the period, never null
   */
  def ofNanos(nanos: Long): Period = {
    if (nanos == 0) Zero
    else new Period(0, 0, 0, 0, 0, 0, nanos)
  }

  /**
   * Obtains a {@code Period} from the time-based fields of a period.
   * <p>
   * A {@code Period} supports 7 units, ISO years, months, days, hours,
   * minutes, seconds and nanoseconds. Any period that contains amounts in
   * these units, or in units that can be converted to these units will be
   * accepted. If the provider contains any other unit, an exception is thrown.
   * <p>
   * Once the initial conversion to the 7 units is complete, the period is created
   * using just the time-based fields - hours, minutes, seconds and nanoseconds.
   * The date-based fields are ignored and will be zero in the created period.
   *
   * @param periodProvider a provider of period information, not null
   * @return the period containing only time-based fields, never null
   * @throws CalendricalException if the provided period cannot be converted to the supported units
   * @throws ArithmeticException if any provided amount, exceeds the supported range
   */
  def ofTimeFields(periodProvider: PeriodProvider): Period = of(periodProvider).withTimeFieldsOnly

  /**
   * Obtains a {@code Period} consisting of the number of days, months
   * and years between two dates.
   * <p>
   * The start date is included, but the end date is not. Only whole years count.
   * For example, from {@code 2010 -01-15} to {@code 2011 -03-18} is one year, two months and three days.
   * <p>
   * The result of this method can be a negative period if the end is before the start.
   * The negative sign will be the same in each of year, month and day.
   * <p>
   * Adding the result of this method to the start date will always yield the end date.
   *
   * @param startDateProvider the start date, inclusive, not null
   * @param endDateProvider the end date, exclusive, not null
   * @return the period in days, never null
   * @throws ArithmeticException if the period exceeds the supported range
   */
  def between(startDateProvider: DateProvider, endDateProvider: DateProvider): Period = {
    val startDate: LocalDate = LocalDate.of(startDateProvider)
    val endDate: LocalDate = LocalDate.of(endDateProvider)
    val startMonth: Long = startDate.getYear * 12L + startDate.getMonthOfYear.ordinal
    val endMonth: Long = endDate.getYear * 12L + endDate.getMonthOfYear.ordinal
    var totalMonths: Long = endMonth - startMonth
    var days: Int = endDate.getDayOfMonth - startDate.getDayOfMonth
    if (totalMonths > 0 && days < 0) {
      totalMonths -= 1
      val calcDate: LocalDate = startDate.plusMonths(totalMonths)
      days = (endDate.toEpochDays - calcDate.toEpochDays).asInstanceOf[Int]
    }
    else if (totalMonths < 0 && days > 0) {
      totalMonths += 1
      days -= endDate.getMonthOfYear.lengthInDays(endDate.isLeapYear)
    }
    val years: Long = totalMonths / 12
    val months: Int = (totalMonths % 12).asInstanceOf[Int]
    ofDateFields(MathUtils.safeToInt(years), months, days)
  }

  /**
   * Obtains a {@code Period} from the date-based fields of a period.
   * <p>
   * A {@code Period} supports 7 units, ISO years, months, days, hours,
   * minutes, seconds and nanoseconds. Any period that contains amounts in
   * these units, or in units that can be converted to these units will be
   * accepted. If the provider contains any other unit, an exception is thrown.
   * <p>
   * Once the initial conversion to the 7 units is complete, the period is created
   * using just the date-based fields - years, months and days.
   * The time-based fields are ignored and will be zero in the created period.
   *
   * @param periodProvider a provider of period information, not null
   * @return the period containing only date-based fields, never null
   * @throws CalendricalException if the provided period cannot be converted to the supported units
   * @throws ArithmeticException if any provided amount, exceeds the supported range
   */
  def ofDateFields(periodProvider: PeriodProvider): Period = of(periodProvider).withDateFieldsOnly

  /**
   * Obtains a {@code Period} from date-based fields.
   * <p>
   * This creates an instance based on years, months and days.
   *
   * @param years the amount of years, may be negative
   * @param months the amount of months, may be negative
   * @param days the amount of days, may be negative
   * @return the period, never null
   */
  def ofDateFields(years: Int, months: Int, days: Int): Period = of(years, months, days, 0, 0, 0, 0)

  /**
   * Obtains a {@code Period} consisting of the number of days between two dates.
   * <p>
   * The start date is included, but the end date is not. For example, from
   * {@code 2010 -01-15} to {@code 2010 -01-18} is three days.
   * <p>
   * The result of this method can be a negative period if the end is before the start.
   *
   * @param startDateProvider the start date, inclusive, not null
   * @param endDateProvider the end date, exclusive, not null
   * @return the period in days, never null
   * @throws ArithmeticException if the period exceeds the supported range
   */
  def daysBetween(startDateProvider: DateProvider, endDateProvider: DateProvider): Period = {
    val startDate: LocalDate = LocalDate.of(startDateProvider)
    val endDate: LocalDate = LocalDate.of(endDateProvider)
    val days: Long = MathUtils.safeSubtract(endDate.toModifiedJulianDays, startDate.toModifiedJulianDays)
    ofDays(MathUtils.safeToInt(days))
  }

  /**
   * Obtains a {@code Period} consisting of the number of months between two dates.
   * <p>
   * The start date is included, but the end date is not. Only whole months count.
   * For example, from {@code 2010 -01-15} to {@code 2010 -03-15} is two months,
   * whereas from {@code 2010 -01-15} to {@code 2010 -03-14} is only one month.
   * <p>
   * The result of this method can be a negative period if the end is before the start.
   *
   * @param startDateProvider the start date, inclusive, not null
   * @param endDateProvider the end date, exclusive, not null
   * @return the period in days, never null
   * @throws ArithmeticException if the period exceeds the supported range
   */
  def monthsBetween(startDateProvider: DateProvider, endDateProvider: DateProvider): Period = {
    val startDate: LocalDate = LocalDate.of(startDateProvider)
    val endDate: LocalDate = LocalDate.of(endDateProvider)
    val startMonth: Long = startDate.getYear * 12L + startDate.getMonthOfYear.ordinal
    val endMonth: Long = endDate.getYear * 12L + endDate.getMonthOfYear.ordinal
    var months: Long = endMonth - startMonth
    if (months > 0 && endDate.getDayOfMonth < startDate.getDayOfMonth) {
      months -= 1
    }
    else if (months < 0 && endDate.getDayOfMonth > startDate.getDayOfMonth) {
      months += 1
    }
    ofMonths(MathUtils.safeToInt(months))
  }

  /**
   * Obtains a {@code Period} from time-based fields.
   * <p>
   * This creates an instance based on hours, minutes and seconds.
   *
   * @param hours the amount of hours, may be negative
   * @param minutes the amount of minutes, may be negative
   * @param seconds the amount of seconds, may be negative
   * @return the period, never null
   */
  def ofTimeFields(hours: Int, minutes: Int, seconds: Int): Period = of(0, 0, 0, hours, minutes, seconds, 0)

  /**
   * Obtains a {@code Period} from an amount and unit.
   * <p>
   * The parameters represent the two parts of a phrase like '6 Days'.
   * <p>
   * A {@code Period} supports 7 units, ISO years, months, days, hours,
   * minutes, seconds and nanoseconds. The unit must be one of these, or be
   * able to be converted to one of these.
   *
   * @param amount the amount of the period, measured in terms of the unit, positive or negative
   * @param unit the unit that the period is measured in, not null
   * @return the period, never null
   */
  def of(amount: Int, unit: PeriodUnit): Period = of(PeriodFields.of(amount, unit))

  /**
   * Obtains a {@code Period} from date-based and time-based fields.
   * <p>
   * This creates an instance based on years, months, days, hours, minutes and seconds.
   *
   * @param years the amount of years, may be negative
   * @param months the amount of months, may be negative
   * @param days the amount of days, may be negative
   * @param hours the amount of hours, may be negative
   * @param minutes the amount of minutes, may be negative
   * @param seconds the amount of seconds, may be negative
   * @return the period, never null
   */
  //  def of(years: Int, months: Int, days: Int, hours: Int, minutes: Int, seconds: Int): Period =
  //    of(years, months, days, hours, minutes, seconds, 0)

  /**
   * Obtains a {@code Period} from date-based and time-based fields.
   * <p>
   * This creates an instance based on years, months, days, hours, minutes, seconds and nanoseconds.
   * The resulting period will have normalized seconds and nanoseconds.
   *
   * @param years the amount of years, may be negative
   * @param months the amount of months, may be negative
   * @param days the amount of days, may be negative
   * @param hours the amount of hours, may be negative
   * @param minutes the amount of minutes, may be negative
   * @param seconds the amount of seconds, may be negative
   * @param nanos the amount of nanos, may be negative
   * @return the period, never null
   */
  def of(years: Int = 0, months: Int = 0, days: Int = 0, hours: Int = 0, minutes: Int = 0, seconds: Int = 0, nanos: Long = 0): Period = {
    if ((years | months | days | hours | minutes | seconds | nanos) == 0) Zero
    else new Period(years, months, days, hours, minutes, seconds, nanos)
  }
}

/**
 * Constructor.
 *
 * @param years the amount of years of this period
 * @param months the amount
 * @param days the amount
 * @param hours the amount
 * @param minutes the amount
 * @param seconds the amount
 * @param nanos the amount
 */
@SerialVersionUID(1L)
sealed class Period private(val years: Int, val months: Int, val days: Int, val hours: Int, val minutes: Int, val seconds: Int, val nanos: Long)
  extends PeriodProvider with Serializable {

  import Period._

  /**
   * The cached toString value.
   */
  @volatile
  @transient
  private var string: String = null

  /**
   * The cached PeriodFields.
   */
  @volatile
  @transient
  private var periodFields: PeriodFields = null

  /**
   * Gets the amount of years of this period, if any.
   *
   * @return the amount of years of this period
   */
  def getYears: Int = years

  /**
   * Returns a copy of this period with the specified number of days added.
   * <p>
   * This method will only affect the the days field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param days the days to add, positive or negative
   * @return a {@code Period} based on this period with the requested days added, never null
   * @throws ArithmeticException if the capacity of an {@code int} is exceeded
   */
  def plusDays(days: Int): Period = withDays(MathUtils.safeAdd(this.days, days))

  /**
   * Returns a copy of this period with the specified number of seconds added.
   * <p>
   * This method will only affect the the seconds field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param seconds the seconds to add, positive or negative
   * @return a {@code Period} based on this period with the requested seconds added, never null
   * @throws ArithmeticException if the capacity of an {@code int} is exceeded
   */
  def plusSeconds(seconds: Int): Period = withSeconds(MathUtils.safeAdd(this.seconds, seconds))

  /**
   * Resolves singletons.
   *
   * @return the resolved instance
   */
  private def readResolve: AnyRef = {
    if ((years | months | days | hours | minutes | seconds | nanos) == 0) Zero
    else this
  }

  /**
   * Returns a copy of this period with the specified number of years subtracted.
   * <p>
   * This method will only affect the the years field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to subtract, positive or negative
   * @return a {@code Period} based on this period with the requested years subtracted, never null
   * @throws ArithmeticException if the capacity of an {@code int} is exceeded
   */
  def minusYears(years: Int): Period = withYears(MathUtils.safeSubtract(this.years, years))

  /**
   * Gets the total number of hours represented by this period using standard
   * assumptions for the meaning of day, hour, minute and second.
   * <p>
   * This method ignores years and months.
   * It calculates using assumptions:
   * <ul>
   * <li>24 hours in a day</li>
   * <li>60 minutes in an hour</li>
   * <li>60 seconds in a minute</li>
   * <li>1,000,000,000 nanoseconds in a second</li>
   * </ul>
   * This method is only appropriate to call if these assumptions are met.
   *
   * @return the total number of hours
   */
  def totalHoursWith24HourDays: Long = {
    if (this == Zero) 0
    else days * 24L + hours + (minutes + (seconds + (nanos / 1000000000L)) / 60L) / 60L
  }

  /**
   * Returns a copy of this period with the specified amount of minutes.
   * <p>
   * This method will only affect the the minutes field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param minutes the minutes to represent
   * @return a {@code Period} based on this period with the requested minutes, never null
   */
  def withMinutes(minutes: Int): Period = {
    if (minutes == this.minutes) this
    else of(years, months, days, hours, minutes, seconds, nanos)
  }

  /**
   * Returns the hash code for this period.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = {
    ((years << 27) | (years >>> 5)) ^
      ((hours << 22) | (hours >>> 10)) ^
      ((months << 18) | (months >>> 14)) ^
      ((minutes << 12) | (minutes >>> 20)) ^
      ((days << 6) | (days >>> 26)) ^
      seconds ^
      ((nanos.toInt) + 37)
  }

  /**
   * Checks if this period is fully positive, including zero.
   * <p>
   * This checks whether all the amounts in the period are positive,
   * defined as greater than or equal to zero.
   *
   * @return true if this period is fully positive including zero
   */
  def isPositiveOrZero: Boolean = ((years | months | days | hours | minutes | seconds | nanos) >= 0)

  /**
   * Calculates the accurate duration of this period.
   * <p>
   * The calculation uses the hours, minutes, seconds and nanoseconds fields.
   * If years, months or days are present an exception is thrown.
   * <p>
   * The duration is calculated using assumptions:
   * <ul>
   * <li>60 minutes in an hour</li>
   * <li>60 seconds in a minute</li>
   * <li>1,000,000,000 nanoseconds in a second</li>
   * </ul>
   * This method is only appropriate to call if these assumptions are met.
   *
   * @return a {@code Duration} equivalent to this period, never null
   * @throws CalendricalException if the period cannot be converted as it contains years/months/days
   */
  def toDuration: Duration = {
    if ((years | months | days) > 0) {
      throw new CalendricalException("Unable to convert period to duration as years/months/days are present: " + this)
    }
    val secs: Long = (hours * 60L + minutes) * 60L + seconds
    return Duration.ofSeconds(secs, nanos)
  }

  /**
   * Returns a copy of this period with the specified number of minutes added.
   * <p>
   * This method will only affect the the minutes field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param minutes the minutes to add, positive or negative
   * @return a {@code Period} based on this period with the requested minutes added, never null
   * @throws ArithmeticException if the capacity of an {@code int} is exceeded
   */
  def plusMinutes(minutes: Int): Period = withMinutes(MathUtils.safeAdd(this.minutes, minutes))

  /**
   * Returns a copy of this period with only the date-based fields retained.
   * <p>
   * The returned period will have the same values for the date-based fields
   * (years, months and days) and zero values for the time-based fields.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @return a {@code Period} based on this period with zero values for time-based fields, never null
   */
  def withDateFieldsOnly: Period = {
    if ((hours | minutes | seconds | nanos) == 0) this
    else ofDateFields(years, months, days)
  }

  /**
   * Returns a copy of this period with the specified amount of seconds.
   * <p>
   * This method will only affect the the seconds field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param seconds the seconds to represent
   * @return a {@code Period} based on this period with the requested seconds, never null
   */
  def withSeconds(seconds: Int): Period = {
    if (seconds == this.seconds) this
    else of(years, months, days, hours, minutes, seconds, nanos)
  }

  /**
   * Returns a copy of this period with the specified number of years added.
   * <p>
   * This method will only affect the the years field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to add, positive or negative
   * @return a {@code Period} based on this period with the requested years added, never null
   * @throws ArithmeticException if the capacity of an {@code int} is exceeded
   */
  def plusYears(years: Int): Period = withYears(MathUtils.safeAdd(this.years, years))

  /**
   * Returns a copy of this period with the specified number of hours added.
   * <p>
   * This method will only affect the the hours field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hours the hours to add, positive or negative
   * @return a {@code Period} based on this period with the requested hours added, never null
   * @throws ArithmeticException if the capacity of an {@code int} is exceeded
   */
  def plusHours(hours: Int): Period = withHours(MathUtils.safeAdd(this.hours, hours))

  /**
   * Returns a copy of this period with the specified number of days subtracted.
   * <p>
   * This method will only affect the the days field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param days the days to subtract, positive or negative
   * @return a {@code Period} based on this period with the requested days subtracted, never null
   * @throws ArithmeticException if the capacity of an {@code int} is exceeded
   */
  def minusDays(days: Int): Period = withDays(MathUtils.safeSubtract(this.days, days))

  /**
   * Gets the total number of seconds represented by this period using standard
   * assumptions for the meaning of hour, minute and second.
   * <p>
   * This method ignores years, months and days.
   * It calculates using assumptions:
   * <ul>
   * <li>60 minutes in an hour</li>
   * <li>60 seconds in a minute</li>
   * <li>1,000,000,000 nanoseconds in a second</li>
   * </ul>
   * This method is only appropriate to call if these assumptions are met.
   *
   * @return the total number of seconds
   */
  def totalSeconds: Long = {
    if (this == Zero) 0
    else (hours * 60L + minutes) * 60L + seconds + nanos / 1000000000L
  }

  /**
   * Returns a copy of this period with the specified amount of hours.
   * <p>
   * This method will only affect the the hours field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hours the hours to represent
   * @return a {@code Period} based on this period with the requested hours, never null
   */
  def withHours(hours: Int): Period = {
    if (hours == this.hours) this
    else of(years, months, days, hours, minutes, seconds, nanos)
  }

  /**
   * Checks if this period is fully positive, excluding zero.
   * <p>
   * This checks whether all the amounts in the period are positive,
   * defined as greater than zero.
   *
   * @return true if this period is fully positive excluding zero
   */
  def isPositive: Boolean = ((years | months | days | hours | minutes | seconds | nanos) > 0)

  /**
   * Returns a copy of this period with the specified number of months subtracted.
   * <p>
   * This method will only affect the the months field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to subtract, positive or negative
   * @return a {@code Period} based on this period with the requested months subtracted, never null
   * @throws ArithmeticException if the capacity of an {@code int} is exceeded
   */
  def minusMonths(months: Int): Period = withMonths(MathUtils.safeSubtract(this.months, months))

  /**
   * Returns a new instance with each element in this period multiplied
   * by the specified scalar.
   *
   * @param scalar the scalar to multiply by, not null
   * @return a {@code Period} based on this period with the amounts multiplied by the scalar, never null
   * @throws ArithmeticException if the capacity of any field is exceeded
   */
  def multipliedBy(scalar: Int): Period = {
    if (this == Zero || scalar == 1) this
    else of(
      MathUtils.safeMultiply(years, scalar),
      MathUtils.safeMultiply(months, scalar),
      MathUtils.safeMultiply(days, scalar),
      MathUtils.safeMultiply(hours, scalar),
      MathUtils.safeMultiply(minutes, scalar),
      MathUtils.safeMultiply(seconds, scalar),
      MathUtils.safeMultiply(nanos, scalar))
  }

  /**
   * Returns a copy of this period with the specified amount of months.
   * <p>
   * This method will only affect the the months field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to represent
   * @return a {@code Period} based on this period with the requested months, never null
   */
  def withMonths(months: Int): Period = {
    if (months == this.months) this
    else of(years, months, days, hours, minutes, seconds, nanos)
  }

  /**
   * Gets the amount of days of this period, if any.
   *
   * @return the amount of days of this period
   */
  def getDays: Int = days

  /**
   * Returns a copy of this period with the specified amount of nanoseconds.
   * <p>
   * This method will only affect the the nanoseconds field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param nanos the nanoseconds to represent
   * @return a {@code Period} based on this period with the requested nanoseconds, never null
   */
  def withNanos(nanos: Long): Period = {
    if (nanos == this.nanos) this
    else of(years, months, days, hours, minutes, seconds, nanos)
  }

  /**
   * Checks if this period is zero-length.
   *
   * @return true if this period is zero-length
   */
  def isZero: Boolean = (this == Zero)

  /**
   * Gets the amount of nanoseconds of this period safely converted
   * to an {@code int}.
   *
   * @return the amount of nanoseconds of this period
   * @throws ArithmeticException if the number of nanoseconds exceeds the capacity of an {@code int }
   */
  def getNanosInt: Int = MathUtils.safeToInt(nanos)

  /**
   * Gets the total number of nanoseconds represented by this period using standard
   * assumptions for the meaning of hour, minute and second.
   * <p>
   * This method ignores years, months and days.
   * It calculates using assumptions:
   * <ul>
   * <li>60 minutes in an hour</li>
   * <li>60 seconds in a minute</li>
   * <li>1,000,000,000 nanoseconds in a second</li>
   * </ul>
   * This method is only appropriate to call if these assumptions are met.
   *
   * @return the total number of nanoseconds
   * @throws ArithmeticException if the capacity of a {@code long} is exceeded
   */
  def totalNanos: Long = {
    if (this == Zero) return 0
    val secs: Long = ((hours * 60L + minutes) * 60L + seconds)
    val otherNanos: Long = MathUtils.safeMultiply(secs, 1000000000L)
    return MathUtils.safeAdd(otherNanos, nanos)
  }

  /**
   * Gets the amount of hours of this period, if any.
   *
   * @return the amount of hours of this period
   */
  def getHours: Int = hours

  /**
   * Returns a copy of this period with all amounts normalized to the
   * standard ranges for date-time fields including the assumption that
   * days are 24 hours long.
   * <p>
   * Two normalizations occur, one for years and months, and one for
   * days, hours, minutes, seconds and nanoseconds.
   * For example, a period of {@code P1Y15M1DT28H} will be normalized to {@code P2Y3M2DT4H}.
   * <p>
   * Note that this method normalizes using assumptions:
   * <ul>
   * <li>12 months in a year</li>
   * <li>24 hours in a day</li>
   * <li>60 minutes in an hour</li>
   * <li>60 seconds in a minute</li>
   * <li>1,000,000,000 nanoseconds in a second</li>
   * </ul>
   * This method is only appropriate to call if these assumptions are met.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @return a {@code Period} based on this period with the amounts normalized, never null
   * @throws ArithmeticException if the capacity of any field is exceeded
   */
  def normalizedWith24HourDays: Period = {
    if (this == Zero) {
      return Zero
    }
    var years: Int = this.years
    var months: Int = this.months
    if (months >= 12) {
      years = MathUtils.safeAdd(years, months / 12)
      months = months % 12
    }
    var total: Long = (days * 24L * 60L * 60L) + (hours * 60L * 60L) + (minutes * 60L) + seconds
    total = MathUtils.safeAdd(total, MathUtils.floorDiv(this.nanos, 1000000000))
    val nanos: Int = MathUtils.floorMod(this.nanos, 1000000000)
    val seconds: Int = (total % 60).toInt
    total /= 60
    val minutes: Int = (total % 60).toInt
    total /= 60
    val hours: Int = (total % 24).toInt
    total /= 24
    val days: Int = MathUtils.safeToInt(total)
    return of(years, months, days, hours, minutes, seconds, nanos)
  }

  /**
   * Gets the amount of nanoseconds of this period, if any.
   *
   * @return the amount of nanoseconds of this period
   */
  def getNanos: Long = nanos

  /**
   * Returns a copy of this period with the specified number of nanoseconds added.
   * <p>
   * This method will only affect the the nanoseconds field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param nanos the nanoseconds to add, positive or negative
   * @return a {@code Period} based on this period with the requested nanoseconds added, never null
   * @throws ArithmeticException if the capacity of a {@code long} is exceeded
   */
  def plusNanos(nanos: Long): Period = withNanos(MathUtils.safeAdd(this.nanos, nanos))

  /**
   * Returns a copy of this period with the specified number of nanoseconds subtracted.
   * <p>
   * This method will only affect the the nanoseconds field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param nanos the nanoseconds to subtract, positive or negative
   * @return a {@code Period} based on this period with the requested nanoseconds subtracted, never null
   * @throws ArithmeticException if the capacity of a {@code long} is exceeded
   */
  def minusNanos(nanos: Long): Period = withNanos(MathUtils.safeSubtract(this.nanos, nanos))

  /**
   * Gets the amount of minutes of this period, if any.
   *
   * @return the amount of minutes of this period
   */
  def getMinutes: Int = minutes

  /**
   * Gets the total number of minutes represented by this period using standard
   * assumptions for the meaning of day, hour, minute and second.
   * <p>
   * This method ignores years and months.
   * It calculates using assumptions:
   * <ul>
   * <li>24 hours in a day</li>
   * <li>60 minutes in an hour</li>
   * <li>60 seconds in a minute</li>
   * <li>1,000,000,000 nanoseconds in a second</li>
   * </ul>
   * This method is only appropriate to call if these assumptions are met.
   *
   * @return the total number of minutes
   */
  def totalMinutesWith24HourDays: Long = {
    if (this == Zero) 0
    else (days * 24L + hours) * 60L + minutes + (seconds + (nanos / 1000000000L)) / 60L
  }

  /**
   * Returns a copy of this period with only the time-based fields retained.
   * <p>
   * The returned period will have the same values for the time-based fields
   * (hours, minutes, seconds and nanoseconds) and zero values for the date-based fields.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @return a {@code Period} based on this period with zero values for date-based fields, never null
   */
  def withTimeFieldsOnly: Period = {
    if ((years | months | days) == 0) this
    else of(0, 0, 0, hours, minutes, seconds, nanos)
  }

  /**
   * Gets the total number of nanoseconds represented by this period using standard
   * assumptions for the meaning of day, hour, minute and second.
   * <p>
   * This method ignores years and months.
   * It calculates using assumptions:
   * <ul>
   * <li>24 hours in a day</li>
   * <li>60 minutes in an hour</li>
   * <li>60 seconds in a minute</li>
   * <li>1,000,000,000 nanoseconds in a second</li>
   * </ul>
   * This method is only appropriate to call if these assumptions are met.
   *
   * @return the total number of nanoseconds
   * @throws ArithmeticException if the capacity of a {@code long} is exceeded
   */
  def totalNanosWith24HourDays: Long = {
    if (this == Zero) return 0
    val secs: Long = (((days * 24L + hours) * 60L + minutes) * 60L + seconds)
    val otherNanos: Long = MathUtils.safeMultiply(secs, 1000000000L)
    return MathUtils.safeAdd(otherNanos, nanos)
  }

  /**
   * Returns a string representation of the amount of time.
   *
   * @return the amount of time in ISO8601 string format
   */
  override def toString: String = {
    var str: String = string
    if (str == null) {
      if (this == Zero) {
        str = "PT0S"
      }
      else {
        val buf: StringBuilder = new StringBuilder
        buf.append('P')
        if (years != 0) {
          buf.append(years).append('Y')
        }
        if (months != 0) {
          buf.append(months).append('M')
        }
        if (days != 0) {
          buf.append(days).append('D')
        }
        if ((hours | minutes | seconds) != 0 || nanos != 0) {
          buf.append('T')
          if (hours != 0) {
            buf.append(hours).append('H')
          }
          if (minutes != 0) {
            buf.append(minutes).append('M')
          }
          if (seconds != 0 || nanos != 0) {
            if (nanos == 0) {
              buf.append(seconds).append('S')
            }
            else {
              var s: Long = seconds + (nanos / 1000000000)
              var n: Long = nanos % 1000000000
              if (s < 0 && n > 0) {
                n -= 1000000000
                s += 1;
              }
              else if (s > 0 && n < 0) {
                n += 1000000000
                s -= 1;
              }
              if (n < 0) {
                n = -n
                if (s == 0) {
                  buf.append('-')
                }
              }
              buf.append(s)
              val dotPos: Int = buf.length
              n += 1000000000
              while (n % 10 == 0) {
                n /= 10
              }
              buf.append(n)
              buf.setCharAt(dotPos, '.')
              buf.append('S')
            }
          }
        }
        str = buf.toString
      }
      string = str
    }
    return str
  }

  /**
   * Gets the total number of months represented by this period using standard
   * assumptions for the meaning of month.
   * <p>
   * This method ignores days, hours, minutes, seconds and nanos.
   * It calculates using the assumption:
   * <ul>
   * <li>12 months in a year</li>
   * </ul>
   * This method is only appropriate to call if these assumptions are met.
   *
   * @return the total number of years
   * @throws ArithmeticException if the capacity of a {@code long} is exceeded
   */
  def totalMonths: Long = MathUtils.safeAdd(MathUtils.safeMultiply(years.asInstanceOf[Long], 12), months)

  /**
   * Returns a copy of this period with the specified number of hours subtracted.
   * <p>
   * This method will only affect the the hours field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hours the hours to subtract, positive or negative
   * @return a {@code Period} based on this period with the requested hours subtracted, never null
   * @throws ArithmeticException if the capacity of an {@code int} is exceeded
   */
  def minusHours(hours: Int): Period = withHours(MathUtils.safeSubtract(this.hours, hours))

  /**
   * Calculates the accurate duration of this period.
   * <p>
   * The calculation uses the days, hours, minutes, seconds and nanoseconds fields.
   * If years or months are present an exception is thrown.
   * <p>
   * The duration is calculated using assumptions:
   * <ul>
   * <li>24 hours in a day</li>
   * <li>60 minutes in an hour</li>
   * <li>60 seconds in a minute</li>
   * <li>1,000,000,000 nanoseconds in a second</li>
   * </ul>
   * This method is only appropriate to call if these assumptions are met.
   *
   * @return a {@code Duration} equivalent to this period, never null
   * @throws CalendricalException if the period cannot be converted as it contains years/months/days
   */
  def toDurationWith24HourDays: Duration = {
    if ((years | months) > 0) {
      throw new CalendricalException("Unable to convert period to duration as years/months are present: " + this)
    }
    val secs: Long = ((days * 24L + hours) * 60L + minutes) * 60L + seconds
    return Duration.ofSeconds(secs, nanos)
  }

  /**
   * Returns a copy of this period with the specified number of seconds subtracted.
   * <p>
   * This method will only affect the the seconds field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param seconds the seconds to subtract, positive or negative
   * @return a {@code Period} based on this period with the requested seconds subtracted, never null
   * @throws ArithmeticException if the capacity of an {@code int} is exceeded
   */
  def minusSeconds(seconds: Int): Period = withSeconds(MathUtils.safeSubtract(this.seconds, seconds))

  /**
   * Returns a copy of this period with the specified period subtracted.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to subtract, not null
   * @return a {@code Period} based on this period with the requested period subtracted, never null
   * @throws ArithmeticException if the capacity of any field is exceeded
   */
  def minus(periodProvider: PeriodProvider): Period = {
    val other: Period = of(periodProvider)
    of(MathUtils.safeSubtract(years, other.years), MathUtils.safeSubtract(months, other.months), MathUtils.safeSubtract(days, other.days), MathUtils.safeSubtract(hours, other.hours), MathUtils.safeSubtract(minutes, other.minutes), MathUtils.safeSubtract(seconds, other.seconds), MathUtils.safeSubtract(nanos, other.nanos))
  }

  /**
   * Gets the total number of days represented by this period using standard
   * assumptions for the meaning of day, hour, minute and second.
   * <p>
   * This method ignores years and months.
   * It calculates using assumptions:
   * <ul>
   * <li>24 hours in a day</li>
   * <li>60 minutes in an hour</li>
   * <li>60 seconds in a minute</li>
   * <li>1,000,000,000 nanoseconds in a second</li>
   * </ul>
   * This method is only appropriate to call if these assumptions are met.
   *
   * @return the total number of days
   */
  def totalDaysWith24HourDays: Long = {
    if (this == Zero) 0
    else days + (hours + (minutes + (seconds + (nanos / 1000000000L)) / 60L) / 60L) / 24L
  }

  /**
   * Is this period equal to the specified period.
   *
   * @param obj the other period to compare to, null returns false
   * @return true if this instance is equal to the specified period
   */
  override def equals(obj: AnyRef): Boolean = {
    if (this == obj) true
    else if (obj.isInstanceOf[Period]) {
      val other: Period = obj.asInstanceOf[Period]
      years == other.years && months == other.months && days == other.days & hours == other.hours && minutes == other.minutes && seconds == other.seconds && nanos == other.nanos
    }
    else return false
  }

  /**
   * Gets the amount of months of this period, if any.
   *
   * @return the amount of months of this period
   */
  def getMonths: Int = months

  /**
   * Returns a copy of this period with the specified number of minutes subtracted.
   * <p>
   * This method will only affect the the minutes field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param minutes the minutes to subtract, positive or negative
   * @return a {@code Period} based on this period with the requested minutes subtracted, never null
   * @throws ArithmeticException if the capacity of an {@code int} is exceeded
   */
  def minusMinutes(minutes: Int): Period = withMinutes(MathUtils.safeSubtract(this.minutes, minutes))

  /**
   * Returns a new instance with each amount in this period negated.
   *
   * @return a {@code Period} based on this period with the amounts negated, never null
   * @throws ArithmeticException if any field has the minimum value
   */
  def negated: Period = multipliedBy(-1)

  def unary_- : Period = multipliedBy(-1)

  /**
   * Gets the total number of hours represented by this period using standard
   * assumptions for the meaning of hour, minute and second.
   * <p>
   * This method ignores years, months and days.
   * It calculates using assumptions:
   * <ul>
   * <li>60 minutes in an hour</li>
   * <li>60 seconds in a minute</li>
   * <li>1,000,000,000 nanoseconds in a second</li>
   * </ul>
   * This method is only appropriate to call if these assumptions are met.
   *
   * @return the total number of hours
   */
  def totalHours: Long = {
    if (this == Zero) 0
    else hours + (minutes + (seconds + (nanos / 1000000000L)) / 60L) / 60L
  }


  /**
   * Returns a copy of this period with all amounts normalized to the
   * standard ranges for date-time fields.
   * <p>
   * Two normalizations occur, one for years and months, and one for
   * hours, minutes, seconds and nanoseconds.
   * Days are not normalized, as a day may vary in length at daylight savings cutover.
   * For example, a period of {@code P1Y15M1DT28H61M} will be normalized to {@code P2Y3M1DT29H1M}.
   * <p>
   * Note that this method normalizes using assumptions:
   * <ul>
   * <li>12 months in a year</li>
   * <li>60 minutes in an hour</li>
   * <li>60 seconds in a minute</li>
   * <li>1,000,000,000 nanoseconds in a second</li>
   * </ul>
   * This method is only appropriate to call if these assumptions are met.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @return a {@code Period} based on this period with the amounts normalized, never null
   * @throws ArithmeticException if the capacity of any field is exceeded
   */
  def normalized: Period = {
    if (this == Zero) return Zero
    var years: Int = this.years
    var months: Int = this.months
    if (months >= 12) {
      years = MathUtils.safeAdd(years, months / 12)
      months = months % 12
    }
    var total: Long = (hours * 60L * 60L) + (minutes * 60L) + seconds
    total = MathUtils.safeMultiply(total, 1000000000)
    total = MathUtils.safeAdd(total, nanos)
    val nanos: Long = total % 1000000000L
    total /= 1000000000L
    val seconds: Int = (total % 60).asInstanceOf[Int]
    total /= 60
    val minutes: Int = (total % 60).asInstanceOf[Int]
    total /= 60
    val hours: Int = MathUtils.safeToInt(total)
    return of(years, months, days, hours, minutes, seconds, nanos)
  }

  /**
   * Returns a new instance with each element in this period divided
   * by the specified value.
   * <p>
   * The implementation simply divides each separate field by the divisor
   * using integer division.
   *
   * @param divisor the value to divide by, not null
   * @return a {@code Period} based on this period with the amounts divided by the divisor, never null
   * @throws ArithmeticException if dividing by zero
   */
  def dividedBy(divisor: Int): Period = {
    if (divisor == 0) throw new ArithmeticException("Cannot divide by zero")
    else if (this == Zero || divisor == 1) this
    else of(years / divisor, months / divisor, days / divisor, hours / divisor, minutes / divisor, seconds / divisor, nanos / divisor)
  }

  /**
   * Converts this period to a {@code PeriodFields}.
   * <p>
   * The returned {@code PeriodFields} will only contain the non-zero amounts.
   *
   * @return a {@code PeriodFields} equivalent to this period, never null
   */
  override def toPeriodFields: PeriodFields = {
    var fields: PeriodFields = periodFields
    if (fields == null) {
      val map: TreeMap[PeriodUnit, PeriodField] = new TreeMap[PeriodUnit, PeriodField]
      if (years != 0) {
        map.put(ISOChronology.periodYears, PeriodField.of(years, ISOChronology.periodYears))
      }
      if (months != 0) {
        map.put(ISOChronology.periodMonths, PeriodField.of(months, ISOChronology.periodMonths))
      }
      if (days != 0) {
        map.put(ISOChronology.periodDays, PeriodField.of(days, ISOChronology.periodDays))
      }
      if (hours != 0) {
        map.put(ISOChronology.periodHours, PeriodField.of(hours, ISOChronology.periodHours))
      }
      if (minutes != 0) {
        map.put(ISOChronology.periodMinutes, PeriodField.of(minutes, ISOChronology.periodMinutes))
      }
      if (seconds != 0) {
        map.put(ISOChronology.periodSeconds, PeriodField.of(seconds, ISOChronology.periodSeconds))
      }
      if (nanos != 0) {
        map.put(ISOChronology.periodNanos, PeriodField.of(nanos, ISOChronology.periodNanos))
      }
      periodFields = ({
        fields = PeriodFields.create(map);
        fields
      })
    }
    return fields
  }

  /**
   * Estimates the duration of this period.
   * <p>
   * Each {@link PeriodUnit} contains an estimated duration for that unit.
   * The per-unit estimate allows an estimate to be calculated for the whole period
   * including years, months and days. The estimate will equal the {@link #toDuration accurate }
   * calculation if the years, months and days fields are zero.
   *
   * @return the estimated duration of this period, never null
   * @throws ArithmeticException if the calculation overflows
   */
  def toEstimatedDuration: Duration = toPeriodFields.toEstimatedDuration

  /**
   * Gets the total number of seconds represented by this period using standard
   * assumptions for the meaning of day, hour, minute and second.
   * <p>
   * This method ignores years and months.
   * It calculates using assumptions:
   * <ul>
   * <li>24 hours in a day</li>
   * <li>60 minutes in an hour</li>
   * <li>60 seconds in a minute</li>
   * <li>1,000,000,000 nanoseconds in a second</li>
   * </ul>
   * This method is only appropriate to call if these assumptions are met.
   *
   * @return the total number of seconds
   */
  def totalSecondsWith24HourDays: Long = {
    if (this == Zero) 0
    else ((days * 24L + hours) * 60L + minutes) * 60L + seconds + nanos / 1000000000L
  }

  /**
   * Returns a copy of this period with the specified amount of days.
   * <p>
   * This method will only affect the the days field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param days the days to represent
   * @return a {@code Period} based on this period with the requested days, never null
   */
  def withDays(days: Int): Period = {
    if (days == this.days) this
    else of(years, months, days, hours, minutes, seconds, nanos)
  }

  /**
   * Gets the amount of seconds of this period, if any.
   *
   * @return the amount of seconds of this period
   */
  def getSeconds: Int = seconds

  /**
   * Gets the total number of years represented by this period using standard
   * assumptions for the meaning of month.
   * <p>
   * This method ignores days, hours, minutes, seconds and nanos.
   * It calculates using the assumption:
   * <ul>
   * <li>12 months in a year</li>
   * </ul>
   * This method is only appropriate to call if these assumptions are met.
   *
   * @return the total number of years
   * @throws ArithmeticException if the capacity of a {@code long} is exceeded
   */
  def totalYears: Long = MathUtils.safeAdd(years.toLong, (months / 12).toLong)

  /**
   * Returns a copy of this period with the specified number of months added.
   * <p>
   * This method will only affect the the months field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to add, positive or negative
   * @return a {@code Period} based on this period with the requested months added, never null
   * @throws ArithmeticException if the capacity of an {@code int} is exceeded
   */
  def plusMonths(months: Int): Period = withMonths(MathUtils.safeAdd(this.months, months))

  /**
   * Gets the total number of minutes represented by this period using standard
   * assumptions for the meaning of hour, minute and second.
   * <p>
   * This method ignores years, months and days.
   * It calculates using assumptions:
   * <ul>
   * <li>60 minutes in an hour</li>
   * <li>60 seconds in a minute</li>
   * <li>1,000,000,000 nanoseconds in a second</li>
   * </ul>
   * This method is only appropriate to call if these assumptions are met.
   *
   * @return the total number of minutes
   */
  def totalMinutes: Long = {
    if (this == Zero) 0
    else hours * 60L + minutes + (seconds + (nanos / 1000000000L)) / 60L
  }

  /**
   * Returns a copy of this period with the specified amount of years.
   * <p>
   * This method will only affect the the years field.
   * All other fields are left untouched.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to represent
   * @return a {@code Period} based on this period with the requested years, never null
   */
  def withYears(years: Int): Period = {
    if (years == this.years) this
    else of(years, months, days, hours, minutes, seconds, nanos)
  }

  /**
   * Returns a copy of this period with the specified period added.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to add, not null
   * @return a {@code Period} based on this period with the requested period added, never null
   * @throws ArithmeticException if the capacity of any field is exceeded
   */
  def plus(periodProvider: PeriodProvider): Period = {
    val other: Period = of(periodProvider)
    of(
      MathUtils.safeAdd(years, other.years),
      MathUtils.safeAdd(months, other.months),
      MathUtils.safeAdd(days, other.days),
      MathUtils.safeAdd(hours, other.hours),
      MathUtils.safeAdd(minutes, other.minutes),
      MathUtils.safeAdd(seconds, other.seconds),
      MathUtils.safeAdd(nanos, other.nanos))
  }

  def +(periodProvider: PeriodProvider): Period = plus(periodProvider)
}