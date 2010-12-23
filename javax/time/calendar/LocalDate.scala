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

import javax.time.CalendricalException
import javax.time.Instant
import javax.time.MathUtils
import javax.time.calendar.format.DateTimeFormatter
import javax.time.calendar.format.DateTimeFormatters

/**
 * A date without a time-zone in the ISO-8601 calendar system,
 * such as '2007-12-03'.
 * <p>
 * LocalDate is an immutable calendrical that represents a date, often viewed
 * as year-month-day. This object can also access other date fields such as
 * day-of-year, day-of-week and week-of-year.
 * <p>
 * This class does not store or represent a time or time-zone.
 * Thus, for example, the value "2nd October 2007" can be stored in a LocalDate.
 * <p>
 * The ISO-8601 calendar system is the modern civil calendar system used today
 * in most of the world. It is equivalent to the proleptic Gregorian calendar
 * system, in which todays's rules for leap years are applied for all time.
 * For most applications written today, the ISO-8601 rules are entirely suitable.
 * <p>
 * However, any application that makes use of historical dates and requires them
 * to be accurate will find the ISO-8601 rules unsuitable. In this case, the
 * application code should use {@code HistoricDate} and define an explicit
 * cutover date between the Julian and Gregorian calendar systems.
 * <p>
 * LocalDate is immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object LocalDate {
  /**
   * Obtains the current date from the specified clock.
   * <p>
   * This will query the specified clock to obtain the current date - today.
   * Using this method allows the use of an alternate clock for testing.
   * The alternate clock may be introduced using {@link Clock dependency injection}.
   *
   * @param clock the clock to use, not null
   * @return the current date, never null
   */
  def now(clock: Clock): LocalDate = {
    ISOChronology.checkNotNull(clock, "Clock must not be null")
    val instant: Instant = clock.instant
    val offset: ZoneOffset = clock.getZone.getRules.getOffset(instant)
    val epochSecs: Long = instant.getEpochSeconds + offset.getAmountSeconds
    val yearZeroDays: Long = MathUtils.floorDiv(epochSecs, ISOChronology.SecondsPerDay) + ISOChronology.Days0000To1970
    LocalDate.ofYearZeroDays(yearZeroDays)
  }

  /**
   * Obtains an instance of {@code LocalDate} from a year, month and day.
   * <p>
   * The day must be valid for the year and month or an exception will be thrown.
   *
   * @param year the year to represent, from MIN_YEAR to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, from 1 (January) to 12 (December)
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @return the local date, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def of(year: Int, monthOfYear: Int, dayOfMonth: Int): LocalDate = {
    ISOChronology.yearRule.checkValue(year)
    ISOChronology.monthOfYearRule.checkValue(monthOfYear)
    ISOChronology.dayOfMonthRule.checkValue(dayOfMonth)
    create(year, MonthOfYear.of(monthOfYear), dayOfMonth)
  }

  /**
   * Obtains an instance of {@code LocalDate} from a year, month and day.
   * <p>
   * The day must be valid for the year and month or an exception will be thrown.
   *
   * @param year the year to represent, from MIN_YEAR to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, not null
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @return the local date, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def of(year: Int, monthOfYear: MonthOfYear, dayOfMonth: Int): LocalDate = {
    ISOChronology.yearRule.checkValue(year)
    ISOChronology.checkNotNull(monthOfYear, "MonthOfYear must not be null")
    ISOChronology.dayOfMonthRule.checkValue(dayOfMonth)
    create(year, monthOfYear, dayOfMonth)
  }

  /**
   * Rule implementation.
   */
  private[calendar] object Rule extends Rule

  @SerialVersionUID(1L)
  private[calendar] sealed class Rule
    extends CalendricalRule[LocalDate](classOf[LocalDate], ISOChronology, "LocalDate", ISOChronology.periodDays, null) with Serializable {

    protected override def derive(calendrical: Calendrical): Option[LocalDate] = {
      val ldt: LocalDateTime = calendrical.get(LocalDateTime.rule).getOrElse(return None)
      return Some(ldt.toLocalDate)
      val od: OffsetDate = calendrical.get(OffsetDate.rule).getOrElse(return None)
      return Some(od.toLocalDate)
    }

    private def readResolve: AnyRef = Rule
  }

  /**
   * Obtains an instance of {@code LocalDate} from a text string.
   * <p>
   * The following format is accepted in ASCII:
   * <ul>
   * <li> {@code  { Year} -  { MonthOfYear} -  { DayOfMonth} }
   * </ul>
   * The year has between 4 and 10 digits with values from MIN_YEAR to MAX_YEAR.
   * If there are more than 4 digits then the year must be prefixed with the plus symbol.
   * Negative years are allowed, but not negative zero.
   * <p>
   * The month-of-year has 2 digits with values from 1 to 12.
   * <p>
   * The day-of-month has 2 digits with values from 1 to 31 appropriate to the month.
   *
   * @param text the text to parse such as '2007-12-03', not null
   * @return the parsed local date, never null
   * @throws CalendricalException if the text cannot be parsed
   */
  def parse(text: String): LocalDate = DateTimeFormatters.isoLocalDate.parse(text, rule)

  /**
   * Obtains the current date from the system clock in the default time-zone.
   * <p>
   * This will query the system clock in the default time-zone to obtain the current date - today.
   * Using this method will prevent the ability to use an alternate clock for testing
   * because the clock is hard-coded.
   *
   * @return the current date using the system clock, never null
   */
  def now: LocalDate = now(Clock.systemDefaultZone)

  /**
   * Obtains an instance of {@code LocalDate} from a date provider.
   * <p>
   * The purpose of this method is to convert a {@code DateProvider }
   * to a {@code LocalDate} in the safest possible way. Specifically,
   * the means checking whether the input parameter is null and
   * whether the result of the provider is null.
   *
   * @param dateProvider the date provider to use, not null
   * @return the local date, never null
   * @throws NullPointerException if the provider is null or returns null
   */
  def of(dateProvider: DateProvider): LocalDate = {
    ISOChronology.checkNotNull(dateProvider, "DateProvider must not be null")
    val result: LocalDate = dateProvider.toLocalDate
    ISOChronology.checkNotNull(result, "DateProvider implementation must not return null")
    result
  }

  /**
   * Obtains an instance of {@code LocalDate} from the Modified Julian Day (MJD).
   * <p>
   * The Modified Julian Day count is a simple incrementing count of days
   * where day 0 is 1858-11-17.
   *
   * @param mjDays the Modified Julian Day to convert, based on the epoch 1858-11-17
   * @return the local date, never null
   * @throws IllegalCalendarFieldValueException if the modified julian days value is outside the supported range
   */
  def ofModifiedJulianDays(mjDays: Long): LocalDate = ofYearZeroDays(mjDays + ISOChronology.Days0000ToModifiedJulianDaysEpoch)

  /**
   * Converts a year zero day count to a date.
   * <p>
   * The year zero day count is a simple incrementing count of days
   * where day 0 is 0000-01-01.
   *
   * @param epochDays the Epoch Day to convert, based on the epoch 0000-01-01
   * @return the local date, never null
   * @throws IllegalCalendarFieldValueException if the epoch days exceeds the supported date range
   */
  private[calendar] def ofYearZeroDays(_epochDays: Long): LocalDate = {
    var epochDays = _epochDays
    epochDays -= 60
    var adjust: Long = 0
    if (epochDays < 0) {
      val adjustCycles: Long = (epochDays + 1) / ISOChronology.DaysPerCycle - 1
      adjust = adjustCycles * 400
      epochDays += -adjustCycles * ISOChronology.DaysPerCycle
    }
    var yearEst: Long = (400 * epochDays + 591) / ISOChronology.DaysPerCycle
    var doyEst: Long = epochDays - (365 * yearEst + yearEst / 4 - yearEst / 100 + yearEst / 400)
    if (doyEst < 0) {
      yearEst -= 1;
      doyEst = epochDays - (365 * yearEst + yearEst / 4 - yearEst / 100 + yearEst / 400)
    }
    yearEst += adjust
    val marchDoy0: Int = doyEst.asInstanceOf[Int]
    val marchMonth0: Int = (marchDoy0 * 5 + 2) / 153
    val month: Int = (marchMonth0 + 2) % 12 + 1
    val dom: Int = marchDoy0 - (marchMonth0 * 306 + 5) / 10 + 1
    yearEst += marchMonth0 / 10
    val year: Int = ISOChronology.yearRule.checkValue(yearEst)
    new LocalDate(year, MonthOfYear.of(month), dom)
  }

  /**
   * Obtains an instance of {@code LocalDate} from a text string using a specific formatter.
   * <p>
   * The text is parsed using the formatter, returning a date.
   *
   * @param text the text to parse, not null
   * @param formatter the formatter to use, not null
   * @return the parsed local date, never null
   * @throws UnsupportedOperationException if the formatter cannot parse
   * @throws CalendricalException if the text cannot be parsed
   */
  def parse(text: String, formatter: DateTimeFormatter): LocalDate = {
    ISOChronology.checkNotNull(formatter, "DateTimeFormatter must not be null")
    formatter.parse(text, rule)
  }

  /**
   * Obtains an instance of {@code LocalDate} from the epoch days count.
   * <p>
   * The Epoch Day count is a simple incrementing count of days
   * where day 0 is 1970-01-01.
   *
   * @param epochDays the Epoch Day to convert, based on the epoch 1970-01-01
   * @return the local date, never null
   * @throws IllegalCalendarFieldValueException if the epoch days exceeds the supported date range
   */
  def ofEpochDays(epochDays: Long): LocalDate = ofYearZeroDays(epochDays + ISOChronology.Days0000To1970)

  /**
   * Gets the field rule for {@code LocalDate}.
   *
   * @return the field rule for the date, never null
   */
  def rule: CalendricalRule[LocalDate] = Rule

  /**
   * Creates a local date from the year, month and day fields.
   *
   * @param year the year to represent, validated from MIN_YEAR to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, validated not null
   * @param dayOfMonth the day-of-month to represent, validated from 1 to 31
   * @return the local date, never null
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  private def create(year: Int, monthOfYear: MonthOfYear, dayOfMonth: Int): LocalDate = {
    if (dayOfMonth > 28 && dayOfMonth > monthOfYear.lengthInDays(ISOChronology.isLeapYear(year))) {
      if (dayOfMonth == 29) {
        throw new InvalidCalendarFieldException("Illegal value for DayOfMonth field, value 29 is not valid as " + year + " is not a leap year", ISOChronology.dayOfMonthRule)
      }
      else {
        throw new InvalidCalendarFieldException("Illegal value for DayOfMonth field, value " + dayOfMonth + " is not valid for month " + monthOfYear.name, ISOChronology.dayOfMonthRule)
      }
    }
    return new LocalDate(year, monthOfYear, dayOfMonth)
  }
}

/**
 * Constructor, previously validated.
 *
 * @param year the year to represent, from MIN_YEAR to MAX_YEAR
 * @param monthOfYear the month-of-year to represent, not null
 * @param dayOfMonth the day-of-month to represent, valid for year-month, from 1 to 31
 */
final class LocalDate private(val year: Int, val month: MonthOfYear, val day: Int)
  extends Calendrical with DateProvider with CalendricalMatcher with DateAdjuster with Comparable[LocalDate] with Serializable {
  /**
   * Resolves the date, handling incorrectly implemented resolvers.
   *
   * @param dateResolver the resolver, not null
   * @param year the year, from MIN_YEAR to MAX_YEAR
   * @param month the month, not null
   * @param day the day-of-month, from 1 to 31
   * @return the resolved date, never null
   * @throws NullPointerException if the resolver returned null
   */
  private def resolveDate(dateResolver: DateResolver, year: Int, month: MonthOfYear, day: Int): LocalDate = {
    ISOChronology.yearRule.checkValue(year)
    ISOChronology.dayOfMonthRule.checkValue(day)
    val date: LocalDate = dateResolver.resolveDate(year, month, day)
    ISOChronology.checkNotNull(date, "DateResolver implementation must not return null")
    return date
  }

  /**
   * A hash code for this {@code LocalDate}.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = {
    val yearValue: Int = year
    val monthValue: Int = month.getValue
    val dayValue: Int = day
    return (yearValue & 0xFFFFF800) ^ ((yearValue << 11) + (monthValue << 6) + (dayValue))
  }

  /**
   * Returns a copy of this {@code LocalDate} with the year altered.
   * If the resulting date is invalid, it will be resolved using {@code dateResolver}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param year the year to set in the returned date, from MIN_YEAR to MAX_YEAR
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return a {@code LocalDate} based on this date with the requested year, never null
   * @throws IllegalCalendarFieldValueException if the year value is invalid
   */
  def withYear(year: Int, dateResolver: DateResolver): LocalDate = {
    ISOChronology.checkNotNull(dateResolver, "DateResolver must not be null")
    if (this.year == year) this
    else resolveDate(dateResolver, year, month, day)
  }

  /**
   * Returns a local date-time formed from this date at the time of midnight.
   * <p>
   * This merges the two objects - {@code this} and {@link LocalTime#MIDNIGHT} -
   * to form an instance of {@code LocalDateTime}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @return the local date-time formed from this date and the time of midnight, never null
   */
  def atMidnight: LocalDateTime = {
    return LocalDateTime.of(this, LocalTime.Midnight)
  }

  /**
   * Returns a copy of this {@code LocalDate} with the specified date period added.
   * <p>
   * This adds the specified period to this date, returning a new date.
   * Before addition, the period is converted to a date-based {@code Period} using
   * {@link Period#ofDateFields ( PeriodProvider )}.
   * That factory ignores any time-based ISO fields, thus adding a time-based
   * period to this date will have no effect. If you want to take time fields into
   * account, call {@link Period#normalizedWith24HourDays()} on the input period.
   * <p>
   * The detailed rules for the addition have some complexity due to variable length months.
   * The goal is to match the code for {@code plusYears ( ).plusMonths ( ).plusDays()} in most cases.
   * The principle case of difference is best expressed by example:
   * {@code 2010 -01-31} plus {@code P1M -1M} yields {@code 2010 -02-28} whereas
   * {@code plusMonths ( 1 ).plusDays ( - 1 )} gives {@code 2010 -02-27}.
   * <p>
   * The rules are expressed in five steps:
   * <ol>
   * <li>Add the input years and months to calculate the resulting year-month</li>
   * <li>Form an imaginary date from the year-month and the original day-of-month,
   *  a date that may be invalid, such as February 30th</li>
   * <li>Add the input days to the imaginary date treating the first move to a later date
   *  from an invalid date as a move to the 1st of the next month</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the day-of-month to the last valid day if necessary</li>
   * </ol>
   * <p>
   * For example, this table shows what happens when for various inputs and periods:
   * <pre>
   *   2010-01-30 plus P1M2D  = 2010-03-02
   *   2010-01-30 plus P1M1D  = 2010-03-01
   *   2010-01-30 plus P1M    = 2010-02-28
   *   2010-01-30 plus P1M-1D = 2010-02-28
   *   2010-01-30 plus P1M-2D = 2010-02-28
   *   2010-01-30 plus P1M-3D = 2010-02-27
   * </pre>
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to add, not null
   * @return a {@code LocalDate} based on this date with the period added, never null
   * @throws CalendricalException if the specified period cannot be converted to a {@code Period }
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plus(periodProvider: PeriodProvider): LocalDate = {
    val period: Period = Period.ofDateFields(periodProvider)
    var periodMonths: Long = period.totalMonths
    var periodDays: Long = period.getDays
    if (periodMonths == 0) return plusDays(periodDays)
    val monthCount: Long = (year.asInstanceOf[Long]) * 12 + (month.getValue - 1)
    val calcMonths: Long = monthCount + periodMonths
    val newYear: Int = ISOChronology.yearRule.checkValue(MathUtils.floorDiv(calcMonths, 12))
    val newMonth: MonthOfYear = MonthOfYear.of(MathUtils.floorMod(calcMonths, 12) + 1)
    val newMonthLen: Int = newMonth.lengthInDays(ISOChronology.isLeapYear(newYear))
    val newDay: Int = math.min(day, newMonthLen)
    if (periodDays < 0 && day > newMonthLen) periodDays = math.min(periodDays + (day - newMonthLen), 0)
    return LocalDate.of(newYear, newMonth, newDay).plusDays(periodDays)
  }

  def +(periodProvider: PeriodProvider): LocalDate = plus(periodProvider)

  /**
   * Returns a zoned date-time from this date at the earliest valid time according
   * to the rules in the time-zone.
   * <p>
   * Time-zone rules, such as daylight savings, mean that not every time on the
   * local time-line exists. When this method converts the date to a date-time it
   * adjusts the time and offset as necessary to ensure that the time is as early
   * as possible on the date, which is typically midnight. Internally this is
   * achieved using a {@link ZoneResolvers#postGapPreOverlap ( ) zone resolver}.
   * <p>
   * To convert to a specific time in a given time-zone call {@link #atTime ( LocalTime ) }
   * followed by {@link LocalDateTime#atZone ( TimeZone )}. Note that the resolver used
   * by {@code atZone()} is different to that used here (it chooses the later
   * offset in an overlap, whereas this method chooses the earlier offset).
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param zone the time-zone to use, not null
   * @return the zoned date-time formed from this date and the earliest valid time for the zone, never null
   */
  def atStartOfDayInZone(zone: TimeZone): ZonedDateTime = {
    ZonedDateTime.of(this, LocalTime.Midnight, zone, ZoneResolvers.postGapPreOverlap)
  }

  /**
   * Returns a copy of this {@code LocalDate} with the specified period in months added.
   * <p>
   * This method add the specified amount to the months field in three steps:
   * <ol>
   * <li>Add the input months to the month-of-year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the date using {@code dateResolver} if necessary</li>
   * </ol>
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to add, may be negative
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return a {@code LocalDate} based on this date with the months added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusMonths(months: Long, dateResolver: DateResolver): LocalDate = {
    ISOChronology.checkNotNull(dateResolver, "DateResolver must not be null")
    if (months == 0) return this
    val monthCount: Long = year * 12L + (month.getValue - 1)
    val calcMonths: Long = monthCount + months
    val newYear: Int = ISOChronology.yearRule.checkValue(MathUtils.floorDiv(calcMonths, 12))
    val newMonth: MonthOfYear = MonthOfYear.of(MathUtils.floorMod(calcMonths, 12) + 1)
    return resolveDate(dateResolver, newYear, newMonth, day)
  }

  /**
   * Outputs this date as a {@code String}, such as {@code 2007 -12-03}.
   * <p>
   * The output will be in the format {@code yyyy -MM-dd}.
   *
   * @return the formatted date, never null
   */
  override def toString: String = {
    val yearValue: Int = year
    val monthValue: Int = month.getValue
    val dayValue: Int = day
    val absYear: Int = math.abs(yearValue)
    val buf: StringBuilder = new StringBuilder(10)
    if (absYear < 1000) {
      if (yearValue < 0) {
        buf.append(yearValue - 10000).deleteCharAt(1)
      }
      else {
        buf.append(yearValue + 10000).deleteCharAt(0)
      }
    }
    else {
      if (yearValue > 9999) {
        buf.append('+')
      }
      buf.append(yearValue)
    }
    return buf.append(if (monthValue < 10) "-0" else "-").append(monthValue).append(if (dayValue < 10) "-0" else "-").append(dayValue).toString
  }

  /**
   * Returns a copy of this {@code LocalDate} with the specified period in weeks subtracted.
   * <p>
   * This method subtract the specified amount in weeks to the days field decrementing
   * the month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2009-01-07 minus one week would result in 2008-12-31.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param weeks the weeks to subtract, may be negative
   * @return a {@code LocalDate} based on this date with the weeks subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusWeeks(weeks: Long): LocalDate = {
    try {
      return minusDays(MathUtils.safeMultiply(weeks, 7))
    }
    catch {
      case ae: ArithmeticException => {
        throw new CalendricalException(this + " - " + weeks + " weeks exceeds capacity")
      }
    }
  }

  /**
   * Returns a local date-time formed from this date at the specified time.
   * <p>
   * This merges the three values - {@code this} and the specified time -
   * to form an instance of {@code LocalDateTime}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay the hour-of-day to use, from 0 to 23
   * @param minuteOfHour the minute-of-hour to use, from 0 to 59
   * @return the local date-time formed from this date and the specified time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   */
  def atTime(hourOfDay: Int, minuteOfHour: Int): LocalDateTime = atTime(LocalTime.of(hourOfDay, minuteOfHour))

  /**
   * Checks whether this {@code LocalDate} matches the specified matcher.
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
   * Outputs this date as a {@code String} using the formatter.
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
   * Returns a copy of this {@code LocalDate} with the specified period in weeks added.
   * <p>
   * This method add the specified amount in weeks to the days field incrementing
   * the month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2008-12-31 plus one week would result in 2009-01-07.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param weeks the weeks to add, may be negative
   * @return a {@code LocalDate} based on this date with the weeks added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusWeeks(weeks: Long): LocalDate = {
    try {
      return plusDays(MathUtils.safeMultiply(weeks, 7))
    }
    catch {
      case ae: ArithmeticException => {
        throw new CalendricalException(this + " + " + weeks + " weeks exceeds capacity")
      }
    }
  }

  /**
   * Returns a copy of this {@code LocalDate} with the day-of-month altered.
   * If the resulting date is invalid, it will be resolved using {@code dateResolver}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfMonth the day-of-month to set in the returned date, from 1 to 31
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return a {@code LocalDate} based on this date with the requested day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-month value is invalid
   */
  def withDayOfMonth(dayOfMonth: Int, dateResolver: DateResolver): LocalDate = {
    ISOChronology.checkNotNull(dateResolver, "DateResolver must not be null")
    if (this.day == dayOfMonth) {
      return this
    }
    return resolveDate(dateResolver, year, month, dayOfMonth)
  }

  /**
   * Adjusts a date to have the value of this date.
   *
   * @param date the date to be adjusted, not null
   * @return the adjusted date, never null
   */
  override def adjustDate(date: LocalDate): LocalDate = {
    ISOChronology.checkNotNull(date, "LocalDate must not be null")
    return if (this.equals(date)) date else this
  }

  /**
   * Returns a copy of this {@code LocalDate} with the month-of-year altered.
   * If the resulting date is invalid, it will be resolved using {@code dateResolver}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear the month-of-year to set in the returned date, from 1 (January) to 12 (December)
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return a {@code LocalDate} based on this date with the requested month, never null
   * @throws IllegalCalendarFieldValueException if the month-of-year value is invalid
   */
  def withMonthOfYear(monthOfYear: Int, dateResolver: DateResolver): LocalDate = {
    return `with`(MonthOfYear.of(monthOfYear), dateResolver)
  }

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
  def isLeapYear: Boolean = ISOChronology.isLeapYear(year)

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
  def getYear: Int = year

  /**
   * Returns a copy of this {@code LocalDate} with the day-of-year altered.
   * If the resulting date is invalid, an exception is thrown.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfYear the day-of-year to set in the returned date, from 1 to 365-366
   * @return a {@code LocalDate} based on this date with the requested day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-year value is invalid
   * @throws InvalidCalendarFieldException if the day-of-year is invalid for the year
   */
  def withDayOfYear(dayOfYear: Int): LocalDate = {
    if (this.getDayOfYear == dayOfYear) this
    else ISOChronology.getDateFromDayOfYear(year, dayOfYear)
  }

  /**
   * Returns a copy of this {@code LocalDate} with the specified period in months subtracted.
   * <p>
   * This method subtract the specified amount to the months field in three steps:
   * <ol>
   * <li>Subtract the input months to the month-of-year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the date using {@code dateResolver} if necessary</li>
   * </ol>
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to subtract, may be negative
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return a {@code LocalDate} based on this date with the months subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusMonths(months: Long, dateResolver: DateResolver): LocalDate = {
    ISOChronology.checkNotNull(dateResolver, "DateResolver must not be null")
    if (months == 0) return this
    val monthCount: Long = year * 12L + (month.getValue - 1)
    val calcMonths: Long = monthCount - months
    val newYear: Int = ISOChronology.yearRule.checkValue(MathUtils.floorDiv(calcMonths, 12))
    val newMonth: MonthOfYear = MonthOfYear.of(MathUtils.floorMod(calcMonths, 12) + 1)
    return resolveDate(dateResolver, newYear, newMonth, day)
  }

  /**
   * Gets the day-of-month field.
   * <p>
   * This method returns the primitive {@code int} value for the day-of-month.
   *
   * @return the day-of-month, from 1 to 31
   */
  def getDayOfMonth: Int = day

  /**
   * Returns a copy of this {@code LocalDate} with the specified period in months subtracted.
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
   * This method does the same as {@code minusMonts ( months, DateResolvers.previousValid ( ) )}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to subtract, may be negative
   * @return a {@code LocalDate} based on this date with the months subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   * @see#minusMonths ( long, javax.time.calendar.DateResolver )
   */
  def minusMonths(months: Long): LocalDate = minusMonths(months, DateResolvers.previousValid)

  /**
   * Returns a local date-time formed from this date at the specified time.
   * <p>
   * This merges the four values - {@code this} and the specified time -
   * to form an instance of {@code LocalDateTime}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay the hour-of-day to use, from 0 to 23
   * @param minuteOfHour the minute-of-hour to use, from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @return the local date-time formed from this date and the specified time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   */
  def atTime(hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int): LocalDateTime = {
    atTime(LocalTime.of(hourOfDay, minuteOfHour, secondOfMinute))
  }

  /**
   * Checks if the date extracted from the calendrical matches this date.
   * <p>
   * This method implements the {@code CalendricalMatcher} interface.
   * It is intended that applications use {@link #matches} rather than this method.
   *
   * @param calendrical the calendrical to match, not null
   * @return true if the calendrical matches, false otherwise
   */
  override def matchesCalendrical(calendrical: Calendrical): Boolean = this.equals(calendrical.get(LocalDate.rule))

  /**
   * Returns an offset date formed from this time and the specified offset.
   * <p>
   * This merges the two objects - {@code this} and the specified offset -
   * to form an instance of {@code OffsetDate}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param offset the offset to use, not null
   * @return the offset date formed from this date and the specified offset, never null
   */
  def atOffset(offset: ZoneOffset): OffsetDate = OffsetDate.of(this, offset)

  /**
   * Returns a copy of this {@code LocalDate} with the day-of-month altered.
   * If the resulting date is invalid, an exception is thrown.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfMonth the day-of-month to set in the returned date, from 1 to 28-31
   * @return a {@code LocalDate} based on this date with the requested day, never null
   * @throws IllegalCalendarFieldValueException if the day-of-month value is invalid
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month-year
   */
  def withDayOfMonth(dayOfMonth: Int): LocalDate = {
    if (this.day == dayOfMonth) this
    else LocalDate.of(year, month, dayOfMonth)
  }

  /**
   * Returns a copy of this {@code LocalDate} with the specified period in years subtracted.
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
   * This method does the same as {@code minusYears ( years, DateResolvers.previousValid ( ) )}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to subtract, may be negative
   * @return a {@code LocalDate} based on this date with the years subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   * @see#minusYears ( long, javax.time.calendar.DateResolver )
   */
  def minusYears(years: Long): LocalDate = minusYears(years, DateResolvers.previousValid)

  /**
   * Converts this date to a {@code LocalDate}, trivially
   * returning {@code this}.
   *
   * @return {@code this}, never null
   */
  override def toLocalDate: LocalDate = this

  /**
   * Returns a copy of this {@code LocalDate} with the month-of-year altered.
   * If the resulting date is invalid, it will be resolved using {@link DateResolvers#previousValid()}.
   * <p>
   * This method does the same as {@code withMonthOfYear ( monthOfYear, DateResolvers.previousValid ( ) )}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear the month-of-year to set in the returned date, from 1 (January) to 12 (December)
   * @return a {@code LocalDate} based on this date with the requested month, never null
   * @throws IllegalCalendarFieldValueException if the month-of-year value is invalid
   */
  def withMonthOfYear(monthOfYear: Int): LocalDate = `with`(MonthOfYear.of(monthOfYear), DateResolvers.previousValid)

  /**
   * Converts this {@code LocalDate} to Epoch Days.
   * <p>
   * The Epoch Day count is a simple incrementing count of days
   * where day 0 is 1970-01-01.
   *
   * @return the Modified Julian Day equivalent to this date
   */
  def toEpochDays: Long = toYearZeroDays - ISOChronology.Days0000To1970

  /**
   * Returns a copy of this {@code LocalDate} with the month-of-year altered.
   * If the resulting date is invalid, it will be resolved using {@code dateResolver}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear the month-of-year to set in the returned date, not null
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return a {@code LocalDate} based on this date with the requested month, never null
   */
  def `with`(monthOfYear: MonthOfYear, dateResolver: DateResolver): LocalDate = {
    ISOChronology.checkNotNull(monthOfYear, "MonthOfYear must not be null")
    ISOChronology.checkNotNull(dateResolver, "DateResolver must not be null")
    if (this.month == monthOfYear) this
    else resolveDate(dateResolver, year, monthOfYear, day)
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
  def getMonthOfYear: MonthOfYear = month

  /**
   * Converts this {@code LocalDate} to year zero days.
   * <p>
   * The year zero day count is a simple incrementing count of days
   * where day 0 is 0000-01-01.
   *
   * @return the year zero days count equal to this date
   */
  private[calendar] def toYearZeroDays: Long = {
    val y: Long = year
    val m: Long = month.getValue
    var total: Long = 0
    total += 365 * y
    if (y >= 0) total += (y + 3) / 4 - (y + 99) / 100 + (y + 399) / 400
    else total -= y / -4 - y / -100 + y / -400
    total += ((367 * m - 362) / 12)
    total += day - 1
    if (m > 2) {
      ({
        total -= 1;
        total
      })
      if (ISOChronology.isLeapYear(year) == false) {
        ({
          total -= 1;
          total
        })
      }
    }
    return total
  }

  /**
   * Returns a copy of this {@code LocalDate} with the specified period in years subtracted.
   * <p>
   * This method subtract the specified amount to the years field in three steps:
   * <ol>
   * <li>Subtract the input years to the year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the date using {@code dateResolver} if necessary</li>
   * </ol>
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to subtract, may be negative
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return a {@code LocalDate} based on this date with the years subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusYears(years: Long, dateResolver: DateResolver): LocalDate = {
    ISOChronology.checkNotNull(dateResolver, "DateResolver must not be null")
    if (years == 0) return this
    val newYear: Int = ISOChronology.yearRule.checkValue(year - years)
    resolveDate(dateResolver, newYear, month, day)
  }

  /**
   * Returns a copy of this {@code LocalDate} with the specified period in years added.
   * <p>
   * This method add the specified amount to the years field in three steps:
   * <ol>
   * <li>Add the input years to the year field</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the date using {@code dateResolver} if necessary</li>
   * </ol>
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to add, may be negative
   * @param dateResolver the DateResolver to be used if the resulting date would be invalid
   * @return a {@code LocalDate} based on this date with the years added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusYears(years: Long, dateResolver: DateResolver): LocalDate = {
    ISOChronology.checkNotNull(dateResolver, "DateResolver must not be null")
    if (years == 0) return this
    val newYear: Int = ISOChronology.yearRule.checkValue(year + years)
    resolveDate(dateResolver, newYear, month, day)
  }

  /**
   * Gets the value of the specified calendrical rule.
   * <p>
   * This method queries the value of the specified calendrical rule.
   * If the value cannot be returned for the rule from this date then
   * {@code null} will be returned.
   *
   * @param rule the rule to use, not null
   * @return the value for the rule, null if the value cannot be returned
   */
  //  def get[T](rule: CalendricalRule[T]): Option[T] = Some(rule.deriveValueFor(rule, this, this, ISOChronology))  //FIXME
  def get[T](rule: CalendricalRule[T]): Option[T] = None

  /**
   * Gets the chronology that this date uses, which is the ISO calendar system.
   *
   * @return the ISO chronology, never null
   */
  def getChronology: ISOChronology = ISOChronology

  /**
   * Returns a copy of this {@code LocalDate} with the specified period in months added.
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
   * This method does the same as {@code plusMonths(months, DateResolvers.previousValid())}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to add, may be negative
   * @return a {@code LocalDate} based on this date with the months added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   * @see #plusMonths(long,javax.time.calendar.DateResolver)
   */
  def plusMonths(months: Long): LocalDate = {
    return plusMonths(months, DateResolvers.previousValid)
  }

  /**
   * Checks if this {@code LocalDate} is after the specified date.
   * <p>
   * The comparison is based on the time-line position of the dates.
   *
   * @param other the other date to compare to, not null
   * @return true if this is after the specified date
   * @throws NullPointerException if {@code other} is null
   */
  def isAfter(other: LocalDate): Boolean = {
    return compareTo(other) > 0
  }

  /**
   * Returns a copy of this {@code LocalDate} with the date altered using the adjuster.
   * <p>
   * Adjusters can be used to alter the date in various ways.
   * A simple adjuster might simply set the one of the fields, such as the year field.
   * A more complex adjuster might set the date to the last day of the month.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param adjuster the adjuster to use, not null
   * @return a {@code LocalDate} based on this date adjusted as necessary, never null
   * @throws NullPointerException if the adjuster returned null
   */
  def `with`(adjuster: DateAdjuster): LocalDate = {
    ISOChronology.checkNotNull(adjuster, "DateAdjuster must not be null")
    val date: LocalDate = adjuster.adjustDate(this)
    ISOChronology.checkNotNull(date, "DateAdjuster implementation must not return null")
    date
  }

  /**
   * Returns a copy of this {@code LocalDate} with the specified date period subtracted.
   * <p>
   * This subtracts the specified period from this date, returning a new date.
   * Before subtraction, the period is converted to a date-based {@code Period} using
   * {@link Period#ofDateFields(PeriodProvider)}.
   * That factory ignores any time-based ISO fields, thus subtracting a time-based
   * period from this date will have no effect. If you want to take time fields into
   * account, call {@link Period#normalizedWith24HourDays()} on the input period.
   * <p>
   * The detailed rules for the addition have some complexity due to variable length months.
   * The goal is to match the code for {@code minusYears().minusMonths().minusDays()} in most cases.
   * The principle case of difference is best expressed by example:
   * {@code 2010 -03-31} minus {@code P1M1M} yields {@code 2010 -02-28} whereas
   * {@code minusMonths(1).minusDays(1)} gives {@code 2010 -02-27}.
   * <p>
   * The rules are expressed in five steps:
   * <ol>
   * <li>Subtract the input years and months to calculate the resulting year-month</li>
   * <li>Form an imaginary date from the year-month and the original day-of-month,
   *  a date that may be invalid, such as February 30th</li>
   * <li>Subtract the input days from the imaginary date treating the first move to a later date
   *  from an invalid date as a move to the 1st of the next month</li>
   * <li>Check if the resulting date would be invalid</li>
   * <li>Adjust the day-of-month to the last valid day if necessary</li>
   * </ol>
   * <p>
   * For example, this table shows what happens when for various inputs and periods:
   * <pre>
   *   2010-03-30 minus P1M3D  = 2010-02-27
   *   2010-03-30 minus P1M2D  = 2010-02-28
   *   2010-03-30 minus P1M1D  = 2010-02-28
   *   2010-03-30 minus P1M    = 2010-02-28
   *   2010-03-30 minus P1M-1D = 2010-03-01
   *   2010-03-30 minus P1M-2D = 2010-03-02
   * </pre>
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to subtract, not null
   * @return a {@code LocalDate} based on this date with the period subtracted, never null
   * @throws CalendricalException if the specified period cannot be converted to a {@code Period}
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minus(periodProvider: PeriodProvider): LocalDate = {
    val period: Period = Period.ofDateFields(periodProvider)
    val periodMonths: Long = period.totalMonths
    var periodDays: Long = period.getDays
    if (periodMonths == 0) return minusDays(periodDays)
    val monthCount: Long = (year.asInstanceOf[Long]) * 12 + (month.getValue - 1)
    val calcMonths: Long = monthCount - periodMonths
    val newYear: Int = ISOChronology.yearRule.checkValue(MathUtils.floorDiv(calcMonths, 12))
    val newMonth: MonthOfYear = MonthOfYear.of(MathUtils.floorMod(calcMonths, 12) + 1)
    val newMonthLen: Int = newMonth.lengthInDays(ISOChronology.isLeapYear(newYear))
    val newDay: Int = math.min(day, newMonthLen)
    if (periodDays > 0 && day > newMonthLen) periodDays = math.max(periodDays - (day - newMonthLen), 0)
    return LocalDate.of(newYear, newMonth, newDay).minusDays(periodDays)
  }

  def -(periodProvider: PeriodProvider): LocalDate = minus(periodProvider)

  /**
   * Checks if this {@code LocalDate} is equal to the specified date.
   * <p>
   * The comparison is based on the time-line position of the dates.
   *
   * @param other the other date to compare to, null returns false
   * @return true if this point is equal to the specified date
   */
  override def equals(other: AnyRef): Boolean = {
    if (this eq other) true
    else if (other.isInstanceOf[LocalDate]) {
      val otherDate: LocalDate = other.asInstanceOf[LocalDate]
      (year == otherDate.year && month == otherDate.month && day == otherDate.day)
    }
    else false
  }

  /**
   * Checks if this {@code LocalDate} is before the specified date.
   * <p>
   * The comparison is based on the time-line position of the dates.
   *
   * @param other the other date to compare to, not null
   * @return true if this point is before the specified date
   * @throws NullPointerException if {@code other} is null
   */
  def isBefore(other: LocalDate): Boolean = compareTo(other) < 0

  /**
   * Returns a copy of this {@code LocalDate} with the month-of-year altered.
   * If the resulting date is invalid, it will be resolved using {@link DateResolvers#previousValid()}.
   * <p>
   * This method does the same as {@code with ( monthOfYear, DateResolvers.previousValid ( ) )}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param monthOfYear the month-of-year to set in the returned date, not null
   * @return a {@code LocalDate} based on this date with the requested month, never null
   */
  def `with`(monthOfYear: MonthOfYear): LocalDate = `with`(monthOfYear, DateResolvers.previousValid)

  /**
   * Returns a copy of this {@code LocalDate} with the specified period in years added.
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
   * This method does the same as {@code plusYears ( years, DateResolvers.previousValid ( ) )}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to add, may be negative
   * @return a {@code LocalDate} based on this date with the years added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   * @see#plusYears ( long, javax.time.calendar.DateResolver )
   */
  def plusYears(years: Long): LocalDate = plusYears(years, DateResolvers.previousValid)

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
  def getDayOfWeek: DayOfWeek = ISOChronology.getDayOfWeekFromDate(this)

  /**
   * Compares this {@code LocalDate} to another date.
   * <p>
   * The comparison is based on the time-line position of the dates.
   *
   * @param other the other date to compare to, not null
   * @return the comparator value, negative if less, positive if greater
   * @throws NullPointerException if {@code other} is null
   */
  def compareTo(other: LocalDate): Int = {
    var cmp: Int = MathUtils.safeCompare(year, other.year)
    if (cmp == 0) {
      cmp = month.compareTo(other.month)
      if (cmp == 0) {
        cmp = MathUtils.safeCompare(day, other.day)
      }
    }
    return cmp
  }

  /**
   * Gets the day-of-year field.
   * <p>
   * This method returns the primitive {@code int} value for the day-of-year.
   *
   * @return the day-of-year, from 1 to 365, or 366 in a leap year
   */
  def getDayOfYear: Int = ISOChronology.getDayOfYearFromDate(this)

  /**
   * Returns a local date-time formed from this date at the specified time.
   * <p>
   * This merges the two objects - {@code this} and the specified time -
   * to form an instance of {@code LocalDateTime}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param time the time to use, not null
   * @return the local date-time formed from this date and the specified time, never null
   */
  def atTime(time: LocalTime): LocalDateTime = LocalDateTime.of(this, time)

  /**
   * Converts this {@code LocalDate} to Modified Julian Days (MJD).
   * <p>
   * The Modified Julian Day count is a simple incrementing count of days
   * where day 0 is 1858-11-17.
   *
   * @return the Modified Julian Day equivalent to this date
   */
  def toModifiedJulianDays: Long = toYearZeroDays - ISOChronology.Days0000ToModifiedJulianDaysEpoch

  /**
   * Returns a copy of this {@code LocalDate} with the year altered.
   * If the resulting date is invalid, it will be resolved using {@link DateResolvers#previousValid()}.
   * <p>
   * This method does the same as {@code withYear ( year, DateResolvers.previousValid ( ) )}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param year the year to set in the returned date, from MIN_YEAR to MAX_YEAR
   * @return a {@code LocalDate} based on this date with the requested year, never null
   * @throws IllegalCalendarFieldValueException if the year value is invalid
   */
  def withYear(year: Int): LocalDate = withYear(year, DateResolvers.previousValid)

  /**
   * Returns a copy of this {@code LocalDate} with the specified number of days subtracted.
   * <p>
   * This method subtract the specified amount to the days field decrementing the
   * month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2009-01-01 minus one day would result in 2008-12-31.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param days the days to subtract, may be negative
   * @return a {@code LocalDate} based on this date with the days subtracted, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def minusDays(days: Long): LocalDate = {
    if (days == 0) return this
    var mjDays: Long = toModifiedJulianDays
    try {
      mjDays = MathUtils.safeSubtract(mjDays, days)
    }
    catch {
      case ae: ArithmeticException => {
        throw new CalendricalException(this + " - " + days + " days exceeds capacity")
      }
    }
    return LocalDate.ofModifiedJulianDays(mjDays)
  }

  /**
   * Returns a copy of this {@code LocalDate} with the specified number of days added.
   * <p>
   * This method add the specified amount to the days field incrementing the
   * month and year fields as necessary to ensure the result remains valid.
   * The result is only invalid if the maximum/minimum year is exceeded.
   * <p>
   * For example, 2008-12-31 plus one day would result in 2009-01-01.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param days the days to add, may be negative
   * @return a {@code LocalDate} based on this date with the days added, never null
   * @throws CalendricalException if the result exceeds the supported date range
   */
  def plusDays(days: Long): LocalDate = {
    if (days == 0) return this
    var mjDays: Long = toModifiedJulianDays
    try {
      mjDays = MathUtils.safeAdd(mjDays, days)
    }
    catch {
      case ae: ArithmeticException => {
        throw new CalendricalException(this + " + " + days + " days exceeds capacity")
      }
    }
    return LocalDate.ofModifiedJulianDays(mjDays)
  }

  /**
   * Returns a local date-time formed from this date at the specified time.
   * <p>
   * This merges the five values - {@code this} and the specified time -
   * to form an instance of {@code LocalDateTime}.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param hourOfDay the hour-of-day to use, from 0 to 23
   * @param minuteOfHour the minute-of-hour to use, from 0 to 59
   * @param secondOfMinute the second-of-minute to represent, from 0 to 59
   * @param nanoOfSecond the nano-of-second to represent, from 0 to 999,999,999
   * @return the local date-time formed from this date and the specified time, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   */
  def atTime(hourOfDay: Int, minuteOfHour: Int, secondOfMinute: Int, nanoOfSecond: Int): LocalDateTime = {
    atTime(LocalTime.of(hourOfDay, minuteOfHour, secondOfMinute, nanoOfSecond))
  }

  /**
   * Gets the year field as a {@code Year}.
   * <p>
   * This method provides access to an object representing the year field.
   * {@code Year} has methods for querying addition year-based information.
   *
   * @return the year, never null
   */
  def toYear: Year = Year.of(year)
}