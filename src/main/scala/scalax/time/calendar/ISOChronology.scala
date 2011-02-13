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

import java.text.DateFormatSymbols
import java.util.Calendar
import java.util.Locale
import scalax.time.Duration
import scalax.time.MathUtils
import scalax.time.calendar.format.DateTimeFormatterBuilder.TextStyle
import scalax.time.calendar.DateTimeFieldRule.TextStore

import collection.mutable.HashMap

/**
 * The ISO-8601 calendar system, which follows the rules of the current
 * <i>de facto</i> world calendar.
 * <p>
 * ISOChronology follows the rules of the Gregorian calendar for all time.
 * Thus, dates is the past, and particularly before 1583, may not correspond
 * to historical documents.
 * <p>
 * ISOChronology is immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */

/**
 * The singleton instance of {@code ISOChronology}.
 */
object ISOChronology extends ISOChronology {
  /**
   * Checks if the specified year is a leap year according to the ISO calendar system rules.
   * <p>
   * The ISO calendar system applies the current rules for leap years across the whole time-line.
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
   * @param year  the year to check, not validated for range
   * @return true if the year is a leap year
   */
  def isLeapYear(year: Int): Boolean = ((year & 3) == 0) && ((year % 100) != 0 || (year % 400) == 0)

  /**
   * Validates that the input value is not null.
   *
   * @param object  the object to check
   * @param errorMessage  the error to throw
   * @throws NullPointerException if the object is null
   */
  private[calendar] def checkNotNull(obj: Any, errorMessage: String): Unit = {
    if (obj == null) throw new NullPointerException(errorMessage)
  }

  /**
   * Calculates the day-of-week from a date.
   *
   * @param date  the date to use, not null
   * @return the day-of-week
   */
  private[calendar] def getDayOfWeekFromDate(date: Date): DayOfWeek = {
    var mjd: Long = date.toModifiedJulianDays
    if (mjd < 0) {
      val weeks: Long = mjd / 7
      mjd += (-weeks + 1) * 7
    }
    val dow0: Int = ((mjd + 2) % 7).asInstanceOf[Int]
    return DayOfWeek.of(dow0 + 1)
  }

  /**
   * Calculates the day-of-year from a date.
   *
   * @param date  the date to use, not null
   * @return the day-of-year
   */
  private[calendar] def getDayOfYearFromDate(date: Date): Int = date.getMonthOfYear.monthStartDayOfYear(date.isLeapYear) + date.getDayOfMonth - 1

  /**
   * Calculates the date from a year and day-of-year.
   *
   * @param year  the year, valid
   * @param dayOfYear  the day-of-year, valid
   * @return the date, never null
   */
  private[calendar] def getDateFromDayOfYear(year: Int, dayOfYear: Int): Date = {
    dayOfYearRule.checkValue(dayOfYear)
    var leap: Boolean = ISOChronology.isLeapYear(year)
    if (dayOfYear == 366 && leap == false) {
      throw new InvalidCalendarFieldException("DayOfYear 366 is invalid for year " + year, dayOfYearRule)
    }
    var moy: MonthOfYear = MonthOfYear.of((dayOfYear - 1) / 31 + 1)
    val monthEnd: Int = moy.monthEndDayOfYear(leap)
    if (dayOfYear > monthEnd) {
      moy = moy.next
    }
    val dom: Int = dayOfYear - moy.monthStartDayOfYear(leap) + 1
    return Date(year, moy, dom)
  }

  /**
   * Calculates the week-based-year.
   *
   * @param date  the date, not null
   * @return the week-based-year
   */
  private[calendar] def getWeekBasedYearFromDate(date: Date): Int = {
    var year: Year = Year(date)
    if (date.getMonthOfYear == MonthOfYear.January) {
      val dom: Int = date.getDayOfMonth
      if (dom < 4) {
        val dow: Int = date.getDayOfWeek.ordinal
        if (dow > dom + 3) {
          year = year.previous
        }
      }
    }
    else if (date.getMonthOfYear == MonthOfYear.December) {
      val dom: Int = date.getDayOfMonth
      if (dom > 28) {
        val dow: Int = date.getDayOfWeek.ordinal
        if (dow <= dom % 7) {
          year = year.next
        }
      }
    }
    return year.getValue
  }

  /**
   * Calculates the week of week-based-year.
   *
   * @param date  the date to use, not null
   * @return the week
   */
  private[calendar] def getWeekOfWeekBasedYearFromDate(date: Date): Int = {
    val wby: Int = getWeekBasedYearFromDate(date)
    val yearStart: Date = Date(wby, MonthOfYear.January, 4)
    MathUtils.safeToInt((date.toModifiedJulianDays - yearStart.toModifiedJulianDays + yearStart.getDayOfWeek.ordinal - 1) / 7 + 1)
  }

  /**
   * Gets the rule for the year field in the ISO chronology.
   * <p>
   * This field counts years using the modern civil calendar system as defined
   * by ISO-8601. There is no historical cutover (as found in historical dates
   * such as from the Julian to Gregorian calendar).
   * <p>
   * The implication of this is that historical dates will not be accurate.
   * All work requiring accurate historical dates must use the appropriate
   * chronology that defines the Gregorian cutover.
   * <p>
   * A further implication of the ISO-8601 rules is that the year zero
   * exists. This roughly equates to 1 BC/BCE, however the alignment is
   * not exact as explained above.
   *
   * @return the rule for the year field, never null
   */
  def yearRule: DateTimeFieldRule[Int] = YearRule

  /**
   * Gets the rule for the month-of-year field in the ISO chronology.
   * <p>
   * This field counts months sequentially from the start of the year.
   * The values follow the ISO-8601 standard and normal human interactions.
   * These define January as value 1 to December as value 12.
   * <p>
   * The enum {@link MonthOfYear} should be used wherever possible in
   * applications when referring to the day of the week to avoid
   * hard-coding the values.
   *
   * @return the rule for the month-of-year field, never null
   */
  def monthOfYearRule: DateTimeFieldRule[MonthOfYear] = MonthOfYearRule

  /**
   * Gets the rule for the day-of-month field in the ISO chronology.
   * <p>
   * This field counts days sequentially from the start of the month.
   * The first day of the month is 1 and the last is 28, 29, 30 or 31
   * depending on the month and whether it is a leap year.
   *
   * @return the rule for the day-of-month field, never null
   */
  def dayOfMonthRule: DateTimeFieldRule[Int] = DayOfMonthRule

  /**
   * Gets the rule for the day-of-year field in the ISO chronology.
   * <p>
   * This field counts days sequentially from the start of the year.
   * The first day of the year is 1 and the last is 365, or 366 in a leap year.
   *
   * @return the rule for the day-of-year field, never null
   */
  def dayOfYearRule: DateTimeFieldRule[Int] = DayOfYearRule

  /**
   * Gets the rule for the week-based-year field in the ISO chronology.
   * <p>
   * This field is the year that results from calculating weeks with the ISO-8601 algorithm.
   * See {@link #weekOfWeekBasedYearRule() week of week-based-year} for details.
   * <p>
   * The week-based-year will either be 52 or 53 weeks long, depending on the
   * result of the algorithm for a particular date.
   *
   * @return the rule for the week-based-year field, never null
   */
  def weekBasedYearRule: DateTimeFieldRule[Int] = WeekBasedYearRule

  /**
   * Gets the rule for the week-of-week-based-year field in the ISO chronology.
   * <p>
   * This field counts weeks using the ISO-8601 algorithm.
   * The first week of the year is the week which has at least 4 days in the year
   * using a Monday to Sunday week definition. Thus it is possible for the first
   * week to start on any day from the 29th December in the previous year to the
   * 4th January in the new year. The year which is aligned with this field is
   * known as the {@link #weekBasedYearRule() week-based-year}.
   *
   * @return the rule for the week-of-week-based-year field, never null
   */
  def weekOfWeekBasedYearRule: DateTimeFieldRule[Int] = WeekOfWeekBasedYearRule

  /**
   * Gets the rule for the day-of-week field.
   * <p>
   * This field uses the ISO-8601 values for the day-of-week.
   * These define Monday as value 1 to Sunday as value 7.
   * <p>
   * The enum {@link DayOfWeek} should be used wherever possible in
   * applications when referring to the day of the week value to avoid
   * needing to remember the values from 1 to 7.
   *
   * @return the rule for the day-of-week field, never null
   */
  def dayOfWeekRule: DateTimeFieldRule[DayOfWeek] = DayOfWeekRule

  /**
   * Gets the rule for the week-of-year field in the ISO chronology.
   * <p>
   * This field counts weeks in groups of seven days starting from the first
   * of January. The 1st to the 7th of January is always week 1 while the
   * 8th to the 14th is always week 2.
   *
   * @return the rule for the week-of-year field, never null
   */
  def weekOfYearRule: DateTimeFieldRule[Int] = WeekOfYearRule

  /**
   * Gets the rule for the quarter-of-year field in the ISO chronology.
   * <p>
   * This field counts quarters sequentially from the start of the year.
   * The first quarter of the year is 1 and the last is 4. Each quarter
   * lasts exactly three months.
   *
   * @return the rule for the quarter-of-year field, never null
   */
  def quarterOfYearRule: DateTimeFieldRule[QuarterOfYear] = QuarterOfYearRule

  /**
   * Gets the rule for the month-of-quarter field in the ISO chronology.
   * <p>
   * This field counts months sequentially from the start of the quarter.
   * The first month of the quarter is 1 and the last is 3. Each quarter
   * lasts exactly three months.
   *
   * @return the rule for the month-of-quarter field, never null
   */
  def monthOfQuarterRule: DateTimeFieldRule[Int] = MonthOfQuarterRule

  /**
   * Gets the rule for the week-of-month field in the ISO chronology.
   * <p>
   * This field counts weeks in groups of seven days starting from the first
   * day of the month. The 1st to the 7th of a month is always week 1 while the
   * 8th to the 14th is always week 2 and so on.
   * <p>
   * This field can be used to create concepts such as 'the second Saturday'
   * of a month. To achieve this, setup a {@link DateTimeFields} instance
   * using this rule and the {@link #dayOfWeekRule() day-of-week} rule.
   *
   * @return the rule for the week-of-month field, never null
   */
  def weekOfMonthRule: DateTimeFieldRule[Int] = WeekOfMonthRule

  /**
   * Gets the rule for the hour-of-day field.
   * <p>
   * This field counts hours sequentially from the start of the day.
   * The values run from 0 to 23.
   *
   * @return the rule for the hour-of-day field, never null
   */
  def hourOfDayRule: DateTimeFieldRule[Int] = HourOfDayRule

  /**
   * Gets the rule for the minute-of-hour field.
   * <p>
   * This field counts minutes sequentially from the start of the hour.
   * The values run from 0 to 59.
   *
   * @return the rule for the minute-of-hour field, never null
   */
  def minuteOfHourRule: DateTimeFieldRule[Int] = MinuteOfHourRule

  /**
   * Gets the rule for the second-of-minute field.
   * <p>
   * This field counts seconds sequentially from the start of the minute.
   * The values run from 0 to 59.
   *
   * @return the rule for the second-of-minute field, never null
   */
  def secondOfMinuteRule: DateTimeFieldRule[Int] = SecondOfMinuteRule

  /**
   * Gets the rule for the nano-of-second field.
   * <p>
   * This field counts nanoseconds sequentially from the start of the second.
   * The values run from 0 to 999,999,999.
   *
   * @return the rule for the nano-of-second field, never null
   */
  def nanoOfSecondRule: DateTimeFieldRule[Int] = NanoOfSecondRule

  /**
   * Gets the rule for the second-of-day field.
   * <p>
   * This field counts seconds sequentially from the start of the day.
   * The values run from 0 to 86399.
   *
   * @return the rule for the second-of-day field, never null
   */
  def secondOfDayRule: DateTimeFieldRule[Int] = SecondOfDayRule

  /**
   * Gets the rule for the milli-of-day field.
   * <p>
   * This field counts milliseconds sequentially from the start of the day.
   * The values run from 0 to 86,399,999.
   *
   * @return the rule for the milli-of-day field, never null
   */
  def milliOfDayRule: DateTimeFieldRule[Int] = MilliOfDayRule

  /**
   * Gets the rule for the milli-of-second field.
   * <p>
   * This field counts milliseconds sequentially from the start of the second.
   * The values run from 0 to 999.
   *
   * @return the rule for the milli-of-second field, never null
   */
  def milliOfSecondRule: DateTimeFieldRule[Int] = MilliOfSecondRule

  /**
   * Gets the rule for the AM/PM of day field.
   * <p>
   * This field defines the half-day AM/PM value. The hour-of-day from 0 to 11 is
   * defined as AM, while the hours from 12 to 23 are defined as PM.
   * AM is defined with the value 0, while PM is defined with the value 1.
   * <p>
   * The enum {@link AmPmOfDay} should be used wherever possible in
   * applications when referring to the day of the week to avoid
   * hard-coding the values.
   *
   * @return the rule for the am/pm of day field, never null
   */
  def amPmOfDayRule: DateTimeFieldRule[AmPmOfDay] = AmPmOfDayRule

  /**
   * Gets the rule for the hour of AM/PM field from 0 to 11.
   * <p>
   * This field counts hours sequentially from the start of the half-day AM/PM.
   * The values run from 0 to 11.
   *
   * @return the rule for the hour of AM/PM field, never null
   */
  def hourOfAmPmRule: DateTimeFieldRule[Int] = HourOfAmPmRule

  /**
   * Gets the rule for the clock hour of AM/PM field from 1 to 12.
   * <p>
   * This field counts hours sequentially within the half-day AM/PM as
   * normally seen on a clock or watch. The values run from 1 to 12.
   *
   * @return the rule for the hour of AM/PM field, never null
   */
  def clockHourOfAmPmRule: DateTimeFieldRule[Int] = ClockHourOfAmPmRule

  /**
   * Gets the rule for the clock hour of AM/PM field from 1 to 24.
   * <p>
   * This field counts hours sequentially within the day starting from 1.
   * The values run from 1 to 24.
   *
   * @return the rule for the clock-hour-of-day field, never null
   */
  def clockHourOfDayRule: DateTimeFieldRule[Int] = ClockHourOfDayRule

  /**
   * Gets the rule for the epoch-days field.
   * <p>
   * This field counts seconds sequentially from the Java epoch of 1970-01-01.
   *
   * @return the rule for the epoch-days field, never null
   */
  def epochDays: CalendricalRule[Long] = EpochDaysRule

  /**
   * Gets the rule for the nano-of-day field.
   * <p>
   * This field counts seconds sequentially from the start of the day.
   * The values run from 0 to 86,399,999,999,999.
   *
   * @return the rule for the nano-of-day field, never null
   */
  def nanoOfDayRule: CalendricalRule[Long] = NanoOfDayRule

  /**
   * Gets the period unit for eras.
   * <p>
   * The period unit defines the concept of a period of a eras.
   * An era, based on a simple before/after point on the time-line, is infinite
   * in length. For this rule, an era has an estimated duration of 2,000,000,000 years.
   * <p>
   * This is a basic unit and has no equivalent period.
   * The estimated duration is equal to 2,000,000,000 years.
   *
   * @return the period unit for eras, never null
   */
  def periodEras: PeriodUnit = Eras

  /**
   * Gets the period unit for millennia of 1000 years.
   * <p>
   * The period unit defines the concept of a period of a century.
   * <p>
   * The equivalent period and estimated duration are equal to 10 centuries.
   *
   * @return the period unit for millennia, never null
   */
  def periodMillennia: PeriodUnit = Millennia

  /**
   * Gets the period unit for centuries of 100 years.
   * <p>
   * The period unit defines the concept of a period of a century.
   * <p>
   * The equivalent period and estimated duration are equal to 10 decades.
   *
   * @return the period unit for centuries, never null
   */
  def periodCenturies: PeriodUnit = Centuries

  /**
   * Gets the period unit for decades of 10 years.
   * <p>
   * The period unit defines the concept of a period of a decade.
   * <p>
   * The equivalent period and estimated duration are equal to 10 years.
   *
   * @return the period unit for decades, never null
   */
  def periodDecades: PeriodUnit = Decades

  /**
   * Gets the period unit for years of 12 months.
   * <p>
   * The period unit defines the concept of a period of a year.
   * <p>
   * The equivalent period and estimated duration are equal to 4 quarters.
   * <p>
   * See {@link #yearRule()} for the main date-time field.
   *
   * @return the period unit for years, never null
   */
  def periodYears: PeriodUnit = Years

  /**
   * Gets the period unit for week-based-years.
   * <p>
   * The period unit defines the concept of a period of a week-based-year.
   * This is typically 52 weeks, and occasionally 53 weeks.
   * <p>
   * This is a basic unit and has no equivalent period.
   * The estimated duration is equal to 364.5 days, which is just over 5 weeks.
   * <p>
   * See {@link #weekBasedYearRule()} for the main date-time field.
   *
   * @return the period unit for week-based-years, never null
   */
  def periodWeekBasedYears: PeriodUnit = WeekBasedYears

  /**
   * Gets the period unit for quarters of 3 months.
   * <p>
   * The period unit defines the concept of a period of a quarter.
   * <p>
   * The equivalent period and estimated duration are equal to 3 months.
   * <p>
   * See {@link #quarterOfYearRule()} for the main date-time field.
   *
   * @return the period unit for quarters, never null
   */
  def periodQuarters: PeriodUnit = Quarters

  /**
   * Gets the period unit for months.
   * <p>
   * The period unit defines the concept of a period of a month.
   * <p>
   * This is a basic unit and has no equivalent period.
   * The estimated duration is equal to one-twelfth of a year based on 365.2425 days.
   * <p>
   * See {@link #monthOfYearRule()} for the main date-time field.
   *
   * @return the period unit for months, never null
   */
  def periodMonths: PeriodUnit = Months

  /**
   * Gets the period unit for weeks of 7 days.
   * <p>
   * The period unit defines the concept of a period of a week.
   * <p>
   * The equivalent period and estimated duration are equal to 7 days.
   * <p>
   * See {@link #weekOfWeekBasedYearRule()} and {@link #weekOfYearRule()} for
   * the main date-time fields.
   *
   * @return the period unit for weeks, never null
   */
  def periodWeeks: PeriodUnit = Weeks

  /**
   * Gets the period unit for days.
   * <p>
   * The period unit defines the concept of a period of a day.
   * This is typically equal to 24 hours, but may vary due to time-zone changes.
   * <p>
   * This chronology defines two units that could represent a day.
   * This unit, {@code Days}, represents a day that varies in length based on
   * time-zone (daylight savings time) changes. It is a basic unit that cannot
   * be converted to seconds, nanoseconds or {@link Duration}.
   * By contrast, the {@link #period24Hours() 24Hours} unit has a fixed length of
   * exactly 24 hours allowing it to be converted to seconds, nanoseconds and {@code Duration}.
   * <p>
   * This is a basic unit and has no equivalent period.
   * The estimated duration is equal to 24 hours.
   * <p>
   * See {@link #dayOfMonthRule()} for the main date-time field.
   *
   * @return the period unit for accurate, variable length, days, never null
   */
  def periodDays: PeriodUnit = Days

  /**
   * Gets the period unit for twenty-four hours, that is often treated as a day.
   * <p>
   * The period unit defines the concept of a period of exactly 24 hours that
   * is often treated as a day. The unit name of "24Hours" is intended to convey
   * the fact that this is primarily a 24 hour unit that happens to be used as
   * a day unit on occasion. In most scenarios, the standard {@link #periodDays() Days}
   * unit is more applicable and accurate.
   * <p>
   * This chronology defines two units that could represent a day.
   * This unit, {@code 24Hours}, represents a fixed length of exactly 24 hours,
   * allowing it to be converted to seconds, nanoseconds and {@link Duration}.
   * By contrast, the {@code Days} unit varies in length based on time-zone (daylight
   * savings time) changes and cannot be converted to seconds, nanoseconds or {@code Duration}.
   * <p>
   * The equivalent period and estimated duration are equal to twice the
   * 12 hours unit, making it also equivalent to 24 hours.
   *
   * @return the period unit for fixed, 24 hour, days, never null
   */
  def period24Hours: PeriodUnit = _24Hours

  /**
   * Gets the period unit for twelve hours, as used by AM/PM.
   * <p>
   * The period unit defines the concept of a period of 12 hours.
   * <p>
   * The equivalent period and estimated duration are equal to 12 hours.
   * <p>
   * See {@link #amPmOfDayRule()} for the main date-time field.
   *
   * @return the period unit for twelve hours, never null
   */
  def period12Hours: PeriodUnit = _12Hours

  /**
   * Gets the period unit for hours of 60 minutes.
   * <p>
   * The period unit defines the concept of a period of a hour.
   * <p>
   * The equivalent period and estimated duration are equal to 60 minutes.
   * <p>
   * See {@link #hourOfDayRule()} for the main date-time field.
   *
   * @return the period unit for hours, never null
   */
  def periodHours: PeriodUnit = Hours

  /**
   * Gets the period unit for minutes of 60 seconds.
   * <p>
   * The period unit defines the concept of a period of a minute.
   * <p>
   * The equivalent period and estimated duration are equal to 60 seconds.
   * <p>
   * See {@link #minuteOfHourRule()} for the main date-time field.
   *
   * @return the period unit for minutes, never null
   */
  def periodMinutes: PeriodUnit = Minutes

  /**
   * Gets the period unit for seconds.
   * <p>
   * The period unit defines the concept of a period of a second.
   * <p>
   * The equivalent period and estimated duration are equal to 1000 milliseconds.
   * <p>
   * See {@link #secondOfMinuteRule()} for the main date-time field.
   *
   * @return the period unit for seconds, never null
   */
  def periodSeconds: PeriodUnit = Seconds

  /**
   * Gets the period unit for milliseconds.
   * <p>
   * The period unit defines the concept of a period of a millisecond.
   * <p>
   * The equivalent period and estimated duration are equal to 1000 microseconds.
   * <p>
   * See {@link #milliOfSecondRule()} for the main date-time field.
   *
   * @return the period unit for milliseconds, never null
   */
  def periodMillis: PeriodUnit = Millis

  /**
   * Gets the period unit for microseconds.
   * <p>
   * The period unit defines the concept of a period of a microsecond.
   * <p>
   * The equivalent period and estimated duration are equal to 1000 nanoseconds.
   *
   * @return the period unit for microseconds, never null
   */
  def periodMicros: PeriodUnit = Micros

  /**
   * Gets the period unit for nanoseconds.
   * <p>
   * The period unit defines the concept of a period of a nanosecond.
   * <p>
   * This is a basic unit and has no equivalent period.
   * The estimated duration is 1 nanosecond.
   * <p>
   * See {@link #nanoOfSecondRule()} for the main date-time field.
   *
   * @return the period unit for nanoseconds, never null
   */
  def periodNanos: PeriodUnit = Nanos

  /**
   * Constant for the minimum week-based-year.
   */
  val MinWeekBasedYear: Int = Year.MinYear

  /**
   * Constant for the maximum week-based-year.
   */
  val MaxWeekBasedYear: Int = Year.MaxYear

  /**
   * The number of seconds in one day.
   */
  private[calendar] val SecondsPerDay: Int = 60 * 60 * 24

  /**
   * The number of days in a 400 year cycle.
   */
  private[calendar] val DaysPerCycle: Int = 146097

  /**
   * The number of days from year zero to year 1970.
   * There are five 400 year cycles from year zero to 2000.
   * There are 7 leap years from 1970 to 2000.
   */
  private[calendar] val Days0000To1970: Long = (DaysPerCycle * 5L) - (30L * 365L + 7L)
  /**
   * The number of days from year zero to the Modified Julian Day epoch of 1858-11-17.
   */
  private[calendar] val Days0000ToModifiedJulianDayEpoch: Long = 678941
  /**
   * Period unit for nanoseconds.
   */
  private val Nanos: ChronoUnit = new ChronoUnit(0 * 16, "Nanos", null, Duration.ofNanos(1))
  /**
   * Period unit for microseconds.
   */
  private val Micros: ChronoUnit = new ChronoUnit(1 * 16, "Micros", PeriodField.of(1000, Nanos), Duration.ofNanos(1000))
  /**
   * Period unit for milliseconds.
   */
  private val Millis: ChronoUnit = new ChronoUnit(2 * 16, "Millis", PeriodField.of(1000, Micros), Duration.ofMillis(1))
  /**
   * Period unit for seconds.
   */
  private val Seconds: ChronoUnit = new ChronoUnit(3 * 16, "Seconds", PeriodField.of(1000, Millis), Duration.ofSeconds(1))
  /**
   * Period unit for minutes.
   */
  private val Minutes: ChronoUnit = new ChronoUnit(4 * 16, "Minutes", PeriodField.of(60, Seconds), Duration.ofSeconds(60))
  /**
   * Period unit for hours.
   */
  private val Hours: ChronoUnit = new ChronoUnit(5 * 16, "Hours", PeriodField.of(60, Minutes), Duration.ofSeconds(60 * 60))
  /**
   * Period unit for 12 hours half-days, used by AM/PM.
   */
  private val _12Hours: ChronoUnit = new ChronoUnit(6 * 16, "12Hours", PeriodField.of(12, Hours), Duration.ofSeconds(12 * 60 * 60))
  /**
   * Period unit for 24 hour fixed length days.
   */
  private val _24Hours: ChronoUnit = new ChronoUnit(7 * 16, "24Hours", PeriodField.of(2, _12Hours), Duration.ofSeconds(24 * 60 * 60))
  /**
   * Period unit for days.
   */
  private val Days: ChronoUnit = new ChronoUnit(8 * 16, "Days", null, Duration.ofSeconds(86400))
  /**
   * Period unit for weeks.
   */
  private val Weeks: ChronoUnit = new ChronoUnit(9 * 16, "Weeks", PeriodField.of(7, Days), Duration.ofSeconds(7L * 86400L))
  /**
   * Period unit for months.
   */
  private val Months: ChronoUnit = new ChronoUnit(10 * 16, "Months", null, Duration.ofSeconds(31556952L / 12L))
  /**
   * Period unit for quarters.
   */
  private val Quarters: ChronoUnit = new ChronoUnit(11 * 16, "Quarters", PeriodField.of(3, Months), Duration.ofSeconds(31556952L / 4))
  /**
   * Period unit for week-based-years.
   */
  private val WeekBasedYears: ChronoUnit = new ChronoUnit(12 * 16, "WeekBasedYears", null, Duration.ofSeconds(364L * 86400L + 43200L))
  /**
   * Period unit for years.
   */
  private val Years: ChronoUnit = new ChronoUnit(13 * 16, "Years", PeriodField.of(4, Quarters), Duration.ofSeconds(31556952L))
  /**
   * Period unit for decades.
   */
  private val Decades: ChronoUnit = new ChronoUnit(14 * 16, "Decades", PeriodField.of(10, Years), Duration.ofSeconds(10L * 31556952L))
  /**
   * Period unit for centuries.
   */
  private val Centuries: ChronoUnit = new ChronoUnit(15 * 16, "Centuries", PeriodField.of(10, Decades), Duration.ofSeconds(100L * 31556952L))
  /**
   * Period unit for millennia.
   */
  private val Millennia: ChronoUnit = new ChronoUnit(16 * 16, "Millennia", PeriodField.of(10, Centuries), Duration.ofSeconds(1000L * 31556952L))
  /**
   * Period unit for eras.
   */
  private val Eras: ChronoUnit = new ChronoUnit(17 * 16, "Eras", null, Duration.ofSeconds(31556952L * 2000000000L))
  /**
   * Cache of units for deserialization.
   * Indices must match ordinal passed to unit constructor.
   */
  private val UnitCache: Array[ChronoUnit] = Array[ChronoUnit](Nanos, Micros, Millis, Seconds, Minutes, Hours, _12Hours, _24Hours, Days, Weeks, Months, Quarters, WeekBasedYears, Years, Decades, Centuries, Millennia, Eras)
  private val NanoOfSecondOrdinal: Int = 0 * 16
  private val MilliOfSecondOrdinal: Int = 1 * 16
  private val MilliOfDayOrdinal: Int = 2 * 16
  private val SecondOfMinuteOrdinal: Int = 3 * 16
  private val SecondOfDayOrdinal: Int = 4 * 16
  private val MinuteOfHourOrdinal: Int = 5 * 16
  private val ClockHourOfAmPmOrdinal: Int = 6 * 16
  private val HourOfAmPmOrdinal: Int = 7 * 16
  private val ClockHourOfDayOrdinal: Int = 8 * 16
  private val HourOfDayOrdinal: Int = 9 * 16
  private val DayOfMonthOrdinal: Int = 10 * 16
  private val DayOfYearOrdinal: Int = 11 * 16
  private val WeekOfMonthOrdinal: Int = 12 * 16
  private val WeekOfWeekBasedYearOrdinal: Int = 13 * 16
  private val WeekOfYearOrdinal: Int = 14 * 16
  private val MonthOfQuarterOrdinal: Int = 15 * 16
  private val WeekBasedYearOrdinal: Int = 16 * 16
  private val YearOrdinal: Int = 17 * 16
  private val NanoOfSecondRule = new Rule(NanoOfSecondOrdinal, "NanoOfSecond", Nanos, Seconds, 0, 999999999, 999999999)
  private val MilliOfSecondRule = new Rule(MilliOfSecondOrdinal, "MilliOfSecond", Millis, Seconds, 0, 999, 999)
  private val MilliOfDayRule = new Rule(MilliOfDayOrdinal, "MilliOfDay", Millis, Days, 0, 86399999, 86399999)
  private val SecondOfMinuteRule = new Rule(SecondOfMinuteOrdinal, "SecondOfMinute", Seconds, Minutes, 0, 59, 59)
  private val SecondOfDayRule = new Rule(SecondOfDayOrdinal, "SecondOfDay", Seconds, Days, 0, 86399, 86399)
  private val MinuteOfHourRule = new Rule(MinuteOfHourOrdinal, "MinuteOfHour", Minutes, Hours, 0, 59, 59)
  private val ClockHourOfAmPmRule = new Rule(ClockHourOfAmPmOrdinal, "ClockHourOfAmPm", Hours, _12Hours, 1, 12, 12)
  private val HourOfAmPmRule = new Rule(HourOfAmPmOrdinal, "HourOfAmPm", Hours, _12Hours, 0, 11, 11)
  private val ClockHourOfDayRule = new Rule(ClockHourOfDayOrdinal, "ClockHourOfDay", Hours, Days, 1, 24, 24)
  private val HourOfDayRule = new Rule(HourOfDayOrdinal, "HourOfDay", Hours, Days, 0, 23, 23)
  private val DayOfMonthRule = new Rule(DayOfMonthOrdinal, "DayOfMonth", Days, Months, 1, 31, 28)
  private val DayOfYearRule = new Rule(DayOfYearOrdinal, "DayOfYear", Days, Years, 1, 366, 365)
  private val WeekOfMonthRule = new Rule(WeekOfMonthOrdinal, "WeekOfMonth", Weeks, Months, 1, 5, 4)
  private val WeekOfWeekBasedYearRule = new Rule(WeekOfWeekBasedYearOrdinal, "WeekOfWeekBasedYear", Weeks, WeekBasedYears, 1, 53, 52)
  private val WeekOfYearRule = new Rule(WeekOfYearOrdinal, "WeekOfYear", Weeks, Years, 1, 53, 53)
  private val MonthOfQuarterRule = new Rule(MonthOfQuarterOrdinal, "MonthOfQuarter", Months, Quarters, 1, 3, 3)
  private val WeekBasedYearRule = new Rule(WeekBasedYearOrdinal, "WeekBasedYear", WeekBasedYears, null, MinWeekBasedYear, MaxWeekBasedYear, MaxWeekBasedYear)
  private val YearRule = new Rule(YearOrdinal, "Year", Years, null, Year.MinYear, Year.MaxYear, Year.MaxYear)
  /**
   * Cache of units for deserialization.
   * Indices must match ordinal passed to rule constructor.
   */
  private val RuleCache: Array[Rule] =
    Array[Rule](
      NanoOfSecondRule,
      MilliOfSecondRule,
      MilliOfDayRule,
      SecondOfMinuteRule,
      SecondOfDayRule,
      MinuteOfHourRule,
      ClockHourOfAmPmRule,
      HourOfAmPmRule,
      ClockHourOfDayRule,
      HourOfDayRule,
      DayOfMonthRule,
      DayOfYearRule,
      WeekOfMonthRule,
      WeekOfWeekBasedYearRule,
      WeekOfYearRule,
      MonthOfQuarterRule,
      WeekBasedYearRule,
      YearRule)

  /**
   * Rule implementation.
   */
  private[calendar] object MonthOfYearRule extends MonthOfYearRule

  @SerialVersionUID(1L)
  private[calendar] sealed class MonthOfYearRule
    extends DateTimeFieldRule[MonthOfYear](classOf[MonthOfYear], ISOChronology, "MonthOfYear", Months, Years, 1, 12, true) with Serializable {

    private def readResolve: AnyRef = MonthOfYearRule

    override def derive(calendrical: Calendrical): Option[MonthOfYear] = calendrical.get(Date.rule).map(_.getMonthOfYear)

    override def convertValueToInt(value: MonthOfYear): Int = value.ordinal

    override def convertIntToValue(value: Int): MonthOfYear = MonthOfYear.of(value)

    override protected def interpret(merger: CalendricalMerger, value: Any): MonthOfYear = {
      if (value.isInstanceOf[Int]) {
        var `val` : Int = value.asInstanceOf[Int]
        if (`val` < 1 || `val` > 12) {
          merger.addToOverflow(Period.ofMonths(`val` - 1))
          `val` = 1
        }
        return MonthOfYear.of(`val`)
      }
      return null
    }

    override protected def createTextStores(textStores: HashMap[TextStyle, TextStore], locale: Locale): Unit = {
      val oldSymbols: DateFormatSymbols = new DateFormatSymbols(locale)
      var array: Array[String] = oldSymbols.getMonths
      val mapFull = HashMap[Int, String](
        (1, array(Calendar.JANUARY)),
        (2, array(Calendar.FEBRUARY)),
        (3, array(Calendar.MARCH)),
        (4, array(Calendar.APRIL)),
        (5, array(Calendar.MAY)),
        (6, array(Calendar.JUNE)),
        (7, array(Calendar.JULY)),
        (8, array(Calendar.AUGUST)),
        (9, array(Calendar.SEPTEMBER)),
        (10, array(Calendar.OCTOBER)),
        (11, array(Calendar.NOVEMBER)),
        (12, array(Calendar.DECEMBER)))
      textStores.put(TextStyle.Full, new TextStore(locale, mapFull))

      array = oldSymbols.getShortMonths
      val mapShort = HashMap[Int, String](
        (1, array(Calendar.JANUARY)),
        (2, array(Calendar.FEBRUARY)),
        (3, array(Calendar.MARCH)),
        (4, array(Calendar.APRIL)),
        (5, array(Calendar.MAY)),
        (6, array(Calendar.JUNE)),
        (7, array(Calendar.JULY)),
        (8, array(Calendar.AUGUST)),
        (9, array(Calendar.SEPTEMBER)),
        (10, array(Calendar.OCTOBER)),
        (11, array(Calendar.NOVEMBER)),
        (12, array(Calendar.DECEMBER)))
      textStores.put(TextStyle.Short, new TextStore(locale, mapShort))
    }

  }

  /**
   * Rule implementation.
   */
  private[calendar] object DayOfWeekRule extends DayOfWeekRule

  @SerialVersionUID(1L)
  private[calendar] sealed class DayOfWeekRule
    extends DateTimeFieldRule[DayOfWeek](classOf[DayOfWeek], ISOChronology, "DayOfWeek", Days, Weeks, 1, 7, true) with Serializable {

    private def readResolve: AnyRef = DayOfWeekRule

    override def derive(calendrical: Calendrical): Option[DayOfWeek] = calendrical.get(Date.rule).map(getDayOfWeekFromDate(_))

    override def convertValueToInt(value: DayOfWeek): Int = value.ordinal

    override def convertIntToValue(value: Int): DayOfWeek = DayOfWeek.of(value)

    override protected def interpret(merger: CalendricalMerger, value: Any): DayOfWeek = {
      if (value.isInstanceOf[Int]) {
        var `val` : Int = value.asInstanceOf[Int]
        if (`val` < 1 || `val` > 7) {
          merger.addToOverflow(Period.ofDays(`val` - 1))
          `val` = 1
        }
        return DayOfWeek.of(`val`)
      }
      return null
    }

    override protected def createTextStores(textStores: HashMap[TextStyle, TextStore], locale: Locale): Unit = {
      val oldSymbols: DateFormatSymbols = new DateFormatSymbols(locale)
      var array: Array[String] = oldSymbols.getWeekdays
      val mapFull = HashMap[Int, String](
        (1, array(Calendar.MONDAY)),
        (2, array(Calendar.TUESDAY)),
        (3, array(Calendar.WEDNESDAY)),
        (4, array(Calendar.THURSDAY)),
        (5, array(Calendar.FRIDAY)),
        (6, array(Calendar.SATURDAY)),
        (7, array(Calendar.SUNDAY)))
      textStores.put(TextStyle.Full, new TextStore(locale, mapFull))

      array = oldSymbols.getShortWeekdays
      val mapShort = HashMap[Int, String](
        (1, array(Calendar.MONDAY)),
        (2, array(Calendar.TUESDAY)),
        (3, array(Calendar.WEDNESDAY)),
        (4, array(Calendar.THURSDAY)),
        (5, array(Calendar.FRIDAY)),
        (6, array(Calendar.SATURDAY)),
        (7, array(Calendar.SUNDAY)))
      textStores.put(TextStyle.Short, new TextStore(locale, mapShort))
    }
  }

  /**
   * Rule implementation.
   */
  private[calendar] object QuarterOfYearRule extends QuarterOfYearRule

  @SerialVersionUID(1L)
  private[calendar] sealed class QuarterOfYearRule
    extends DateTimeFieldRule[QuarterOfYear](classOf[QuarterOfYear], ISOChronology, "QuarterOfYear", Quarters, Years, 1, 4) with Serializable {

    private def readResolve: AnyRef = QuarterOfYearRule

    override def derive(calendrical: Calendrical): Option[QuarterOfYear] =
      calendrical.get(monthOfYearRule).map(moy => QuarterOfYear.of(moy.ordinal / 3 + 1))

    override def convertValueToInt(value: QuarterOfYear): Int = value.getValue

    override def convertIntToValue(value: Int): QuarterOfYear = QuarterOfYear.of(value)
  }

  /**
   * Rule implementation.
   */
  private[calendar] object AmPmOfDayRule extends AmPmOfDayRule

  private[calendar] sealed class AmPmOfDayRule
    extends DateTimeFieldRule[AmPmOfDay](classOf[AmPmOfDay], ISOChronology, "AmPmOfDay", _12Hours, Days, 0, 1, true) with Serializable {

    private def readResolve: AnyRef = AmPmOfDayRule

    override def derive(calendrical: Calendrical): Option[AmPmOfDay] = {
      var hour: Int = calendrical.get(hourOfDayRule).getOrElse(return None)
      hour = (if (hour < 0) 1073741832 + hour + 1073741832 else hour)
      return Some(AmPmOfDay.of((hour % 24) / 12))
    }

    override def convertValueToInt(value: AmPmOfDay): Int = value.ordinal

    override def convertIntToValue(value: Int): AmPmOfDay = AmPmOfDay.of(value)

    override protected def interpret(merger: CalendricalMerger, value: Any): AmPmOfDay = {
      if (value.isInstanceOf[Int]) {
        var `val` : Int = value.asInstanceOf[Int]
        if (`val` < 0 || `val` > 1) {
          val days: Int = if (`val` > 0) `val` / 2 else ((`val` + 1) / 2) - 1
          merger.addToOverflow(Period.ofDays(days))
          `val` = (if (`val` > 0) `val` % 2 else -(`val` % 2))
        }
        return AmPmOfDay.of(`val`)
      }
      return null
    }

    override protected def createTextStores(textStores: HashMap[TextStyle, TextStore], locale: Locale): Unit = {
      val oldSymbols: DateFormatSymbols = new DateFormatSymbols(locale)
      val array: Array[String] = oldSymbols.getAmPmStrings
      val map = HashMap[Int, String](
        (0, array(Calendar.AM)),
        (1, array(Calendar.PM)))
      val textStore: TextStore = new TextStore(locale, map)
      textStores.put(TextStyle.Full, textStore)
      textStores.put(TextStyle.Short, textStore)
    }
  }

  /**
   * Rule implementation.
   */
  private[calendar] object EpochDaysRule extends EpochDaysRule

  @SerialVersionUID(1L)
  private[calendar] sealed class EpochDaysRule
    extends CalendricalRule[Long](classOf[Long], ISOChronology, "EpochDays", Days, null) with Serializable {

    private def readResolve: AnyRef = EpochDaysRule

    override def derive(calendrical: Calendrical): Option[Long] = calendrical.get(Date.rule).map(_.toEpochDays)

    override def merge(merger: CalendricalMerger): Unit = {
      val epochDays: Long = merger.getValue(this).get
      merger.storeMerged(Date.rule, Date.ofEpochDays(epochDays))
      merger.removeProcessed(this)
    }
  }

  /**
   * Rule implementation.
   */
  private[calendar] object NanoOfDayRule extends NanoOfDayRule

  @SerialVersionUID(1L)
  private[calendar] sealed class NanoOfDayRule
    extends CalendricalRule[Long](classOf[Long], ISOChronology, "NanoOfDay", Nanos, Days) with Serializable {

    private def readResolve: AnyRef = NanoOfDayRule

    override def derive(calendrical: Calendrical): Option[Long] = calendrical.get(Time.rule).map(_.toNanoOfDay)

    override def merge(merger: CalendricalMerger): Unit = {
      val nod: Long = merger.getValue(this).get
      merger.storeMerged(Time.rule, Time.ofNanoOfDay(nod))
      merger.removeProcessed(this)
    }
  }

  /**
   * Single unit subclass, which means fewer classes to load at startup.
   */
  @SerialVersionUID(1L)
  private[calendar] final class ChronoUnit(val ordinal: Int, name: String, equivalentPeriod: PeriodField, estimatedDuration: Duration)
    extends PeriodUnit(name, equivalentPeriod, estimatedDuration) {

    private def readResolve: AnyRef = UnitCache(ordinal / 16)

    override def compare(other: PeriodUnit): Int = {
      if (other.isInstanceOf[ChronoUnit]) {
        return ordinal - (other.asInstanceOf[ChronoUnit]).ordinal
      }
      return super.compare(other)
    }

    override def equals(obj: Any): Boolean = {
      if (obj.isInstanceOf[ChronoUnit]) {
        return ordinal == (obj.asInstanceOf[ChronoUnit]).ordinal
      }
      return super.equals(obj)
    }

    override val hashCode: Int = ordinal
  }

  /**
   * Single rule subclass, which means fewer classes to load at startup.
   */
  @SerialVersionUID(1L)
  private[calendar] final class Rule(private val ordinal: Int, name: String, periodUnit: PeriodUnit, periodRange: PeriodUnit,
                                     minimumValue: Int, maximumValue: Int, @transient private val smallestMaximum: Int)
    extends DateTimeFieldRule[Int](classOf[Int], ISOChronology, name, periodUnit, periodRange, minimumValue, maximumValue) with Serializable {

    private def readResolve: AnyRef = RuleCache(ordinal / 16)

    override def derive(calendrical: Calendrical): Option[Int] = {
      ordinal match {
        case NanoOfSecondOrdinal => calendrical.get(Time.rule).map(_.getNanoOfSecond)
        case MilliOfSecondOrdinal => calendrical.get(Time.rule).map(_.getNanoOfSecond / 1000000)
        case MilliOfDayOrdinal => calendrical.get(Time.rule).map(time => (time.toNanoOfDay / 1000000L).toInt)
        case SecondOfMinuteOrdinal => calendrical.get(Time.rule).map(_.getSecondOfMinute)
        case SecondOfDayOrdinal => calendrical.get(Time.rule).map(_.toSecondOfDay)
        case MinuteOfHourOrdinal => calendrical.get(Time.rule).map(_.getMinuteOfHour)
        case ClockHourOfAmPmOrdinal => calendrical.get(hourOfAmPmRule).map(hour => (hour + 11) % 12)
        case HourOfAmPmOrdinal => calendrical.get(hourOfDayRule).map(_ % 12)
        case ClockHourOfDayOrdinal => calendrical.get(hourOfDayRule).map(hour => (hour + 23) % 24)
        case HourOfDayOrdinal => calendrical.get(Time.rule).map(_.getHourOfDay)
        case DayOfMonthOrdinal => calendrical.get(Date.rule).map(_.getDayOfMonth)
        case DayOfYearOrdinal => calendrical.get(Date.rule).map(getDayOfYearFromDate(_))
        case MonthOfQuarterOrdinal => calendrical.get(monthOfYearRule).map(_.ordinal % 3 + 1)
        case WeekOfMonthOrdinal => calendrical.get(dayOfMonthRule).map(dom => (dom + 6) / 7)
        case WeekOfWeekBasedYearOrdinal => calendrical.get(Date.rule).map(getWeekOfWeekBasedYearFromDate(_))
        case WeekOfYearOrdinal => calendrical.get(dayOfYearRule).map(doy => (doy + 6) / 7)
        case WeekBasedYearOrdinal => calendrical.get(Date.rule).map(getWeekBasedYearFromDate(_))
        case YearOrdinal => calendrical.get(Date.rule).map(_.getYear)
        case _ => None
      }
    }

    override def getSmallestMaximumValue: Int = smallestMaximum

    override def getMaximumValue(calendrical: Calendrical): Int = {
      ordinal match {
        case DayOfMonthOrdinal => {
          val moy = calendrical.get(monthOfYearRule).getOrElse(return 31)
          val year: Int = calendrical.get(yearRule).getOrElse(return moy.maxLengthInDays)
          return moy.lengthInDays(isLeapYear(year))
        }
        case DayOfYearOrdinal => {
          val year: Int = calendrical.get(yearRule).getOrElse(return 366)
          return if (isLeapYear(year)) 366 else 365
        }
        case WeekOfMonthOrdinal => {
          val year: Int = calendrical.get(yearRule).getOrElse(return getMaximumValue)
          var moy: MonthOfYear = calendrical.get(monthOfYearRule).getOrElse(return getMaximumValue)
          if (moy == MonthOfYear.February) {
            return if (isLeapYear(year)) 5 else 4
          }
          return getMaximumValue
        }
        case WeekOfWeekBasedYearOrdinal => {
          var date: Date = calendrical.get(Date.rule).getOrElse(return 53)
          date = date.copy(day = 1).withMonthOfYear(1)
          if (date.getDayOfWeek == DayOfWeek.Thursday || (date.getDayOfWeek == DayOfWeek.Wednesday && isLeapYear(date.getYear))) {
            return 53
          }
          return 52
        }
      }
      return super.getMaximumValue
    }

    override def compare(other: CalendricalRule[Int]): Int = {
      if (other.isInstanceOf[Rule]) {
        return ordinal - (other.asInstanceOf[Rule]).ordinal
      }
      return super.compare(other)
    }

    override def equals(obj: Any): Boolean = {
      if (obj.isInstanceOf[ISOChronology.Rule]) {
        return ordinal == (obj.asInstanceOf[Rule]).ordinal
      }
      return super.equals(obj)
    }

    override def hashCode: Int = {
      return ordinal
    }
  }

}

@SerialVersionUID(1L)
sealed class ISOChronology private
  extends Chronology with Serializable {

  /**
   * Resolves singleton.
   *
   * @return the singleton instance
   */
  private def readResolve: AnyRef = ISOChronology

  /**
   * Gets the name of the chronology.
   *
   * @return the name of the chronology, never null
   */
  def getName: String = "ISO"

  /**
   * Merges the set of fields known by this chronology.
   *
   * @param merger  the merger to use, not null
   */
  private[calendar] def merge(merger: CalendricalMerger): Unit = {}

  //FIXME
  //  private[calendar] override def merge(merger: CalendricalMerger): Unit = {
  //    val modVal: Int = merger.getValue(ISOChronology.milliOfDayRule)
  //    if (modVal != null) {
  //      merger.storeMerged(Time.rule, Time.ofNanoOfDay(modVal * 1000000L))
  //      merger.removeProcessed(ISOChronology.milliOfDayRule)
  //    }
  //    val sodVal: Int = merger.getValue(ISOChronology.secondOfDayRule)
  //    if (modVal != null) {
  //      val nosVal: Int = merger.getValue(ISOChronology.nanoOfSecondRule)
  //      if (nosVal != null) {
  //        merger.storeMerged(Time.rule, Time.ofSecondOfDay(sodVal, nosVal))
  //        merger.removeProcessed(ISOChronology.nanoOfSecondRule)
  //      }
  //      else {
  //        val mosVal: Int = merger.getValue(ISOChronology.milliOfSecondRule)
  //        if (mosVal != null) {
  //          merger.storeMerged(Time.rule, Time.ofSecondOfDay(sodVal, mosVal * 1000000))
  //          merger.removeProcessed(ISOChronology.milliOfSecondRule)
  //        }
  //        else {
  //          merger.storeMerged(Time.rule, Time.ofSecondOfDay(sodVal))
  //        }
  //      }
  //      merger.removeProcessed(ISOChronology.secondOfDayRule)
  //    }
  //    var amPm: AmPmOfDay = merger.getValue(ISOChronology.amPmOfDayRule)
  //    if (amPm != null) {
  //      var hapVal: Int = merger.getValue(ISOChronology.hourOfAmPmRule)
  //      if (hapVal != null) {
  //        var hourOfDay: Int = amPm.getValue * 12 + hapVal
  //        merger.storeMerged(ISOChronology.hourOfDayRule, hourOfDay)
  //        merger.removeProcessed(ISOChronology.amPmOfDayRule)
  //        merger.removeProcessed(ISOChronology.hourOfAmPmRule)
  //      }
  //      var chapVal: Int = merger.getValue(ISOChronology.hourOfAmPmRule)
  //      if (chapVal != null) {
  //        var hourOfDay: Int = amPm.getValue * 12 + chapVal
  //        if (hourOfDay == 24) {
  //          merger.addToOverflow(Period.ofDays(1))
  //          hourOfDay = 0
  //        }
  //        merger.storeMerged(ISOChronology.hourOfDayRule, hourOfDay)
  //        merger.removeProcessed(ISOChronology.amPmOfDayRule)
  //        merger.removeProcessed(ISOChronology.clockHourOfAmPmRule)
  //      }
  //    }
  //    var hourVal: Int = merger.getValue(ISOChronology.hourOfDayRule).get
  //    if (hourVal != null) {
  //      val minuteVal: Int = merger.getValue(ISOChronology.minuteOfHourRule).get
  //      val secondVal: Int = merger.getValue(ISOChronology.secondOfMinuteRule).get
  //      val mosVal: Int = merger.getValue(ISOChronology.milliOfSecondRule).get
  //      val nanoVal: Int = merger.getValue(ISOChronology.nanoOfSecondRule).get
  //      if (minuteVal != null && secondVal != null && nanoVal != null) {
  //        merger.storeMerged(Time.rule, Time.of(hourVal, minuteVal, secondVal, nanoVal))
  //        merger.removeProcessed(ISOChronology.hourOfDayRule)
  //        merger.removeProcessed(ISOChronology.minuteOfHourRule)
  //        merger.removeProcessed(ISOChronology.secondOfMinuteRule)
  //        merger.removeProcessed(ISOChronology.nanoOfSecondRule)
  //      }
  //      else if (minuteVal != null && secondVal != null && mosVal != null) {
  //        merger.storeMerged(Time.rule, Time.of(hourVal, minuteVal, secondVal, mosVal * 1000000))
  //        merger.removeProcessed(ISOChronology.hourOfDayRule)
  //        merger.removeProcessed(ISOChronology.minuteOfHourRule)
  //        merger.removeProcessed(ISOChronology.secondOfMinuteRule)
  //        merger.removeProcessed(ISOChronology.milliOfSecondRule)
  //      }
  //      else if (minuteVal != null && secondVal != null) {
  //        merger.storeMerged(Time.rule, Time.of(hourVal, minuteVal, secondVal, 0))
  //        merger.removeProcessed(ISOChronology.hourOfDayRule)
  //        merger.removeProcessed(ISOChronology.minuteOfHourRule)
  //        merger.removeProcessed(ISOChronology.secondOfMinuteRule)
  //      }
  //      else if (minuteVal != null) {
  //        merger.storeMerged(Time.rule, Time.of(hourVal, minuteVal, 0, 0))
  //        merger.removeProcessed(ISOChronology.hourOfDayRule)
  //        merger.removeProcessed(ISOChronology.minuteOfHourRule)
  //      }
  //      else {
  //        merger.storeMerged(Time.rule, Time.of(hourVal, 0))
  //        merger.removeProcessed(ISOChronology.hourOfDayRule)
  //      }
  //    }
  //    var qoy: QuarterOfYear = merger.getValue(ISOChronology.quarterOfYearRule)
  //    var moqVal: Int = merger.getValue(ISOChronology.monthOfQuarterRule).get
  //    if (qoy != null && moqVal != null) {
  //      var moy: MonthOfYear = MonthOfYear.of(qoy.getFirstMonthOfQuarter.ordinal + moqVal)
  //      merger.storeMerged(ISOChronology.monthOfYearRule, moy)
  //      merger.removeProcessed(ISOChronology.quarterOfYearRule)
  //      merger.removeProcessed(ISOChronology.monthOfQuarterRule)
  //    }
  //    var yearVal: Int = merger.getValue(ISOChronology.yearRule).get
  //    if (yearVal != null) {
  //      var moy: MonthOfYear = merger.getValue(ISOChronology.monthOfYearRule).get
  //      val domVal: Int = merger.getValue(ISOChronology.dayOfMonthRule).get
  //      if (moy != null && domVal != null) {
  //        var date: Date = merger.getContext.resolveDate(yearVal, moy.getValue, domVal)
  //        merger.storeMerged(Date.rule, date)
  //        merger.removeProcessed(ISOChronology.yearRule)
  //        merger.removeProcessed(ISOChronology.monthOfYearRule)
  //        merger.removeProcessed(ISOChronology.dayOfMonthRule)
  //      }
  //      val doyVal: Int = merger.getValue(ISOChronology.dayOfYearRule).get
  //      if (doyVal != null) {
  //        merger.storeMerged(Date.rule, ISOChronology.getDateFromDayOfYear(yearVal, doyVal))
  //        merger.removeProcessed(ISOChronology.yearRule)
  //        merger.removeProcessed(ISOChronology.dayOfYearRule)
  //      }
  //      val woyVal: Int = merger.getValue(ISOChronology.weekOfYearRule).get
  //      val dow: DayOfWeek = merger.getValue(ISOChronology.dayOfWeekRule).get
  //      if (woyVal != null && dow != null) {
  //        var date: Date = Date(yearVal, 1, 1).plusWeeks(woyVal - 1)
  //        date = date.`with`(DateAdjusters.nextOrCurrent(dow))
  //        merger.storeMerged(Date.rule, date)
  //        merger.removeProcessed(ISOChronology.yearRule)
  //        merger.removeProcessed(ISOChronology.weekOfYearRule)
  //        merger.removeProcessed(ISOChronology.dayOfWeekRule)
  //      }
  //      val womVal: Int = merger.getValue(ISOChronology.weekOfMonthRule).get
  //      if (moy != null && womVal != null && dow != null) {
  //        var date: Date = Date(yearVal, moy, 1).plusWeeks(womVal - 1)
  //        date = date.`with`(DateAdjusters.nextOrCurrent(dow))
  //        merger.storeMerged(Date.rule, date)
  //        merger.removeProcessed(ISOChronology.yearRule)
  //        merger.removeProcessed(ISOChronology.monthOfYearRule)
  //        merger.removeProcessed(ISOChronology.weekOfMonthRule)
  //        merger.removeProcessed(ISOChronology.dayOfWeekRule)
  //      }
  //    }
  //    val wbyVal: Int = merger.getValue(ISOChronology.weekBasedYearRule).get
  //    if (wbyVal != null) {
  //      val woy: Int = merger.getValue(ISOChronology.weekOfWeekBasedYearRule).get
  //      val dow: DayOfWeek = merger.getValue(ISOChronology.dayOfWeekRule).get
  //      if (woy != null && dow != null) {
  //        merger.removeProcessed(ISOChronology.weekBasedYearRule)
  //        merger.removeProcessed(ISOChronology.weekOfWeekBasedYearRule)
  //        merger.removeProcessed(ISOChronology.dayOfWeekRule)
  //      }
  //    }
  //    var date: Date = merger.getValue(Date.rule).get
  //    val time: Time = merger.getValue(Time.rule).get
  //    val offset: ZoneOffset = merger.getValue(ZoneOffset.rule).get
  //    val zone: TimeZone = merger.getValue(TimeZone.rule).get
  //    if (date != null && time != null) {
  //      merger.storeMerged(DateTime.rule, DateTime.of(date, time))
  //      merger.removeProcessed(Date.rule)
  //      merger.removeProcessed(Time.rule)
  //    }
  //    if (date != null && offset != null) {
  //      merger.storeMerged(OffsetDate.rule, OffsetDate.of(date, offset))
  //      merger.removeProcessed(Date.rule)
  //      merger.removeProcessed(ZoneOffset.rule)
  //    }
  //    if (time != null && offset != null) {
  //      merger.storeMerged(OffsetTime.rule, OffsetTime.of(time, offset))
  //      merger.removeProcessed(Time.rule)
  //      merger.removeProcessed(ZoneOffset.rule)
  //    }
  //    val ldt: DateTime = merger.getValue(DateTime.rule).get
  //    if (ldt != null && offset != null) {
  //      merger.storeMerged(OffsetDateTime.rule, OffsetDateTime.of(ldt, offset))
  //      merger.removeProcessed(DateTime.rule)
  //      merger.removeProcessed(ZoneOffset.rule)
  //    }
  //    else {
  //      val od: OffsetDate = merger.getValue(OffsetDate.rule).orNull
  //      var ot: OffsetTime = merger.getValue(OffsetTime.rule).orNull
  //      if (od != null && ot != null) {
  //        if (od.offset.equals(ot.offset) == false) {
  //          if (merger.getContext.isStrict) {
  //            throw new CalendricalRuleException("Unable to merge OffsetDate and OffsetTime as offsets differ", OffsetTime.rule)
  //          }
  //          else {
  //            ot = ot.withOffsetSameInstant(od.offset)
  //          }
  //        }
  //        merger.storeMerged(OffsetDateTime.rule, OffsetDateTime.of(od, ot, od.offset))
  //        merger.removeProcessed(OffsetDate.rule)
  //        merger.removeProcessed(OffsetTime.rule)
  //      }
  //    }
  //    val odt: OffsetDateTime = merger.getValue(OffsetDateTime.rule).orNull
  //    if (odt != null && zone != null) {
  //      if (merger.getContext.isStrict) merger.storeMerged(ZonedDateTime.rule, ZonedDateTime.of(odt, zone))
  //      else merger.storeMerged(ZonedDateTime.rule, ZonedDateTime.ofInstant(odt, zone))
  //      merger.removeProcessed(OffsetDateTime.rule)
  //      merger.removeProcessed(TimeZone.rule)
  //    }
  //  }
}