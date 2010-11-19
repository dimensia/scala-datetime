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
import java.text.DateFormatSymbols
import java.util.Calendar
import java.util.EnumMap
import java.util.HashMap
import java.util.Locale
import java.util.Map
import javax.time.calendar.format.DateTimeFormatterBuilder.TextStyle
import javax.time.{MathUtils, Duration}

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
object ISOChronology extends ISOChronology {
  /**
   * Gets the rule for the nano-of-second field.
   * <p>
   * This field counts nanoseconds sequentially from the start of the second.
   * The values run from 0 to 999,999,999.
   *
   * @return the rule for the nano-of-second field, never null
   */
  def nanoOfSecondRule: DateTimeFieldRule[Int] = NANO_OF_SECOND

  private val MINUTE_OF_HOUR_ORDINAL: Int = 5 * 16
  private val MILLI_OF_DAY: Rule = new Rule(MILLI_OF_DAY_ORDINAL, "MilliOfDay", MILLIS, DAYS, 0, 86399999, 86399999)
  private val MILLI_OF_SECOND_ORDINAL: Int = 1 * 16
  /**
   * Gets the period unit for months.
   * <p>
   * The period unit defines the concept of a period of a month.
   * <p>
   * This is a basic unit and has no equivalent period.
   * The estimated duration is equal to one-twelfth of a year based on 365.2425 days.
   * <p>
   * See      { @link # monthOfYearRule ( ) } for the main date-time field.
   *
   * @return the period unit for months, never null
   */
  def periodMonths: PeriodUnit = MONTHS

  /**
   * Period unit for seconds.
   */
  private val SECONDS: Unit = new Unit(3 * 16, "Seconds", PeriodField.of(1000, MILLIS), Duration.ofSeconds(1))

  /**
   * Rule implementation.
   */
  private[calendar] object EpochDaysRule extends EpochDaysRule

  @SerialVersionUID(1L)
  private[calendar] sealed class EpochDaysRule
    extends CalendricalRule[Long](classOf[Long], ISOChronology, "EpochDays", DAYS, null)
    with Serializable {

    protected override def derive(calendrical: Calendrical): Long = {
      var date: LocalDate = calendrical.get(LocalDate.rule)
      return if (date != null) date.toEpochDays else null
    }

    protected override def merge(merger: CalendricalMerger): Unit = {
      var epochDays: Long = merger.getValue(this)
      merger.storeMerged(LocalDate.rule, LocalDate.ofEpochDays(epochDays))
      merger.removeProcessed(this)
    }

    private def readResolve: AnyRef = EpochDaysRule
  }

  /**
   * Rule implementation.
   */
  private[calendar] object MonthOfYearRule extends MonthOfYearRule

  @SerialVersionUID(1L)
  private[calendar] sealed class MonthOfYearRule
    extends DateTimeFieldRule[MonthOfYear](classOf[MonthOfYear], ISOChronology, "MonthOfYear", MONTHS, YEARS, 1, 12, true)
    with Serializable {
    protected def interpret(merger: CalendricalMerger, value: AnyRef): MonthOfYear = {
      if (value.isInstanceOf[Integer]) {
        var value: Int = value.toInt

        if (value < 1 || value > 12) {
          merger.addToOverflow(Period.ofMonths(value - 1))
          value = 1
        }
        return MonthOfYear.of(value)
      }

      return null
    }

    protected def derive(calendrical: Calendrical): MonthOfYear = {
      var date: LocalDate = calendrical.get(LocalDate.rule)
      return if (date != null) date.getMonthOfYear else null
    }

    override def convertIntToValue(value: Int): MonthOfYear = {
      return MonthOfYear.of(value)
    }

    def convertValueToInt(value: MonthOfYear): Int = {
      return value.getValue
    }

    private def readResolve: AnyRef = MonthOfYearRule

    protected def createTextStores(textStores: EnumMap[TextStyle.type, DateTimeFieldRule.TextStore], locale: Locale): Unit = {
      var oldSymbols: DateFormatSymbols = new DateFormatSymbols(locale)
      var array: Array[String] = oldSymbols.getMonths
      var map: Map[Integer, String] = new HashMap[Integer, String]
      map.put(1, array(Calendar.JANUARY))
      map.put(2, array(Calendar.FEBRUARY))
      map.put(3, array(Calendar.MARCH))
      map.put(4, array(Calendar.APRIL))
      map.put(5, array(Calendar.MAY))
      map.put(6, array(Calendar.JUNE))
      map.put(7, array(Calendar.JULY))
      map.put(8, array(Calendar.AUGUST))
      map.put(9, array(Calendar.SEPTEMBER))
      map.put(10, array(Calendar.OCTOBER))
      map.put(11, array(Calendar.NOVEMBER))
      map.put(12, array(Calendar.DECEMBER))
      textStores.put(TextStyle.FULL, new DateTimeFieldRule.TextStore(locale, map))
      array = oldSymbols.getShortMonths
      map.clear
      map.put(1, array(Calendar.JANUARY))
      map.put(2, array(Calendar.FEBRUARY))
      map.put(3, array(Calendar.MARCH))
      map.put(4, array(Calendar.APRIL))
      map.put(5, array(Calendar.MAY))
      map.put(6, array(Calendar.JUNE))
      map.put(7, array(Calendar.JULY))
      map.put(8, array(Calendar.AUGUST))
      map.put(9, array(Calendar.SEPTEMBER))
      map.put(10, array(Calendar.OCTOBER))
      map.put(11, array(Calendar.NOVEMBER))
      map.put(12, array(Calendar.DECEMBER))
      textStores.put(TextStyle.SHORT, new DateTimeFieldRule.TextStore(locale, map))
    }
  }

  private val WEEK_OF_MONTH_ORDINAL: Int = 12 * 16

  /**
   * Gets the period unit for millennia of 1000 years.
   * <p>
   * The period unit defines the concept of a period of a century.
   * <p>
   * The equivalent period and estimated duration are equal to 10 centuries.
   *
   * @return the period unit for millennia, never null
   */
  def periodMillennia: PeriodUnit = MILLENNIA

  /**
   * Gets the period unit for nanoseconds.
   * <p>
   * The period unit defines the concept of a period of a nanosecond.
   * <p>
   * This is a basic unit and has no equivalent period.
   * The estimated duration is 1 nanosecond.
   * <p>
   * See      { @link # nanoOfSecondRule ( ) } for the main date-time field.
   *
   * @return the period unit for nanoseconds, never null
   */
  def periodNanos: PeriodUnit = NANOS

  /**
   * Gets the period unit for weeks of 7 days.
   * <p>
   * The period unit defines the concept of a period of a week.
   * <p>
   * The equivalent period and estimated duration are equal to 7 days.
   * <p>
   * See      { @link # weekOfWeekBasedYearRule ( ) } and      { @link # weekOfYearRule ( ) } for
   * the main date-time fields.
   *
   * @return the period unit for weeks, never null
   */
  def periodWeeks: PeriodUnit = WEEKS

  private val SECOND_OF_DAY: Rule = new Rule(SECOND_OF_DAY_ORDINAL, "SecondOfDay", SECONDS, DAYS, 0, 86399, 86399)

  /**
   * Period unit for microseconds.
   */
  private val MICROS: Unit = new Unit(1 * 16, "Micros", PeriodField.of(1000, NANOS), Duration.ofNanos(1000))
  private val WEEK_OF_WEEK_BASED_YEAR: Rule = new Rule(WEEK_OF_WEEK_BASED_YEAR_ORDINAL, "WeekOfWeekBasedYear", WEEKS, WEEK_BASED_YEARS, 1, 53, 52)

  /**
   * Gets the rule for the month-of-quarter field in the ISO chronology.
   * <p>
   * This field counts months sequentially from the start of the quarter.
   * The first month of the quarter is 1 and the last is 3. Each quarter
   * lasts exactly three months.
   *
   * @return the rule for the month-of-quarter field, never null
   */
  def monthOfQuarterRule: DateTimeFieldRule[Int] = MONTH_OF_QUARTER

  /**
   * Gets the rule for the clock hour of AM/PM field from 1 to 24.
   * <p>
   * This field counts hours sequentially within the day starting from 1.
   * The values run from 1 to 24.
   *
   * @return the rule for the clock-hour-of-day field, never null
   */
  def clockHourOfDayRule: DateTimeFieldRule[Int] = CLOCK_HOUR_OF_DAY

  /**
   * Period unit for hours.
   */
  private val HOURS: Unit = new Unit(5 * 16, "Hours", PeriodField.of(60, MINUTES), Duration.ofSeconds(60 * 60))

  /**
   * Period unit for weeks.
   */
  private val WEEKS: Unit = new Unit(9 * 16, "Weeks", PeriodField.of(7, DAYS), Duration.ofSeconds(7L * 86400L))
  private val WEEK_BASED_YEAR: Rule = new Rule(WEEK_BASED_YEAR_ORDINAL, "WeekBasedYear", WEEK_BASED_YEARS, null, MIN_WEEK_BASED_YEAR, MAX_WEEK_BASED_YEAR, MAX_WEEK_BASED_YEAR)

  /**
   * Gets the rule for the day-of-week field.
   * <p>
   * This field uses the ISO-8601 values for the day-of-week.
   * These define Monday as value 1 to Sunday as value 7.
   * <p>
   * The enum      { @link DayOfWeek } should be used wherever possible in
   * applications when referring to the day of the week value to avoid
   * needing to remember the values from 1 to 7.
   *
   * @return the rule for the day-of-week field, never null
   */
  def dayOfWeekRule: DateTimeFieldRule[DayOfWeek] = DayOfWeekRule

  /**
   * Gets the period unit for quarters of 3 months.
   * <p>
   * The period unit defines the concept of a period of a quarter.
   * <p>
   * The equivalent period and estimated duration are equal to 3 months.
   * <p>
   * See      { @link # quarterOfYearRule ( ) } for the main date-time field.
   *
   * @return the period unit for quarters, never null
   */
  def periodQuarters: PeriodUnit = QUARTERS

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
  def yearRule: DateTimeFieldRule[Int] = YEAR

  /**
   * Calculates the week-based-year.
   *
   * @param date the date, not null
   * @return the week-based-year
   */
  private[calendar] def getWeekBasedYearFromDate(date: LocalDate): Int = {
    var year: Year = date.toYear
    if (date.getMonthOfYear == MonthOfYear.JANUARY) {
      var dom: Int = date.getDayOfMonth
      if (dom < 4) {
        var dow: Int = date.getDayOfWeek.getValue
        if (dow > dom + 3) {
          year = year.previous
        }
      }
    }
    else if (date.getMonthOfYear == MonthOfYear.DECEMBER) {
      var dom: Int = date.getDayOfMonth
      if (dom > 28) {
        var dow: Int = date.getDayOfWeek.getValue
        if (dow <= dom % 7) {
          year = year.next
        }
      }
    }
    return year.getValue
  }

  /**
   * Gets the rule for the week-based-year field in the ISO chronology.
   * <p>
   * This field is the year that results from calculating weeks with the ISO-8601 algorithm.
   * See      { @link # weekOfWeekBasedYearRule ( ) week of week-based-year } for details.
   * <p>
   * The week-based-year will either be 52 or 53 weeks long, depending on the
   * result of the algorithm for a particular date.
   *
   * @return the rule for the week-based-year field, never null
   */
  def weekBasedYearRule: DateTimeFieldRule[Int] = WEEK_BASED_YEAR

  /**
   * Gets the period unit for years of 12 months.
   * <p>
   * The period unit defines the concept of a period of a year.
   * <p>
   * The equivalent period and estimated duration are equal to 4 quarters.
   * <p>
   * See      { @link # yearRule ( ) } for the main date-time field.
   *
   * @return the period unit for years, never null
   */
  def periodYears: PeriodUnit = YEARS

  private val NANO_OF_SECOND: Rule = new Rule(NANO_OF_SECOND_ORDINAL, "NanoOfSecond", NANOS, SECONDS, 0, 999999999, 999999999)

  /**
   * Period unit for milliseconds.
   */
  private val MILLIS: Unit = new Unit(2 * 16, "Millis", PeriodField.of(1000, MICROS), Duration.ofMillis(1))

  /**
   * The number of seconds in one day.
   */
  private[calendar] val SECONDS_PER_DAY: Int = 60 * 60 * 24

  /**
   * Rule implementation.
   */
  private[calendar] object DayOfWeekRule extends DayOfWeekRule

  @SerialVersionUID(1L)
  private[calendar] sealed class DayOfWeekRule private
    extends DateTimeFieldRule[DayOfWeek](classOf[DayOfWeek], ISOChronology, "DayOfWeek", DAYS, WEEKS, 1, 7, true)
    with Serializable {
    override def convertIntToValue(value: Int): DayOfWeek = DayOfWeek.of(value)

    protected def createTextStores(textStores: EnumMap[TextStyle.type, DateTimeFieldRule.TextStore], locale: Locale): Unit = {
      var oldSymbols: DateFormatSymbols = new DateFormatSymbols(locale)
      var array: Array[String] = oldSymbols.getWeekdays
      var map: Map[Integer, String] = new HashMap[Integer, String]
      map.put(1, array(Calendar.MONDAY))
      map.put(2, array(Calendar.TUESDAY))
      map.put(3, array(Calendar.WEDNESDAY))
      map.put(4, array(Calendar.THURSDAY))
      map.put(5, array(Calendar.FRIDAY))
      map.put(6, array(Calendar.SATURDAY))
      map.put(7, array(Calendar.SUNDAY))
      textStores.put(TextStyle.FULL, new DateTimeFieldRule.TextStore(locale, map))
      array = oldSymbols.getShortWeekdays
      map.clear
      map.put(1, array(Calendar.MONDAY))
      map.put(2, array(Calendar.TUESDAY))
      map.put(3, array(Calendar.WEDNESDAY))
      map.put(4, array(Calendar.THURSDAY))
      map.put(5, array(Calendar.FRIDAY))
      map.put(6, array(Calendar.SATURDAY))
      map.put(7, array(Calendar.SUNDAY))
      textStores.put(TextStyle.SHORT, new DateTimeFieldRule.TextStore(locale, map))
    }

    protected def derive(calendrical: Calendrical): DayOfWeek = {
      var date: LocalDate = calendrical.get(LocalDate.rule)
      return if (date != null) getDayOfWeekFromDate(date) else null
    }

    private def readResolve: AnyRef = DayOfWeekRule

    protected def interpret(merger: CalendricalMerger, value: AnyRef): DayOfWeek = {
      if (value.isInstanceOf[Int]) {
        var intVal: Int = value.asInstanceOf[Int]
        if (intVal < 1 || intVal > 7) {
          merger.addToOverflow(Period.ofDays(intVal - 1))
          intVal = 1
        }
        return DayOfWeek.of(intVal)
      }
      return null
    }

    def convertValueToInt(value: DayOfWeek): Int = value.getValue
  }

  private val CLOCK_HOUR_OF_AMPM_ORDINAL: Int = 6 * 16

  /**
   * Gets the rule for the hour-of-day field.
   * <p>
   * This field counts hours sequentially from the start of the day.
   * The values run from 0 to 23.
   *
   * @return the rule for the hour-of-day field, never null
   */
  def hourOfDayRule: DateTimeFieldRule[Int] = HOUR_OF_DAY

  private val WEEK_OF_WEEK_BASED_YEAR_ORDINAL: Int = 13 * 16

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
  def periodEras: PeriodUnit = ERAS

  /**
   * The number of days from year zero to year 1970.
   * There are five 400 year cycles from year zero to 2000.
   * There are 7 leap years from 1970 to 2000.
   */
  private[calendar] val DAYS_0000_TO_1970: Long = (DAYS_PER_CYCLE * 5L) - (30L * 365L + 7L)
  private val DAY_OF_YEAR: Rule = new Rule(DAY_OF_YEAR_ORDINAL, "DayOfYear", DAYS, YEARS, 1, 366, 365)
  private val DAY_OF_MONTH: Rule = new Rule(DAY_OF_MONTH_ORDINAL, "DayOfMonth", DAYS, MONTHS, 1, 31, 28)

  /**
   * Period unit for days.
   */
  private val DAYS: Unit = new Unit(8 * 16, "Days", null, Duration.ofSeconds(86400))

  /**
   * Gets the rule for the day-of-year field in the ISO chronology.
   * <p>
   * This field counts days sequentially from the start of the year.
   * The first day of the year is 1 and the last is 365, or 366 in a leap year.
   *
   * @return the rule for the day-of-year field, never null
   */
  def dayOfYearRule: DateTimeFieldRule[Int] = DAY_OF_YEAR

  /**
   * Period unit for decades.
   */
  private val DECADES: Unit = new Unit(14 * 16, "Decades", PeriodField.of(10, YEARS), Duration.ofSeconds(10L * 31556952L))

  /**
   * Gets the rule for the day-of-month field in the ISO chronology.
   * <p>
   * This field counts days sequentially from the start of the month.
   * The first day of the month is 1 and the last is 28, 29, 30 or 31
   * depending on the month and whether it is a leap year.
   *
   * @return the rule for the day-of-month field, never null
   */
  def dayOfMonthRule: DateTimeFieldRule[Int] = DAY_OF_MONTH

  /**
   * Period unit for 24 hour fixed length days.
   */
  private val _24_HOURS: Unit = new Unit(7 * 16, "24Hours", PeriodField.of(2, _12_HOURS), Duration.ofSeconds(24 * 60 * 60))
  private val CLOCK_HOUR_OF_AMPM: Rule = new Rule(CLOCK_HOUR_OF_AMPM_ORDINAL, "ClockHourOfAmPm", HOURS, _12_HOURS, 1, 12, 12)

  /**
   * The start of months in a standard year.
   */
  private val STANDARD_MONTH_START: Array[Int] = Array[Int](0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)

  /**
   * The number of days from year zero to the Modified Julian Day epoch of 1858-11-17.
   */
  private[calendar] val DAYS_0000_TO_MJD_EPOCH: Long = 678941

  /**
   * Gets the period unit for twenty-four hours, that is often treated as a day.
   * <p>
   * The period unit defines the concept of a period of exactly 24 hours that
   * is often treated as a day. The unit name of "24Hours" is intended to convey
   * the fact that this is primarily a 24 hour unit that happens to be used as
   * a day unit on occasion. In most scenarios, the standard      { @link # periodDays ( ) Days }
   * unit is more applicable and accurate.
   * <p>
   * This chronology defines two units that could represent a day.
   * This unit,      { @code 24Hours }, represents a fixed length of exactly 24 hours,
   * allowing it to be converted to seconds, nanoseconds and      { @link Duration }.
   * By contrast, the      { @code Days } unit varies in length based on time-zone (daylight
   * savings time) changes and cannot be converted to seconds, nanoseconds or      { @code Duration }.
   * <p>
   * The equivalent period and estimated duration are equal to twice the
   * 12 hours unit, making it also equivalent to 24 hours.
   *
   * @return the period unit for fixed, 24 hour, days, never null
   */
  def period24Hours: PeriodUnit = _24_HOURS

  private val WEEK_OF_YEAR_ORDINAL: Int = 14 * 16
  private val SECOND_OF_MINUTE_ORDINAL: Int = 3 * 16

  /**
   * Period unit for eras.
   */
  private val ERAS: Unit = new Unit(17 * 16, "Eras", null, Duration.ofSeconds(31556952L * 2000000000L))
  private val MONTH_OF_QUARTER: Rule = new Rule(MONTH_OF_QUARTER_ORDINAL, "MonthOfQuarter", MONTHS, QUARTERS, 1, 3, 3)
  private val YEAR: Rule = new Rule(YEAR_ORDINAL, "Year", YEARS, null, Year.MIN_YEAR, Year.MAX_YEAR, Year.MAX_YEAR)

  /**
   * Gets the period unit for week-based-years.
   * <p>
   * The period unit defines the concept of a period of a week-based-year.
   * This is typically 52 weeks, and occasionally 53 weeks.
   * <p>
   * This is a basic unit and has no equivalent period.
   * The estimated duration is equal to 364.5 days, which is just over 5 weeks.
   * <p>
   * See      { @link # weekBasedYearRule ( ) } for the main date-time field.
   *
   * @return the period unit for week-based-years, never null
   */
  def periodWeekBasedYears: PeriodUnit = WEEK_BASED_YEARS

  private val MONTH_OF_QUARTER_ORDINAL: Int = 15 * 16
  private val WEEK_OF_MONTH: Rule = new Rule(WEEK_OF_MONTH_ORDINAL, "WeekOfMonth", WEEKS, MONTHS, 1, 5, 4)

  /**
   * Period unit for 12 hours half-days, used by AM/PM.
   */
  private val _12_HOURS: Unit = new Unit(6 * 16, "12Hours", PeriodField.of(12, HOURS), Duration.ofSeconds(12 * 60 * 60))

  /**
   * Gets the period unit for microseconds.
   * <p>
   * The period unit defines the concept of a period of a microsecond.
   * <p>
   * The equivalent period and estimated duration are equal to 1000 nanoseconds.
   *
   * @return the period unit for microseconds, never null
   */
  def periodMicros: PeriodUnit = MICROS

  private val SECOND_OF_DAY_ORDINAL: Int = 4 * 16

  /**
   * Gets the rule for the second-of-minute field.
   * <p>
   * This field counts seconds sequentially from the start of the minute.
   * The values run from 0 to 59.
   *
   * @return the rule for the second-of-minute field, never null
   */
  def secondOfMinuteRule: DateTimeFieldRule[Int] = SECOND_OF_MINUTE

  /**
   * Calculates the day-of-week from a date.
   *
   * @param date the date to use, not null
   * @return the day-of-week
   */
  private[calendar] def getDayOfWeekFromDate(date: LocalDate): DayOfWeek = {
    var mjd: Long = date.toModifiedJulianDays
    if (mjd < 0) {
      var weeks: Long = mjd / 7
      mjd += (-weeks + 1) * 7
    }
    var dow0: Int = ((mjd + 2) % 7).asInstanceOf[Int]
    return DayOfWeek.of(dow0 + 1)
  }

  private val HOUR_OF_AMPM_ORDINAL: Int = 7 * 16

  /**
   * Gets the period unit for milliseconds.
   * <p>
   * The period unit defines the concept of a period of a millisecond.
   * <p>
   * The equivalent period and estimated duration are equal to 1000 microseconds.
   * <p>
   * See      { @link # milliOfSecondRule ( ) } for the main date-time field.
   *
   * @return the period unit for milliseconds, never null
   */
  def periodMillis: PeriodUnit = MILLIS

  /**
   * Gets the rule for the AM/PM of day field.
   * <p>
   * This field defines the half-day AM/PM value. The hour-of-day from 0 to 11 is
   * defined as AM, while the hours from 12 to 23 are defined as PM.
   * AM is defined with the value 0, while PM is defined with the value 1.
   * <p>
   * The enum      { @link AmPmOfDay } should be used wherever possible in
   * applications when referring to the day of the week to avoid
   * hard-coding the values.
   *
   * @return the rule for the am/pm of day field, never null
   */
  def amPmOfDayRule: DateTimeFieldRule[AmPmOfDayRule] = AmPmOfDayRule

  /**
   * Gets the period unit for decades of 10 years.
   * <p>
   * The period unit defines the concept of a period of a decade.
   * <p>
   * The equivalent period and estimated duration are equal to 10 years.
   *
   * @return the period unit for decades, never null
   */
  def periodDecades: PeriodUnit = DECADES

  /**
   * Gets the rule for the milli-of-day field.
   * <p>
   * This field counts milliseconds sequentially from the start of the day.
   * The values run from 0 to 86,399,999.
   *
   * @return the rule for the milli-of-day field, never null
   */
  def milliOfDayRule: DateTimeFieldRule[Int] = MILLI_OF_DAY

  /**
   * Period unit for quarters.
   */
  private val QUARTERS: Unit = new Unit(11 * 16, "Quarters", PeriodField.of(3, MONTHS), Duration.ofSeconds(31556952L / 4))
  private val WEEK_OF_YEAR: Rule = new Rule(WEEK_OF_YEAR_ORDINAL, "WeekOfYear", WEEKS, YEARS, 1, 53, 53)

  /**
   * Gets the rule for the second-of-day field.
   * <p>
   * This field counts seconds sequentially from the start of the day.
   * The values run from 0 to 86399.
   *
   * @return the rule for the second-of-day field, never null
   */
  def secondOfDayRule: DateTimeFieldRule[Int] = SECOND_OF_DAY

  /**
   * Constant for the maximum week-based-year.
   */
  val MAX_WEEK_BASED_YEAR: Int = Year.MAX_YEAR

  /**
   * Calculates the day-of-year from a date.
   *
   * @param date the date to use, not null
   * @return the day-of-year
   */
  private[calendar] def getDayOfYearFromDate(date: LocalDate): Int = {
    var moy0: Int = date.getMonthOfYear.ordinal
    var dom: Int = date.getDayOfMonth
    if (isLeapYear(date.getYear)) LEAP_MONTH_START(moy0) + dom
    else STANDARD_MONTH_START(moy0) + dom
  }

  /**
   * Gets the rule for the clock hour of AM/PM field from 1 to 12.
   * <p>
   * This field counts hours sequentially within the half-day AM/PM as
   * normally seen on a clock or watch. The values run from 1 to 12.
   *
   * @return the rule for the hour of AM/PM field, never null
   */
  def clockHourOfAmPmRule: DateTimeFieldRule[Int] = CLOCK_HOUR_OF_AMPM

  private val HOUR_OF_DAY: Rule = new Rule(HOUR_OF_DAY_ORDINAL, "HourOfDay", HOURS, DAYS, 0, 23, 23)
  private val MILLI_OF_SECOND: Rule = new Rule(MILLI_OF_SECOND_ORDINAL, "MilliOfSecond", MILLIS, SECONDS, 0, 999, 999)

  /**
   * Constant for the minimum week-based-year.
   */
  val MIN_WEEK_BASED_YEAR: Int = Year.MIN_YEAR
  private val YEAR_ORDINAL: Int = 17 * 16

  /**
   * Gets the period unit for seconds.
   * <p>
   * The period unit defines the concept of a period of a second.
   * <p>
   * The equivalent period and estimated duration are equal to 1000 milliseconds.
   * <p>
   * See      { @link # secondOfMinuteRule ( ) } for the main date-time field.
   *
   * @return the period unit for seconds, never null
   */
  def periodSeconds: PeriodUnit = SECONDS

  /**
   * Period unit for months.
   */
  private val MONTHS: Unit = new Unit(10 * 16, "Months", null, Duration.ofSeconds(31556952L / 12L))
  private val DAY_OF_YEAR_ORDINAL: Int = 11 * 16

  /**
   * Gets the rule for the quarter-of-year field in the ISO chronology.
   * <p>
   * This field counts quarters sequentially from the start of the year.
   * The first quarter of the year is 1 and the last is 4. Each quarter
   * lasts exactly three months.
   *
   * @return the rule for the quarter-of-year field, never null
   */
  def quarterOfYearRule: DateTimeFieldRule[QuarterOfYearRule] = QuarterOfYearRule

  /**
   * Validates that the input value is not null.
   *
   * @param object the object to check
   * @param errorMessage the error to throw
   * @throws NullPointerException if the object is null
   */
  private[calendar] def checkNotNull(obj: AnyRef, errorMessage: String): Unit = {
    if (obj == null) throw new NullPointerException(errorMessage)
  }

  /**
   * Gets the rule for the week-of-month field in the ISO chronology.
   * <p>
   * This field counts weeks in groups of seven days starting from the first
   * day of the month. The 1st to the 7th of a month is always week 1 while the
   * 8th to the 14th is always week 2 and so on.
   * <p>
   * This field can be used to create concepts such as 'the second Saturday'
   * of a month. To achieve this, setup a      { @link DateTimeFields } instance
   * using this rule and the      { @link # dayOfWeekRule ( ) day-of-week } rule.
   *
   * @return the rule for the week-of-month field, never null
   */
  def weekOfMonthRule: DateTimeFieldRule[Int] = WEEK_OF_MONTH

  /**
   * Gets the period unit for twelve hours, as used by AM/PM.
   * <p>
   * The period unit defines the concept of a period of 12 hours.
   * <p>
   * The equivalent period and estimated duration are equal to 12 hours.
   * <p>
   * See      { @link # amPmOfDayRule ( ) } for the main date-time field.
   *
   * @return the period unit for twelve hours, never null
   */
  def period12Hours: PeriodUnit = _12_HOURS

  /**
   * Rule implementation.
   */
  private[calendar] object AmPmOfDayRule extends AmPmOfDayRule

  @SerialVersionUID(1L)
  private[calendar] sealed class AmPmOfDayRule
    extends DateTimeFieldRule[AmPmOfDay](classOf[AmPmOfDay], ISOChronology, "AmPmOfDay", _12_HOURS, DAYS, 0, 1, true) with Serializable {
    private def readResolve: AnyRef = AmPmOfDayRule

    override def convertIntToValue(value: Int): AmPmOfDay = AmPmOfDay.of(value)

    protected def createTextStores(textStores: EnumMap[TextStyle.type, DateTimeFieldRule.TextStore], locale: Locale): Unit = {
      var oldSymbols: DateFormatSymbols = new DateFormatSymbols(locale)
      var array: Array[String] = oldSymbols.getAmPmStrings
      var map: Map[Int, String] = new HashMap[Int, String]
      map.put(0, array(Calendar.AM))
      map.put(1, array(Calendar.PM))
      var textStore: DateTimeFieldRule.TextStore = new DateTimeFieldRule.TextStore(locale, map)
      textStores.put(TextStyle.FULL, textStore)
      textStores.put(TextStyle.SHORT, textStore)
    }

    def convertValueToInt(value: AmPmOfDay): Int = value.getValue

    protected def interpret(merger: CalendricalMerger, value: AnyRef): AmPmOfDay = {
      if (value.isInstanceOf[Int]) {
        var intVal: Int = value.asInstanceOf[Int]

        if (intVal < 0 || intVal > 1) {
          var days: Int = if (intVal > 0) intVal / 2 else ((intVal + 1) / 2) - 1
          merger.addToOverflow(Period.ofDays(days))
          intVal = (if (intVal > 0) intVal % 2 else -(intVal % 2))
        }
        return AmPmOfDay.of(intVal)
      }

      return null
    }

    protected def derive(calendrical: Calendrical): AmPmOfDay = {
      var hourVal: Int = calendrical.get(hourOfDayRule)
      if (hourVal == null) {
        return null
      }
      var hour: Int = hourVal
      hour = (if (hour < 0) 1073741832 + hour + 1073741832 else hour)
      return AmPmOfDay.of((hour % 24) / 12)
    }
  }

  private val CLOCK_HOUR_OF_DAY: Rule = new Rule(CLOCK_HOUR_OF_DAY_ORDINAL, "ClockHourOfDay", HOURS, DAYS, 1, 24, 24)

  /**
   * Period unit for millennia.
   */
  private val MILLENNIA: Unit = new Unit(16 * 16, "Millennia", PeriodField.of(10, CENTURIES), Duration.ofSeconds(1000L * 31556952L))
  private val SECOND_OF_MINUTE: Rule = new Rule(SECOND_OF_MINUTE_ORDINAL, "SecondOfMinute", SECONDS, MINUTES, 0, 59, 59)

  /**
   * Single unit subclass, which means fewer classes to load at startup.
   */
  @SerialVersionUID(1L)
  private[calendar] sealed class Unit(ordinal: Int, name: String, equivalentPeriod: PeriodField, estimatedDuration: Duration)
    extends PeriodUnit(name, equivalentPeriod, estimatedDuration) {

    def compareTo(other: PeriodUnit): Int = {
      if (other.isInstanceOf[Unit]) ordinal - (other.asInstanceOf[Unit]).ordinal
      else super.compareTo(other)
    }

    private def readResolve: AnyRef = UNIT_CACHE(ordinal / 16)

    override def equals(obj: AnyRef): Boolean = {
      if (obj.isInstanceOf[Unit]) ordinal == (obj.asInstanceOf[Unit]).ordinal
      else super.equals(obj)
    }

  }

  /**
   * Gets the rule for the week-of-week-based-year field in the ISO chronology.
   * <p>
   * This field counts weeks using the ISO-8601 algorithm.
   * The first week of the year is the week which has at least 4 days in the year
   * using a Monday to Sunday week definition. Thus it is possible for the first
   * week to start on any day from the 29th December in the previous year to the
   * 4th January in the new year. The year which is aligned with this field is
   * known as the      { @link # weekBasedYearRule ( ) week-based-year }.
   *
   * @return the rule for the week-of-week-based-year field, never null
   */
  def weekOfWeekBasedYearRule: DateTimeFieldRule[Int] = WEEK_OF_WEEK_BASED_YEAR

  /**
   * Period unit for years.
   */
  private val YEARS: Unit = new Unit(13 * 16, "Years", PeriodField.of(4, QUARTERS), Duration.ofSeconds(31556952L))
  /**
   * Gets the period unit for minutes of 60 seconds.
   * <p>
   * The period unit defines the concept of a period of a minute.
   * <p>
   * The equivalent period and estimated duration are equal to 60 seconds.
   * <p>
   * See      { @link # minuteOfHourRule ( ) } for the main date-time field.
   *
   * @return the period unit for minutes, never null
   */
  def periodMinutes: PeriodUnit = MINUTES

  private val HOUR_OF_DAY_ORDINAL: Int = 9 * 16
  private val CLOCK_HOUR_OF_DAY_ORDINAL: Int = 8 * 16
  /**
   * Period unit for week-based-years.
   */
  private val WEEK_BASED_YEARS: Unit = new Unit(12 * 16, "WeekBasedYears", null, Duration.ofSeconds(364L * 86400L + 43200L))

  /**
   * Rule implementation.
   */
  private[calendar] object NanoOfDayRule extends NanoOfDayRule

  @SerialVersionUID(1L)
  private[calendar] sealed class NanoOfDayRule
    extends CalendricalRule[Long](classOf[Long], ISOChronology, "NanoOfDay", NANOS, DAYS) with Serializable {
    protected override def derive(calendrical: Calendrical): Long = {
      var time: LocalTime = calendrical.get(LocalTime.rule)
      return if (time != null) time.toNanoOfDay else null
    }

    private def readResolve: AnyRef = NanoOfDayRule

    protected override def merge(merger: CalendricalMerger): Unit = {
      var nod: Long = merger.getValue(this)
      merger.storeMerged(LocalTime.rule, LocalTime.ofNanoOfDay(nod))
      merger.removeProcessed(this)
    }

  }

  private val NANO_OF_SECOND_ORDINAL: Int = 0 * 16

  /**
   * The singleton instance of      { @code ISOChronology }.
   */
  object ISOChronology extends ISOChronology

  /**
   * Gets the rule for the month-of-year field in the ISO chronology.
   * <p>
   * This field counts months sequentially from the start of the year.
   * The values follow the ISO-8601 standard and normal human interactions.
   * These define January as value 1 to December as value 12.
   * <p>
   * The enum      { @link MonthOfYear } should be used wherever possible in
   * applications when referring to the day of the week to avoid
   * hard-coding the values.
   *
   * @return the rule for the month-of-year field, never null
   */
  def monthOfYearRule: DateTimeFieldRule[MonthOfYear] = MonthOfYearRule

  /**
   * Period unit for nanoseconds.
   */
  private val NANOS: Unit = new Unit(0 * 16, "Nanos", null, Duration.ofNanos(1))

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
   * @param year the year to check, not validated for range
   * @return true if the year is a leap year
   */
  def isLeapYear(year: Int): Boolean = ((year & 3) == 0) && ((year % 100) != 0 || (year % 400) == 0)

  /**
   * Cache of units for deserialization.
   * Indices must match ordinal passed to rule constructor.
   */
  private val RULE_CACHE: Array[Rule] = Array[Rule](NANO_OF_SECOND, MILLI_OF_SECOND, MILLI_OF_DAY, SECOND_OF_MINUTE, SECOND_OF_DAY, MINUTE_OF_HOUR, CLOCK_HOUR_OF_AMPM, HOUR_OF_AMPM, CLOCK_HOUR_OF_DAY, HOUR_OF_DAY, DAY_OF_MONTH, DAY_OF_YEAR, WEEK_OF_MONTH, WEEK_OF_WEEK_BASED_YEAR, WEEK_OF_YEAR, MONTH_OF_QUARTER, WEEK_BASED_YEAR, YEAR)

  /**
   * Period unit for minutes.
   */
  private val MINUTES: Unit = new Unit(4 * 16, "Minutes", PeriodField.of(60, SECONDS), Duration.ofSeconds(60))

  /**
   * Gets the rule for the minute-of-hour field.
   * <p>
   * This field counts minutes sequentially from the start of the hour.
   * The values run from 0 to 59.
   *
   * @return the rule for the minute-of-hour field, never null
   */
  def minuteOfHourRule: DateTimeFieldRule[Int] = MINUTE_OF_HOUR

  /**
   * Calculates the week of week-based-year.
   *
   * @param date the date to use, not null
   * @return the week
   */
  private[calendar] def getWeekOfWeekBasedYearFromDate(date: LocalDate): Int = {
    var wby: Int = getWeekBasedYearFromDate(date)
    var yearStart: LocalDate = LocalDate.of(wby, MonthOfYear.JANUARY, 4)
    return MathUtils.safeToInt((date.toModifiedJulianDays - yearStart.toModifiedJulianDays + yearStart.getDayOfWeek.getValue - 1) / 7 + 1)
  }

  /**
   * Period unit for centuries.
   */
  private val CENTURIES: Unit = new Unit(15 * 16, "Centuries", PeriodField.of(10, DECADES), Duration.ofSeconds(100L * 31556952L))

  /**
   * Gets the rule for the week-of-year field in the ISO chronology.
   * <p>
   * This field counts weeks in groups of seven days starting from the first
   * of January. The 1st to the 7th of January is always week 1 while the
   * 8th to the 14th is always week 2.
   *
   * @return the rule for the week-of-year field, never null
   */
  def weekOfYearRule: DateTimeFieldRule[Int] = WEEK_OF_YEAR

  private val MILLI_OF_DAY_ORDINAL: Int = 2 * 16

  /**
   * Cache of units for deserialization.
   * Indices must match ordinal passed to unit constructor.
   */
  private val UNIT_CACHE: Array[Unit] = Array[Unit](NANOS, MICROS, MILLIS, SECONDS, MINUTES, HOURS, _12_HOURS, _24_HOURS, DAYS, WEEKS, MONTHS, QUARTERS, WEEK_BASED_YEARS, YEARS, DECADES, CENTURIES, MILLENNIA, ERAS)

  /**
   * Rule implementation.
   */
  private[calendar] object QuarterOfYearRule extends QuarterOfYearRule

  private[calendar] sealed class QuarterOfYearRule
    extends DateTimeFieldRule[QuarterOfYear](classOf[QuarterOfYear], ISOChronology, "QuarterOfYear", QUARTERS, YEARS, 1, 4)
    with Serializable {


    private def readResolve: AnyRef = QuarterOfYearRule

    def convertValueToInt(value: QuarterOfYear): Int = value.getValue

    protected def derive(calendrical: Calendrical): QuarterOfYear = {
      var moy: MonthOfYear = calendrical.get(monthOfYearRule)
      return if (moy != null) QuarterOfYear.of(moy.ordinal / 3 + 1) else null
    }

    override def convertIntToValue(value: Int): QuarterOfYear = QuarterOfYear.of(value)
  }

  private val DAY_OF_MONTH_ORDINAL: Int = 10 * 16

  /**
   * Calculates the date from a year and day-of-year.
   *
   * @param year the year, valid
   * @param dayOfYear the day-of-year, valid
   * @return the date, never null
   */
  private[calendar] def getDateFromDayOfYear(year: Int, dayOfYear: Int): LocalDate = {
    var leap: Boolean = isLeapYear(year)
    if (dayOfYear == 366 && leap == false) {
      throw new InvalidCalendarFieldException("DayOfYear 366 is invalid for year " + year, dayOfYearRule)
    }

    var doy0: Int = dayOfYear - 1
    var array: Array[Int] = (if (leap) LEAP_MONTH_START else STANDARD_MONTH_START)
    var month: Int = 1
    while (month < 12) {
      {
        if (doy0 < array(month)) {
          break //todo: break is not supported
        }
      }
      ({
        month += 1;
        month
      })
    }
    var moy: MonthOfYear = MonthOfYear.of(month)
    var dom: Int = dayOfYear - array(month - 1)
    return LocalDate.of(year, moy, dom)
  }

  /**
   * Gets the rule for the hour of AM/PM field from 0 to 11.
   * <p>
   * This field counts hours sequentially from the start of the half-day AM/PM.
   * The values run from 0 to 11.
   *
   * @return the rule for the hour of AM/PM field, never null
   */
  def hourOfAmPmRule: DateTimeFieldRule[Int] = HOUR_OF_AMPM

  /**
   * Gets the period unit for centuries of 100 years.
   * <p>
   * The period unit defines the concept of a period of a century.
   * <p>
   * The equivalent period and estimated duration are equal to 10 decades.
   *
   * @return the period unit for centuries, never null
   */
  def periodCenturies: PeriodUnit = CENTURIES

  /**
   * Single rule subclass, which means fewer classes to load at startup.
   */
  @SerialVersionUID(1L)
  private[calendar] sealed class Rule (private val ordinal: Int, name: String, periodUnit: PeriodUnit, periodRange: PeriodUnit, minimumValue: Int, maximumValue: Int, @transient smallestMaximum: Int)
    extends DateTimeFieldRule[Int](classOf[Int], ISOChronology, name, periodUnit, periodRange, minimumValue, maximumValue) with Serializable {

    override def getSmallestMaximumValue: Int = smallestMaximum

    protected def derive(calendrical: Calendrical): Integer = {
      ordinal match {
        case NANO_OF_SECOND_ORDINAL => {
          var time: LocalTime = calendrical.get(LocalTime.rule)
          return if (time != null) time.getNanoOfSecond else null
        }
        case MILLI_OF_SECOND_ORDINAL => {
          var time: LocalTime = calendrical.get(LocalTime.rule)
          return if (time != null) time.getNanoOfSecond / 1000000 else null
        }
        case MILLI_OF_DAY_ORDINAL => {
          var time: LocalTime = calendrical.get(LocalTime.rule)
          return if (time != null) (time.toNanoOfDay / 1000000L).asInstanceOf[Int] else null
        }
        case SECOND_OF_MINUTE_ORDINAL => {
          var time: LocalTime = calendrical.get(LocalTime.rule)
          return if (time != null) time.getSecondOfMinute else null
        }
        case SECOND_OF_DAY_ORDINAL => {
          var time: LocalTime = calendrical.get(LocalTime.rule)
          return if (time != null) time.toSecondOfDay else null
        }
        case MINUTE_OF_HOUR_ORDINAL => {
          var time: LocalTime = calendrical.get(LocalTime.rule)
          return if (time != null) time.getMinuteOfHour else null
        }
        case CLOCK_HOUR_OF_AMPM_ORDINAL => {
          var hourVal: Int = calendrical.get(hourOfAmPmRule)
          return if (hourVal != null) (hourVal + 12) % 13 else null
        }
        case HOUR_OF_AMPM_ORDINAL => {
          var hourVal: Int = calendrical.get(hourOfDayRule)
          return if (hourVal != null) hourVal % 12 else null
        }
        case CLOCK_HOUR_OF_DAY_ORDINAL => {
          var hourVal: Int = calendrical.get(hourOfDayRule)
          return if (hourVal != null) (hourVal + 24) % 25 else null
        }
        case HOUR_OF_DAY_ORDINAL => {
          var time: LocalTime = calendrical.get(LocalTime.rule)
          return if (time != null) time.getHourOfDay else null
        }
        case DAY_OF_MONTH_ORDINAL => {
          var date: LocalDate = calendrical.get(LocalDate.rule)
          return if (date != null) date.getDayOfMonth else null
        }
        case DAY_OF_YEAR_ORDINAL => {
          var date: LocalDate = calendrical.get(LocalDate.rule)
          return if (date != null) getDayOfYearFromDate(date) else null
        }
        case MONTH_OF_QUARTER_ORDINAL => {
          var moy: MonthOfYear = calendrical.get(monthOfYearRule)
          return if (moy != null) (moy.ordinal % 3 + 1) else null
        }
        case WEEK_OF_MONTH_ORDINAL => {
          var domVal: Int = calendrical.get(dayOfMonthRule)
          return if (domVal != null) (domVal + 6) / 7 else null
        }
        case WEEK_OF_WEEK_BASED_YEAR_ORDINAL => {
          var date: LocalDate = calendrical.get(LocalDate.rule)
          return if (date != null) getWeekOfWeekBasedYearFromDate(date) else null
        }
        case WEEK_OF_YEAR_ORDINAL => {
          var doyVal: Int = calendrical.get(dayOfYearRule)
          return if (doyVal != null) (doyVal + 6) / 7 else null
        }
        case WEEK_BASED_YEAR_ORDINAL => {
          var date: LocalDate = calendrical.get(LocalDate.rule)
          return if (date != null) getWeekBasedYearFromDate(date) else null
        }
        case YEAR_ORDINAL => {
          var date: LocalDate = calendrical.get(LocalDate.rule)
          return if (date != null) date.getYear else null
        }
      }
      return null
    }

    private def readResolve: AnyRef = RULE_CACHE(ordinal / 16)

    def compareTo(other: CalendricalRule[Int]): Int = {
      if (other.isInstanceOf[Rule]) ordinal - (other.asInstanceOf[Rule]).ordinal
      else super.compareTo(other)
    }

    override def getMaximumValue(calendrical: Calendrical): Int = {
      ordinal match {
        case DAY_OF_MONTH_ORDINAL => {
          var moy: MonthOfYear = calendrical.get(monthOfYearRule)
          if (moy == null) {
            return 31
          }
          var year: Int = calendrical.get(yearRule)
          return if (year != null) moy.lengthInDays(isLeapYear(year)) else moy.maxLengthInDays
        }
        case DAY_OF_YEAR_ORDINAL => {
          var year: Int = calendrical.get(yearRule)
          return (if (year != null && isLeapYear(year) == false) 365 else 366)
        }
        case WEEK_OF_MONTH_ORDINAL => {
          var year: Int = calendrical.get(yearRule)
          var moy: MonthOfYear = calendrical.get(monthOfYearRule)
          if (year != null && moy == MonthOfYear.FEBRUARY) {
            return if (isLeapYear(year)) 5 else 4
          }
          return getMaximumValue
        }
        case WEEK_OF_WEEK_BASED_YEAR_ORDINAL => {
          var date: LocalDate = calendrical.get(LocalDate.rule)
          if (date == null) {
            return 53
          }
          date = date.withDayOfMonth(1).withMonthOfYear(1)
          if (date.getDayOfWeek == DayOfWeek.THURSDAY || (date.getDayOfWeek == DayOfWeek.WEDNESDAY && isLeapYear(date.getYear))) {
            return 53
          }
          return 52
        }
      }
      return super.getMaximumValue
    }

    override def equals(obj: AnyRef): Boolean = {
      if (obj.isInstanceOf[Rule]) ordinal == (obj.asInstanceOf[Rule]).ordinal
      else super.equals(obj)
    }
  }

    /**
     * Gets the period unit for hours of 60 minutes.
     * <p>
     * The period unit defines the concept of a period of a hour.
     * <p>
     * The equivalent period and estimated duration are equal to 60 minutes.
     * <p>
     * See      { @link # hourOfDayRule ( ) } for the main date-time field.
     *
     * @return the period unit for hours, never null
     */
    def periodHours: PeriodUnit = HOURS

    /**
     * The start of months in a leap year.
     */
    private val LEAP_MONTH_START: Array[Int] = Array[Int](0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
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
     * The number of days in a 400 year cycle.
     */
    private[calendar] val DAYS_PER_CYCLE: Int = 146097
    private val MINUTE_OF_HOUR: Rule = new Rule(MINUTE_OF_HOUR_ORDINAL, "MinuteOfHour", MINUTES, HOURS, 0, 59, 59)
    private val HOUR_OF_AMPM: Rule = new Rule(HOUR_OF_AMPM_ORDINAL, "HourOfAmPm", HOURS, _12_HOURS, 0, 11, 11)
    /**
     * Gets the rule for the epoch-days field.
     * <p>
     * This field counts seconds sequentially from the Java epoch of 1970-01-01.
     *
     * @return the rule for the epoch-days field, never null
     */
    def epochDays: CalendricalRule[Long] = EpochDaysRule

    /**
     * Gets the period unit for days.
     * <p>
     * The period unit defines the concept of a period of a day.
     * This is typically equal to 24 hours, but may vary due to time-zone changes.
     * <p>
     * This chronology defines two units that could represent a day.
     * This unit,      { @code Days }, represents a day that varies in length based on
     * time-zone (daylight savings time) changes. It is a basic unit that cannot
     * be converted to seconds, nanoseconds or      { @link Duration }.
     * By contrast, the      { @link # period24Hours ( ) 24Hours } unit has a fixed length of
     * exactly 24 hours allowing it to be converted to seconds, nanoseconds and      { @code Duration }.
     * <p>
     * This is a basic unit and has no equivalent period.
     * The estimated duration is equal to 24 hours.
     * <p>
     * See      { @link # dayOfMonthRule ( ) } for the main date-time field.
     *
     * @return the period unit for accurate, variable length, days, never null
     */
    def periodDays: PeriodUnit = DAYS

    private val WEEK_BASED_YEAR_ORDINAL: Int = 16 * 16
    /**
     * Gets the rule for the milli-of-second field.
     * <p>
     * This field counts milliseconds sequentially from the start of the second.
     * The values run from 0 to 999.
     *
     * @return the rule for the milli-of-second field, never null
     */
    def milliOfSecondRule: DateTimeFieldRule[Int] = MILLI_OF_SECOND
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
     * Merges the set of fields known by this chronology.
     *
     * @param merger the merger to use, not null
     */
    private[calendar] def merge(merger: CalendricalMerger): Unit = {
      var modVal: Int = merger.getValue(ISOChronology.milliOfDayRule)
      if (modVal != null) {
        merger.storeMerged(LocalTime.rule, LocalTime.ofNanoOfDay(modVal * 1000000L))
        merger.removeProcessed(ISOChronology.milliOfDayRule)
      }
      var sodVal: Int = merger.getValue(ISOChronology.secondOfDayRule)
      if (modVal != null) {
        var nosVal: Int = merger.getValue(ISOChronology.nanoOfSecondRule)
        if (nosVal != null) {
          merger.storeMerged(LocalTime.rule, LocalTime.ofSecondOfDay(sodVal, nosVal))
          merger.removeProcessed(ISOChronology.nanoOfSecondRule)
        }
        else {
          var mosVal: Int = merger.getValue(ISOChronology.milliOfSecondRule)
          if (mosVal != null) {
            merger.storeMerged(LocalTime.rule, LocalTime.ofSecondOfDay(sodVal, mosVal * 1000000))
            merger.removeProcessed(ISOChronology.milliOfSecondRule)
          }
          else {
            merger.storeMerged(LocalTime.rule, LocalTime.ofSecondOfDay(sodVal))
          }
        }
        merger.removeProcessed(ISOChronology.secondOfDayRule)
      }
      var amPm: AmPmOfDay = merger.getValue(ISOChronology.amPmOfDayRule)
      if (amPm != null) {
        var hapVal: Int = merger.getValue(ISOChronology.hourOfAmPmRule)
        if (hapVal != null) {
          var hourOfDay: Int = amPm.getValue * 12 + hapVal
          merger.storeMerged(ISOChronology.hourOfDayRule, hourOfDay)
          merger.removeProcessed(ISOChronology.amPmOfDayRule)
          merger.removeProcessed(ISOChronology.hourOfAmPmRule)
        }
        var chapVal: Int = merger.getValue(ISOChronology.hourOfAmPmRule)
        if (chapVal != null) {
          var hourOfDay: Int = amPm.getValue * 12 + chapVal
          if (hourOfDay == 24) {
            merger.addToOverflow(Period.ofDays(1))
            hourOfDay = 0
          }
          merger.storeMerged(ISOChronology.hourOfDayRule, hourOfDay)
          merger.removeProcessed(ISOChronology.amPmOfDayRule)
          merger.removeProcessed(ISOChronology.clockHourOfAmPmRule)
        }
      }
      var hourVal: Int = merger.getValue(ISOChronology.hourOfDayRule)
      if (hourVal != null) {
        var minuteVal: Int = merger.getValue(ISOChronology.minuteOfHourRule)
        var secondVal: Int = merger.getValue(ISOChronology.secondOfMinuteRule)
        var mosVal: Int = merger.getValue(ISOChronology.milliOfSecondRule)
        var nanoVal: Int = merger.getValue(ISOChronology.nanoOfSecondRule)
        if (minuteVal != null && secondVal != null && nanoVal != null) {
          merger.storeMerged(LocalTime.rule, LocalTime.of(hourVal, minuteVal, secondVal, nanoVal))
          merger.removeProcessed(ISOChronology.hourOfDayRule)
          merger.removeProcessed(ISOChronology.minuteOfHourRule)
          merger.removeProcessed(ISOChronology.secondOfMinuteRule)
          merger.removeProcessed(ISOChronology.nanoOfSecondRule)
        }
        else if (minuteVal != null && secondVal != null && mosVal != null) {
          merger.storeMerged(LocalTime.rule, LocalTime.of(hourVal, minuteVal, secondVal, mosVal * 1000000))
          merger.removeProcessed(ISOChronology.hourOfDayRule)
          merger.removeProcessed(ISOChronology.minuteOfHourRule)
          merger.removeProcessed(ISOChronology.secondOfMinuteRule)
          merger.removeProcessed(ISOChronology.milliOfSecondRule)
        }
        else if (minuteVal != null && secondVal != null) {
          merger.storeMerged(LocalTime.rule, LocalTime.of(hourVal, minuteVal, secondVal, 0))
          merger.removeProcessed(ISOChronology.hourOfDayRule)
          merger.removeProcessed(ISOChronology.minuteOfHourRule)
          merger.removeProcessed(ISOChronology.secondOfMinuteRule)
        }
        else if (minuteVal != null) {
          merger.storeMerged(LocalTime.rule, LocalTime.of(hourVal, minuteVal, 0, 0))
          merger.removeProcessed(ISOChronology.hourOfDayRule)
          merger.removeProcessed(ISOChronology.minuteOfHourRule)
        }
        else {
          merger.storeMerged(LocalTime.rule, LocalTime.of(hourVal, 0))
          merger.removeProcessed(ISOChronology.hourOfDayRule)
        }
      }
      var qoy: QuarterOfYear = merger.getValue(ISOChronology.quarterOfYearRule)
      var moqVal: Int = merger.getValue(ISOChronology.monthOfQuarterRule)
      if (qoy != null && moqVal != null) {
        var moy: MonthOfYear = MonthOfYear.of(qoy.getFirstMonthOfQuarter.ordinal + moqVal)
        merger.storeMerged(ISOChronology.monthOfYearRule, moy)
        merger.removeProcessed(ISOChronology.quarterOfYearRule)
        merger.removeProcessed(ISOChronology.monthOfQuarterRule)
      }
      var yearVal: Int = merger.getValue(ISOChronology.yearRule)
      if (yearVal != null) {
        var moy: MonthOfYear = merger.getValue(ISOChronology.monthOfYearRule)
        var domVal: Int = merger.getValue(ISOChronology.dayOfMonthRule)
        if (moy != null && domVal != null) {
          var date: LocalDate = merger.getContext.resolveDate(yearVal, moy.getValue, domVal)
          merger.storeMerged(LocalDate.rule, date)
          merger.removeProcessed(ISOChronology.yearRule)
          merger.removeProcessed(ISOChronology.monthOfYearRule)
          merger.removeProcessed(ISOChronology.dayOfMonthRule)
        }
        var doyVal: Int = merger.getValue(ISOChronology.dayOfYearRule)
        if (doyVal != null) {
          merger.storeMerged(LocalDate.rule, getDateFromDayOfYear(yearVal, doyVal))
          merger.removeProcessed(ISOChronology.yearRule)
          merger.removeProcessed(ISOChronology.dayOfYearRule)
        }
        var woyVal: Int = merger.getValue(ISOChronology.weekOfYearRule)
        var dow: DayOfWeek = merger.getValue(ISOChronology.dayOfWeekRule)
        if (woyVal != null && dow != null) {
          var date: LocalDate = LocalDate.of(yearVal, 1, 1).plusWeeks(woyVal - 1)
          date = date.`with`(DateAdjusters.nextOrCurrent(dow))
          merger.storeMerged(LocalDate.rule, date)
          merger.removeProcessed(ISOChronology.yearRule)
          merger.removeProcessed(ISOChronology.weekOfYearRule)
          merger.removeProcessed(ISOChronology.dayOfWeekRule)
        }
        var womVal: Int = merger.getValue(ISOChronology.weekOfMonthRule)
        if (moy != null && womVal != null && dow != null) {
          var date: LocalDate = LocalDate.of(yearVal, moy, 1).plusWeeks(womVal - 1)
          date = date.`with`(DateAdjusters.nextOrCurrent(dow))
          merger.storeMerged(LocalDate.rule, date)
          merger.removeProcessed(ISOChronology.yearRule)
          merger.removeProcessed(ISOChronology.monthOfYearRule)
          merger.removeProcessed(ISOChronology.weekOfMonthRule)
          merger.removeProcessed(ISOChronology.dayOfWeekRule)
        }
      }
      var wbyVal: Int = merger.getValue(ISOChronology.weekBasedYearRule)
      if (wbyVal != null) {
        var woy: Int = merger.getValue(ISOChronology.weekOfWeekBasedYearRule)
        var dow: DayOfWeek = merger.getValue(ISOChronology.dayOfWeekRule)
        if (woy != null && dow != null) {
          merger.removeProcessed(ISOChronology.weekBasedYearRule)
          merger.removeProcessed(ISOChronology.weekOfWeekBasedYearRule)
          merger.removeProcessed(ISOChronology.dayOfWeekRule)
        }
      }
      var date: LocalDate = merger.getValue(LocalDate.rule)
      var time: LocalTime = merger.getValue(LocalTime.rule)
      var offset: ZoneOffset = merger.getValue(ZoneOffset.rule)
      var zone: TimeZone = merger.getValue(TimeZone.rule)
      if (date != null && time != null) {
        merger.storeMerged(LocalDateTime.rule, LocalDateTime.of(date, time))
        merger.removeProcessed(LocalDate.rule)
        merger.removeProcessed(LocalTime.rule)
      }
      if (date != null && offset != null) {
        merger.storeMerged(OffsetDate.rule, OffsetDate.of(date, offset))
        merger.removeProcessed(LocalDate.rule)
        merger.removeProcessed(ZoneOffset.rule)
      }
      if (time != null && offset != null) {
        merger.storeMerged(OffsetTime.rule, OffsetTime.of(time, offset))
        merger.removeProcessed(LocalTime.rule)
        merger.removeProcessed(ZoneOffset.rule)
      }
      var ldt: LocalDateTime = merger.getValue(LocalDateTime.rule)
      if (ldt != null && offset != null) {
        merger.storeMerged(OffsetDateTime.rule, OffsetDateTime.of(ldt, offset))
        merger.removeProcessed(LocalDateTime.rule)
        merger.removeProcessed(ZoneOffset.rule)
      }
      else {
        var od: OffsetDate = merger.getValue(OffsetDate.rule)
        var ot: OffsetTime = merger.getValue(OffsetTime.rule)
        if (od != null && ot != null) {
          if (od.getOffset.equals(ot.getOffset) == false) {
            if (merger.getContext.isStrict) {
              throw new CalendricalRuleException("Unable to merge OffsetDate and OffsetTime as offsets differ", OffsetTime.rule)
            }
            else {
              ot = ot.adjustLocalTime(od.getOffset)
            }
          }
          merger.storeMerged(OffsetDateTime.rule, OffsetDateTime.of(od, ot, od.getOffset))
          merger.removeProcessed(OffsetDate.rule)
          merger.removeProcessed(OffsetTime.rule)
        }
      }
      var odt: OffsetDateTime = merger.getValue(OffsetDateTime.rule)
      if (odt != null && zone != null) {
        if (merger.getContext.isStrict) {
          merger.storeMerged(ZonedDateTime.rule, ZonedDateTime.of(odt, zone))
        }
        else {
          merger.storeMerged(ZonedDateTime.rule, ZonedDateTime.ofInstant(odt, zone))
        }
        merger.removeProcessed(OffsetDateTime.rule)
        merger.removeProcessed(TimeZone.rule)
      }
    }

    /**
     * Gets the name of the chronology.
     *
     * @return the name of the chronology, never null
     */
    override def getName: String = "ISO"
  }