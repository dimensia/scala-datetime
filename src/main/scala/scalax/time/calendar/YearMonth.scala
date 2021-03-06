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

import scalax.time.MathUtils
import scalax.time.calendar.format.DateTimeFormatter
import scalax.time.calendar.format.DateTimeFormatterBuilder
import scalax.time.calendar.format.DateTimeFormatterBuilder.SignStyle

/**
 * A year-month in the ISO-8601 calendar system, such as '2007-12'.
 * <p>
 * {@code YearMonth} is an immutable calendrical that represents the combination
 * of a year and month. Any field that can be derived from a year and month, such as
 * quarter-of-year, can be obtained.
 * <p>
 * This class does not store or represent a day, time or time-zone.
 * Thus, for example, the value "October 2007" can be stored in a {@code YearMonth}.
 * <p>
 * The ISO-8601 calendar system is the modern civil calendar system used today
 * in most of the world. It is equivalent to the proleptic Gregorian calendar
 * system, in which todays's rules for leap years are applied for all time.
 * For most applications written today, the ISO-8601 rules are entirely suitable.
 * Any application that uses historical dates should consider using {@code HistoricDate}.
 * <p>
 * YearMonth is immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object YearMonth {
  /**
   * Parser.
   */
  private val Parser: DateTimeFormatter =
    (new DateTimeFormatterBuilder)
      .appendValue(ISOChronology.yearRule, 4, 10, SignStyle.ExceedsPad)
      .appendLiteral('-')
      .appendValue(ISOChronology.monthOfYearRule, 2)
      .toFormatter

  /**
   * Obtains an instance of {@code YearMonth} from a Calendrical.
   * <p>
   * This method will create a year-month from the Calendrical by extracting
   * the year and month-of-year fields.
   *
   * @param calendrical the calendrical to use, not null
   * @return the year-month, never null
   * @throws UnsupportedRuleException if either field cannot be found
   * @throws InvalidCalendarFieldException if the value for either field is invalid
   */
  def apply(calendrical: Calendrical): YearMonth = {
    val year: Int = ISOChronology.yearRule.getValueChecked(calendrical)
    val month: MonthOfYear = ISOChronology.monthOfYearRule.getValueChecked(calendrical)
    YearMonth(year, month)
  }

  /**
   * Rule implementation.
   */
  private[calendar] object Rule extends Rule

  @SerialVersionUID(1L)
  private[calendar] sealed class Rule
    extends CalendricalRule[YearMonth](classOf[YearMonth], ISOChronology, "YearMonth", ISOChronology.periodMonths, null)
    with Serializable {
    override def derive(calendrical: Calendrical): Option[YearMonth] = {
      val year: Int = calendrical.get(ISOChronology.yearRule).getOrElse(return None)
      val moy: MonthOfYear = calendrical.get(ISOChronology.monthOfYearRule).getOrElse(return None)
      return Some(YearMonth(year, moy))
    }

    private def readResolve: AnyRef = Rule
  }

  /**
   * Obtains the current year-month from the specified clock.
   * <p>
   * This will query the specified clock to obtain the current year-month.
   * Using this method allows the use of an alternate clock for testing.
   * The alternate clock may be introduced using  {@link Clock dependency injection}.
   *
   * @param clock the clock to use, by default {@code Clock.systemDefaultZone}, not null
   * @return the current year-month, never null
   */
  def now(implicit clock: Clock = Clock.systemDefaultZone): YearMonth = {
    val date: Date = Date.now(clock)
    YearMonth(date.getYear, date.getMonthOfYear)
  }

  /**
   * Obtains an instance of  {@code YearMonth} from a year and month.
   *
   * @param year the year to represent, from MIN_YEAR to MAX_YEAR
   * @param monthOfYear the month-of-year to represent, from 1 (January) to 12 (December)
   * @return the year-month, never null
   * @throws IllegalCalendarFieldValueException if either field value is invalid
   */
  def apply(year: Int, monthOfYear: Int): YearMonth = YearMonth(year, MonthOfYear.of(monthOfYear))

  /**
   * Gets the field rule for the year-month.
   *
   * @return the field rule for the date-time, never null
   */
  def rule: CalendricalRule[YearMonth] = Rule

  /**
   * Obtains an instance of  {@code YearMonth} from a text string.
   * <p>
   * The following formats are accepted in ASCII:
   * <ul>
   * <li>  {year}-{monthOfYear}
   * </ul>
   * The year has between 4 and 10 digits with values from MIN_YEAR to MAX_YEAR.
   * If there are more than 4 digits then the year must be prefixed with the plus symbol.
   * Negative years are allowed, but not negative zero.
   * <p>
   * The month-of-year has 2 digits and has values from 1 to 12.
   *
   * @param text the text to parse such as '2007-12', not null
   * @return the parsed year-month, never null
   * @throws CalendricalException if the text cannot be parsed
   */
  def parse(text: String): YearMonth = Parser.parse(text, rule)

  /**
   * Obtains an instance of  {@code YearMonth} from a text string using a specific formatter.
   * <p>
   * The text is parsed using the formatter, returning a year-month.
   *
   * @param text the text to parse, not null
   * @param formatter the formatter to use, not null
   * @return the parsed year-month, never null
   * @throws UnsupportedOperationException if the formatter cannot parse
   * @throws CalendricalException if the text cannot be parsed
   */
  def parse(text: String, formatter: DateTimeFormatter = Parser): YearMonth = {
    ISOChronology.checkNotNull(formatter, "DateTimeFormatter must not be null")
    formatter.parse(text, rule)
  }
}


/**
 * Constructor.
 *
 * Obtains an instance of  {@code YearMonth} from a year and month.
 *
 * @param year the year to represent, from MIN_YEAR to MAX_YEAR
 * @param monthOfYear the month-of-year to represent, not null
 * @return the year-month, never null
 * @throws IllegalCalendarFieldValueException if the year value is invalid
 */
@SerialVersionUID(1L)
final case class YearMonth(year: Int, month: MonthOfYear) extends Calendrical with CalendricalMatcher with DateAdjuster with Ordered[YearMonth] {
  ISOChronology.yearRule.checkValue(year)
  ISOChronology.checkNotNull(month, "MonthOfYear must not be null")

  /**
   * Adjusts a date to have the value of this year-month, using a resolver to
   * handle the case when the day-of-month becomes invalid.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param date the date to be adjusted, not null
   * @param resolver the date resolver to use if the day-of-month becomes invalid, not null
   * @return the adjusted date, never null
   * @throws IllegalCalendarFieldValueException if the date cannot be resolved using the resolver
   */
  def adjustDate(date: Date, resolver: DateResolver): Date = {
    ISOChronology.checkNotNull(date, "Date must not be null")
    ISOChronology.checkNotNull(resolver, "DateResolver must not be null")
    if (date.getYear == year && date.getMonthOfYear == month) {
      return date
    }
    val resolved: Date = resolver.resolveDate(year, month, date.getDayOfMonth)
    ISOChronology.checkNotNull(resolved, "The implementation of DateResolver must not return null")
    return resolved
  }

  /**
   * Returns a copy of this  {@code YearMonth} with the specified period subtracted.
   * <p>
   * This subtracts the specified period from this year-month, returning a new year-month.
   * Before subtraction, the period is converted to a  {@code Period} using
   * {@link Period#of ( PeriodProvider )}.
   * The calculation only uses the years and months fields.
   * Other fields are ignored.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to subtract, not null
   * @return a {@code YearMonth} based on this year-month with the period subtracted, never null
   * @throws CalendricalException if the specified period cannot be converted to a  {@code Period }
   * @throws ArithmeticException if the result exceeds the supported range
   */
  def minus(periodProvider: PeriodProvider): YearMonth = {
    val period: Period = Period.of(periodProvider)
    minusMonths(period.totalMonths)
  }

  /**
   * Returns a copy of this YearMonth with the specified period in years subtracted.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to subtract, positive or negative
   * @return a {@code YearMonth} based on this year-month with the years subtracted, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def minusYears(years: Long): YearMonth = {
    if (years == 0) return this
    val newYear: Int = ISOChronology.yearRule.checkValue(year - years)
    return copy(newYear, month)
  }

  /**
   * Rolls the month-of-year, adding the specified number of months to a copy
   * of this  {@code YearMonth}.
   * <p>
   * This method will add the specified number of months to the month-day,
   * rolling from December back to January if necessary.
   * The year is not altered.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to roll by, positive or negative
   * @return a {@code YearMonth} based on this year-month with the month rolled, never null
   */
  def rollMonthOfYear(months: Int): YearMonth = copy(month.roll(months).ordinal)

  /**
   * Gets the value of the specified calendrical rule.
   * <p>
   * This method queries the value of the specified calendrical rule.
   * If the value cannot be returned for the rule from this year-month then
   * {@code null} will be returned.
   *
   * @param rule the rule to use, not null
   * @return the value for the rule, null if the value cannot be returned
   */
  def get[T](rule: CalendricalRule[T]): Option[T] = {
    ISOChronology.checkNotNull(rule, "CalendricalRule must not be null")
    if (rule.equals(ISOChronology.yearRule)) return rule.reify(year)
    if (rule.equals(ISOChronology.monthOfYearRule)) return rule.reify(month)
    //    return Some(rule.deriveValueFor(rule, this, this, ISOChronology))     //FIXME
    return None
  }

  /**
   * Gets the year field as a  {@code Year}.
   * <p>
   * This method provides access to an object representing the year field.
   * {@code Year} has methods for querying addition year-based information.
   *
   * @return the year, never null
   */
  def toYear: Year = Year(year)

  /**
   * Checks if the year-month extracted from the calendrical matches this.
   *
   * @param calendrical the calendrical to match, not null
   * @return true if the calendrical matches, false otherwise
   */
  override def matchesCalendrical(calendrical: Calendrical): Boolean = this.equals(calendrical.get(YearMonth.rule))

  /**
   * Outputs this year-month as a  {@code String} using the formatter.
   *
   * @param formatter the formatter to use, not null
   * @return the formatted year-month string, never null
   * @throws UnsupportedOperationException if the formatter cannot print
   * @throws CalendricalPrintException if an error occurs during printing
   */
  def toString(formatter: DateTimeFormatter): String = {
    ISOChronology.checkNotNull(formatter, "DateTimeFormatter must not be null")
    formatter.print(this)
  }

  /**
   * A hash code for this year-month.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = year ^ (month.ordinal << 27)

  /**
   * Outputs this year-month as a  {@code String}, such as  {@code 2007 -12}.
   * <p>
   * The output will be in the format  {@code yyyy -MM} :
   *
   * @return the formatted year-month, never null
   */
  override def toString: String = {
    val yearValue: Int = year
    val monthValue: Int = month.ordinal
    val absYear: Int = math.abs(yearValue)
    val buf: StringBuilder = new StringBuilder(9)
    if (absYear < 1000) {
      if (yearValue < 0) buf.append(yearValue - 10000).deleteCharAt(1)
      else buf.append(yearValue + 10000).deleteCharAt(0)
    }
    else {
      buf.append(yearValue)
    }
    return buf.append(if (monthValue < 10) "-0" else "-").append(monthValue).toString
  }

  /**
   * Returns a copy of this  {@code YearMonth} with the specified period added.
   * <p>
   * This adds the specified period to this year-month, returning a new year-month.
   * Before addition, the period is converted to a  {@code Period} using
   * {@link Period#of ( PeriodProvider )}.
   * The calculation only uses the years and months fields.
   * Other fields are ignored.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to add, not null
   * @return a {@code YearMonth} based on this year-month with the period added, never null
   * @throws CalendricalException if the specified period cannot be converted to a  {@code Period }
   * @throws ArithmeticException if the result exceeds the supported range
   */
  def plus(periodProvider: PeriodProvider): YearMonth = {
    val period: Period = Period.of(periodProvider)
    plusMonths(period.totalMonths)
  }

  def +(periodProvider: PeriodProvider): YearMonth = plus(periodProvider)

  /**
   * Returns a date formed from this year-month at the specified day-of-month.
   * <p>
   * This method merges  {@code this} and the specified day to form an
   * instance of  {@code Date}.
   * This method can be used as part of a chain to produce a date:
   * <pre>
   * Date date = year.atMonth(month).atDay(day);
   * </pre>
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param dayOfMonth the day-of-month to use, from 1 to 31
   * @return the date formed from this year-month and the specified day, never null
   * @throws InvalidCalendarFieldException when the day is invalid for the year-month
   * @see #isValidDay(int)
   */
  def atDay(dayOfMonth: Int): Date = Date(year, month, dayOfMonth)

  /**
   * Is this year-month after the specified year-month.
   *
   * @param other the other year-month to compare to, not null
   * @return true if this is after the specified year-month
   * @throws NullPointerException if  {@code other} is null
   */
  def isAfter(other: YearMonth): Boolean = this > other

  /**
   * Compares this year-month to another year-month.
   *
   * @param other the other year-month to compare to, not null
   * @return the comparator value, negative if less, positive if greater
   * @throws NullPointerException if  {@code other} is null
   */
  def compare(other: YearMonth): Int = {
    var cmp: Int = MathUtils.safeCompare(year, other.year)
    if (cmp == 0) cmp = month.compareTo(other.month)
    cmp
  }

  /**
   * Returns a copy of this YearMonth with the specified period in years added.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param years the years to add, positive or negative
   * @return a {@code YearMonth} based on this year-month with the years added, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def plusYears(years: Long): YearMonth = {
    if (years == 0) return this
    val newYear: Int = ISOChronology.yearRule.checkValue(year + years)
    return copy(newYear, month)
  }

  /**
   * Returns a copy of this YearMonth with the specified period in months added.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to add, positive or negative
   * @return a {@code YearMonth} based on this year-month with the months added, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def plusMonths(months: Long): YearMonth = {
    if (months == 0) {
      return this
    }
    val monthCount: Long = year * 12L + (month.ordinal - 1)
    val calcMonths: Long = monthCount + months
    val newYear: Int = ISOChronology.yearRule.checkValue(MathUtils.floorDiv(calcMonths, 12))
    val newMonth: MonthOfYear = MonthOfYear.of(MathUtils.floorMod(calcMonths, 12) + 1)
    return copy(newYear, newMonth)
  }

  /**
   * Adjusts a date to have the value of this year-month, returning a new date.
   * <p>
   * This method implements the  {@link DateAdjuster} interface.
   * It is intended that, instead of calling this method directly, it is used from
   * an instance of  {@code Date} :
   * <pre>
   *   date = date.with(yearMonth);
   * </pre>
   * <p>
   * This implementation handles the case where the day-of-month is invalid for the new
   * month and year using the  {@link DateResolvers#previousValid()} resolver.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param date the date to be adjusted, not null
   * @return the adjusted date, never null
   */
  override def adjustDate(date: Date): Date = adjustDate(date, DateResolvers.previousValid)

  /**
   * Gets the length of this month in days.
   * <p>
   * This returns the length in days of the month.
   * The year is used to determine the correct length of February.
   *
   * @return the length of the month in days, from 28 to 31
   */
  def lengthInDays: Int = month.lengthInDays(ISOChronology.isLeapYear(year))

  /**
   * Gets the chronology that this year-month uses, which is the ISO calendar system.
   *
   * @return the ISO chronology, never null
   */
  def getChronology: ISOChronology = ISOChronology

  /**
   * Checks if the day-of-month is valid for this year-month.
   * <p>
   * This method checks whether this year and month and the input day form
   * a valid date.
   *
   * @param dayOfMonth the day-of-month to validate, from 1 to 31, invalid value returns false
   * @return true if the day is valid for this year-month
   */
  def isValidDay(dayOfMonth: Int): Boolean = dayOfMonth >= 1 && dayOfMonth <= lengthInDays

  /**
   * Is this year-month before the specified year-month.
   *
   * @param other the other year-month to compare to, not null
   * @return true if this point is before the specified year-month
   * @throws NullPointerException if  {@code other} is null
   */
  def isBefore(other: YearMonth): Boolean = this < other

  /**
   * Returns a copy of this YearMonth with the specified period in months subtracted.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to subtract, positive or negative
   * @return a {@code YearMonth} based on this year-month with the months subtracted, never null
   * @throws CalendricalException if the result exceeds the supported range
   */
  def minusMonths(months: Long): YearMonth = {
    if (months == 0) {
      return this
    }
    val monthCount: Long = year * 12L + (month.ordinal - 1)
    val calcMonths: Long = monthCount - months
    val newYear: Int = ISOChronology.yearRule.checkValue(MathUtils.floorDiv(calcMonths, 12))
    val newMonth: MonthOfYear = MonthOfYear.of(MathUtils.floorMod(calcMonths, 12) + 1)
    return copy(newYear, newMonth)
  }
}