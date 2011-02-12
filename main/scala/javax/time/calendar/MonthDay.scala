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

import javax.time.MathUtils
import javax.time.calendar.format.DateTimeFormatter
import javax.time.calendar.format.DateTimeFormatterBuilder

/**
 * A month-day in the ISO-8601 calendar system, such as '--12-03'.
 * <p>
 * {@code MonthDay} is an immutable calendrical that represents the combination
 * of a year and month. Any field that can be derived from a month and day, such as
 * quarter-of-year, can be obtained.
 * <p>
 * This class does not store or represent a year, time or time-zone.
 * Thus, for example, the value "3rd December" can be stored in a {@code MonthDay}.
 * <p>
 * Since a {@code MonthDay} does not possess a year, the leap day of
 * 29th of February is considered valid.
 * <p>
 * The ISO-8601 calendar system is the modern civil calendar system used today
 * in most of the world. It is equivalent to the proleptic Gregorian calendar
 * system, in which todays's rules for leap years are applied for all time.
 * For most applications written today, the ISO-8601 rules are entirely suitable.
 * Any application that uses historical dates should consider using {@code HistoricDate}.
 * <p>
 * MonthDay is immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object MonthDay {

  /**
   * Parser.
   */
  private val Parser: DateTimeFormatter =
    (new DateTimeFormatterBuilder)
      .appendLiteral("--")
      .appendValue(ISOChronology.monthOfYearRule, 2)
      .appendLiteral('-')
      .appendValue(ISOChronology.dayOfMonthRule, 2)
      .toFormatter

  /**
   * Gets the rule for the month-day.
   *
   * @return the rule for the month-day, never null
   */
  def rule: CalendricalRule[MonthDay] = Rule

  /**
   * Obtains an instance of {@code MonthDay} from a Calendrical.
   * <p>
   * This method will create a MonthDay from the Calendrical by extracting the
   * month-of-year and day-of-month fields.
   *
   * @param calendrical the calendrical to use, not null
   * @return the month-day, never null
   * @throws UnsupportedRuleException if either field cannot be found
   * @throws InvalidCalendarFieldException if the value for either field is invalid
   */
  def apply(calendrical: Calendrical): MonthDay = {
    val month: MonthOfYear = ISOChronology.monthOfYearRule.getValueChecked(calendrical)
    val dom: Int = ISOChronology.dayOfMonthRule.getValueChecked(calendrical)
    MonthDay(month, dom)
  }

  /**
   * Obtains an instance of {@code MonthDay} from a text string.
   * <p>
   * The following formats are accepted in ASCII:
   * <ul>
   * <li>--{monthOfYear}-{dayOfMonth}
   * </ul>
   * The month-of-year has 2 digits and has values from 1 to 12.
   * <p>
   * The day-of-month has 2 digits with values from 1 to 31 appropriate to the month.
   *
   * @param text the text to parse such as '--12-03', not null
   * @return the parsed month-day, never null
   * @throws CalendricalException if the text cannot be parsed
   */
  def parse(text: String): MonthDay = Parser.parse(text, rule)

  /**
   * Obtains the current month-day from the specified clock.
   * <p>
   * This will query the specified clock to obtain the current month-day.
   * Using this method allows the use of an alternate clock for testing.
   * The alternate clock may be introduced using {@link Clock dependency injection}.
   *
   * @param clock the clock to use, by default {@code Clock.systemDefaultZone}, not null
   * @return the current month-day, never null
   */
  def now(implicit clock: Clock = Clock.systemDefaultZone): MonthDay = {
    val now: LocalDate = LocalDate.now(clock)
    MonthDay(now.getMonthOfYear, now.getDayOfMonth)
  }

  /**
   * Obtains an instance of {@code MonthDay} from a text string using a specific formatter.
   * <p>
   * The text is parsed using the formatter, returning a month-day.
   *
   * @param text the text to parse, not null
   * @param formatter the formatter to use, not null
   * @return the parsed month-day, never null
   * @throws UnsupportedOperationException if the formatter cannot parse
   * @throws CalendricalException if the text cannot be parsed
   */
  def parse(text: String, formatter: DateTimeFormatter): MonthDay = {
    ISOChronology.checkNotNull(formatter, "DateTimeFormatter must not be null")
    formatter.parse(text, rule)
  }

  /**
   * Rule implementation.
   */
  private[calendar] object Rule extends Rule

  @SerialVersionUID(1L)
  private[calendar] sealed class Rule
    extends CalendricalRule[MonthDay](classOf[MonthDay], ISOChronology, "MonthDay", ISOChronology.periodDays, ISOChronology.periodYears)
    with Serializable {

    private def readResolve: AnyRef = Rule

    override def derive(calendrical: Calendrical): Option[MonthDay] = {
      val moy: MonthOfYear = calendrical.get(ISOChronology.monthOfYearRule).getOrElse(return None)
      val dom: Int = calendrical.get(ISOChronology.dayOfMonthRule).getOrElse(return None)
      return Some(MonthDay(moy, dom))
    }
  }

  /**
   * Obtains an instance of {@code MonthDay}.
   * <p>
   * The day-of-month must be valid for the month within a leap year.
   * Hence, for month 2 (February), day 29 is valid.
   * <p>
   * For example, passing in month 4 (April) and day 31 will throw an exception, as
   * there can never be a 31st April in any year. Alternately, passing in
   * 29th February is valid, as that month-day can be valid.
   *
   * @param monthOfYear the month-of-year to represent, from 1 (January) to 12 (December)
   * @param dayOfMonth the day-of-month to represent, from 1 to 31
   * @return the month-day, never null
   * @throws IllegalCalendarFieldValueException if the value of any field is out of range
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month
   */
  def apply(monthOfYear: Int, dayOfMonth: Int): MonthDay = MonthDay(MonthOfYear(monthOfYear), dayOfMonth)
}

/**
 * Constructor.
 *
 * <p>
 * The day-of-month must be valid for the month within a leap year.
 * Hence, for February, day 29 is valid.
 * <p>
 * For example, passing in April and day 31 will throw an exception, as
 * there can never be a 31st April in any year. Alternately, passing in
 * 29th February is valid, as that month-day can be valid.
 *
 * @param monthOfYear the month-of-year to represent, not null
 * @param dayOfMonth the day-of-month to represent, from 1 to 31
 * @return the month-day, never null
 * @throws IllegalCalendarFieldValueException if the value of any field is out of range
 * @throws InvalidCalendarFieldException if the day-of-month is invalid for the month
 */
@SerialVersionUID(-254395108L)
final case class MonthDay(month: MonthOfYear, day: Int) extends Calendrical with CalendricalMatcher with DateAdjuster with Ordered[MonthDay] {
  ISOChronology.checkNotNull(month, "MonthOfYear must not be null")
  ISOChronology.dayOfMonthRule.checkValue(day)
  if (day > month.maxLengthInDays) {
    throw new InvalidCalendarFieldException("Illegal value for DayOfMonth field, value " + day + " is not valid for month " + month.name, ISOChronology.dayOfMonthRule)
  }

  import MonthDay._

  /**
   * Checks if the month-day extracted from the calendrical matches this.
   * <p>
   * This method implements the {@code CalendricalMatcher} interface.
   * It is intended that applications use {@link LocalDate#matches} rather than this method.
   *
   * @param calendrical the calendrical to match, not null
   * @return true if the calendrical matches, false otherwise
   */
  override def matchesCalendrical(calendrical: Calendrical): Boolean = this.equals(calendrical.get(rule))

  /**
   * Rolls the month-of-year, adding the specified number of months to a copy
   * of this {@code MonthDay}.
   * <p>
   * This method will add the specified number of months to the month-day,
   * rolling from December back to January if necessary.
   * <p>
   * If the day-of-month is invalid for the specified month in the result,
   * the day will be adjusted to the last valid day-of-month.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to roll by, positive or negative
   * @return a {@code MonthDay} based on this month-day with the month rolled, never null
   */
  def rollMonthOfYear(months: Int): MonthDay = copy(month.roll(months))

  /**
   * Is this month-day before the specified month-day.
   *
   * @param other the other month-day to compare to, not null
   * @return true if this point is before the specified month-day
   * @throws NullPointerException if {@code other} is null
   */
  def isBefore(other: MonthDay): Boolean = this < other

  /**
   * Rolls the day-of-month, adding the specified number of days to a copy
   * of this {@code MonthDay}.
   * <p>
   * This method will add the specified number of days to the month-day,
   * rolling from last day-of-month to the first if necessary.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param days the days to roll by, positive or negative
   * @return a {@code MonthDay} based on this month-day with the day rolled, never null
   */
  def rollDayOfMonth(days: Int): MonthDay = {
    if (days == 0) return this
    val monthLength: Int = month.maxLengthInDays
    var newDOM0: Int = (days % monthLength) + (day - 1)
    newDOM0 = (newDOM0 + monthLength) % monthLength
    return copy(day = ({
      newDOM0 += 1;
      newDOM0 - 1
    }))
  }

  /**
   * Adjusts a date to have the value of this month-day, using a resolver to
   * handle the case when the day-of-month becomes invalid.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param date the date to be adjusted, not null
   * @param resolver the date resolver to use if the day-of-month is invalid, not null
   * @return the adjusted date, never null
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the year
   */
  def adjustDate(date: LocalDate, resolver: DateResolver): LocalDate = {
    ISOChronology.checkNotNull(date, "LocalDate must not be null")
    ISOChronology.checkNotNull(resolver, "DateResolver must not be null")
    if (date.getMonthOfYear == month && date.getDayOfMonth == day) {
      return date
    }
    val resolved: LocalDate = resolver.resolveDate(date.getYear, month, day)
    ISOChronology.checkNotNull(resolved, "The implementation of DateResolver must not return null")
    return resolved
  }

  /**
   * Compares this month-day to another month-day.
   *
   * @param other the other month-day to compare to, not null
   * @return the comparator value, negative if less, positive if greater
   * @throws NullPointerException if {@code other} is null
   */
  def compare(other: MonthDay): Int = {
    var cmp: Int = month.compareTo(other.month)
    if (cmp == 0) cmp = MathUtils.safeCompare(day, other.day)
    cmp
  }

  /**
   * Gets the value of the specified calendrical rule.
   * <p>
   * This method queries the value of the specified calendrical rule.
   * If the value cannot be returned for the rule from this month-day then
   * {@code null} will be returned.
   *
   * @param rule the rule to use, not null
   * @return the value for the rule, null if the value cannot be returned
   */
  def get[T](rule: CalendricalRule[T]): Option[T] = {
    ISOChronology.checkNotNull(rule, "CalendricalRule must not be null")
    if (rule.equals(ISOChronology.monthOfYearRule)) return rule.reify(month)
    if (rule.equals(ISOChronology.dayOfMonthRule)) return rule.reify(day)
    //    return Some(rule.deriveValueFor(rule, this, this, ISOChronology))    //FIXME
    None
  }

  /**
   * Outputs this month-day as a {@code String} using the formatter.
   *
   * @param formatter the formatter to use, not null
   * @return the formatted month-day string, never null
   * @throws UnsupportedOperationException if the formatter cannot print
   * @throws CalendricalPrintException if an error occurs during printing
   */
  def toString(formatter: DateTimeFormatter): String = {
    ISOChronology.checkNotNull(formatter, "DateTimeFormatter must not be null")
    formatter.print(this)
  }

  /**
   * Is this month-day after the specified month-day.
   *
   * @param other the other month-day to compare to, not null
   * @return true if this is after the specified month-day
   * @throws NullPointerException if {@code other} is null
   */
  def isAfter(other: MonthDay): Boolean = this > other

  /**
   * A hash code for this month-day.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = (month.ordinal << 6) + day

  /**
   * Checks if the year is valid for this month-day.
   * <p>
   * This method checks whether this month and day and the input year form
   * a valid date.
   *
   * @param year the year to validate, an out of range value returns false
   * @return true if the year is valid for this month-day
   * @see Year#isValidMonthDay ( MonthDay )
   */
  def isValidYear(year: Int): Boolean = (day == 29 && month.isFebruary && ISOChronology.isLeapYear(year) == false) == false

  /**
   * Adjusts a date to have the value of this month-day, returning a new date.
   * <p>
   * This method implements the {@link DateAdjuster} interface.
   * It is intended that, instead of calling this method directly, it is used from
   * an instance of {@code LocalDate} :
   * <pre>
   *   date = date.with(monthDay);
   * </pre>
   * <p>
   * This implementation handles the case where this represents February 29 and
   * the year is not a leap year by throwing an exception.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param date the date to be adjusted, not null
   * @return the adjusted date, never null
   * @throws InvalidCalendarFieldException if the day-of-month is invalid for the year
   */
  override def adjustDate(date: LocalDate): LocalDate = adjustDate(date, DateResolvers.strict)

  /**
   * Returns a date formed from this month-day at the specified year.
   * <p>
   * This method merges {@code this} and the specified year to form an
   * instance of {@code LocalDate}.
   * <pre>
   * LocalDate date = monthDay.atYear(year);
   * </pre>
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param year the year to use, from MIN_YEAR to MAX_YEAR
   * @return the local date formed from this month-day and the specified year, never null
   * @see Year#atMonthDay(MonthDay)
   */
  def atYear(year: Int): LocalDate = LocalDate(year, month, day)

  /**
   * Outputs this month-day as a {@code String}, such as {@code --12-03}.
   * <p>
   * The output will be in the format {@code --MM-dd} :
   *
   * @return the formatted month-day, never null
   */
  override def toString: String = {
    val monthValue: Int = month.ordinal
    val dayValue: Int = day
    new StringBuilder(10)
      .append("--")
      .append(if (monthValue < 10) "0" else "")
      .append(monthValue)
      .append(if (dayValue < 10) "-0" else "-")
      .append(dayValue)
      .toString
  }

  /**
   * Gets the chronology that this month-day uses, which is the ISO calendar system.
   *
   * @return the ISO chronology, never null
   */
  def getChronology: ISOChronology = ISOChronology
}