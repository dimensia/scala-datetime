/*
 * Copyright (c) 2007-2010 Stephen Colebourne & Michael Nascimento Santos
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

import java.util.Locale
import scalax.time.calendar.format.DateTimeFormatterBuilder.TextStyle

/**
 * A month-of-year, such as 'July'.
 * <p>
 * {@code MonthOfYear} is an enum representing the 12 months of the year -
 * January, February, March, April, May, June, July, August, September, October,
 * November and December.
 * <p>
 * In addition to the textual enum name, each month-of-year has an {@code int} value.
 * The {@code int} value follows normal usage and the ISO-8601 standard,
 * from 1 (January) to 12 (December). It is recommended that applications use the enum
 * rather than the {@code int} value to ensure code clarity.
 * <p>
 * <b>Do not use {@code ordinal()} to obtain the numeric representation of {@code MonthOfYear}.
 * Use {@code getValue()} instead.</b>
 * <p>
 * This enum represents a common concept that is found in many calendar systems.
 * As such, this enum may be used by any calendar system that has the month-of-year
 * concept with a twelve month year where the names and month lengths are equivalent to
 * those defined. Note that the implementation of {@link DateTimeFieldRule} for month-of-year
 * may vary by calendar system.
 * <p>
 * MonthOfYear is an immutable and thread-safe enum.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object MonthOfYear {

  /**
   * The singleton instance for the month of January with 31 days.
   * This has the numeric value of {@code 1}.
   */
  object January extends MonthOfYear(1)

  /**
   * The singleton instance for the month of February with 28 days, or 29 in a leap year.
   * This has the numeric value of {@code 2}.
   */
  object February extends MonthOfYear(2)

  /**
   * The singleton instance for the month of March with 31 days.
   * This has the numeric value of {@code 3}.
   */
  object March extends MonthOfYear(3)

  /**
   * The singleton instance for the month of April with 30 days.
   * This has the numeric value of {@code 4}.
   */
  object April extends MonthOfYear(4)

  /**
   * The singleton instance for the month of May with 31 days.
   * This has the numeric value of {@code 5}.
   */
  object May extends MonthOfYear(5)

  /**
   * The singleton instance for the month of June with 30 days.
   * This has the numeric value of {@code 6}.
   */
  object June extends MonthOfYear(6)

  /**
   * The singleton instance for the month of July with 31 days.
   * This has the numeric value of {@code 7}.
   */
  object July extends MonthOfYear(7)

  /**
   * The singleton instance for the month of August with 31 days.
   * This has the numeric value of {@code 8}.
   */
  object August extends MonthOfYear(8)

  /**
   * The singleton instance for the month of September with 30 days.
   * This has the numeric value of {@code 9}.
   */
  object September extends MonthOfYear(9)

  /**
   * The singleton instance for the month of October with 31 days.
   * This has the numeric value of {@code 10}.
   */
  object October extends MonthOfYear(10)

  /**
   * The singleton instance for the month of November with 30 days.
   * This has the numeric value of {@code 11}.
   */
  object November extends MonthOfYear(11)

  /**
   * The singleton instance for the month of December with 31 days.
   * This has the numeric value of {@code 12}.
   */
  object December extends MonthOfYear(12)

  lazy val values = Array(January, February, March, April, May, June, July, August, September, October, November, December)

  /**
   * Obtains an instance of {@code MonthOfYear} from an {@code int} value.
   * <p>
   * {@code MonthOfYear} is an enum representing the 12 months of the year.
   * This factory allows the enum to be obtained from the {@code int} value.
   * The {@code int} value follows the ISO-8601 standard, from 1 (January) to 12 (December).
   * <p>
   * An exception is thrown if the value is invalid. The exception uses the
   * {@link ISOChronology} month-of-year rule to indicate the failed rule.
   *
   * @param monthOfYear the month-of-year to represent, from 1 (January) to 12 (December)
   * @return the MonthOfYear singleton, never null
   * @throws IllegalCalendarFieldValueException if the month-of-year is invalid
   */
  def of(monthOfYear: Int): MonthOfYear = {
    monthOfYear match {
      case 0 => December //Is that a good solution or should we better fix roll to return 12 instead of 0?
      case 1 => January
      case 2 => February
      case 3 => March
      case 4 => April
      case 5 => May
      case 6 => June
      case 7 => July
      case 8 => August
      case 9 => September
      case 10 => October
      case 11 => November
      case 12 => December
      case _ => throw new IllegalCalendarFieldValueException(ISOChronology.monthOfYearRule, monthOfYear, 1, 12)
    }
  }

  def apply(monthOfYear: Int) = of(monthOfYear)

}

sealed abstract class MonthOfYear(val ordinal: Int) extends Ordered[MonthOfYear] with Calendrical {

  import MonthOfYear._

  override def toString = this.getClass.getSimpleName.split('$')(1)

  /**
   * Is this instance representing January.
   *
   * @return true if this instance represents January
   */
  def isJanuary: Boolean = (this == January)

  /**
   * Is this instance representing February.
   *
   * @return true if this instance represents February
   */
  def isFebruary: Boolean = (this == February)

  /**
   * Is this instance representing March.
   *
   * @return true if this instance represents March
   */
  def isMarch: Boolean = (this == March)

  /**
   * Is this instance representing April.
   *
   * @return true if this instance represents April
   */
  def isApril: Boolean = (this == April)

  /**
   * Is this instance representing May.
   *
   * @return true if this instance represents May
   */
  def isMay: Boolean = (this == May)

  /**
   * Is this instance representing June.
   *
   * @return true if this instance represents June
   */
  def isJune: Boolean = (this == June)

  /**
   * Is this instance representing July.
   *
   * @return true if this instance represents July
   */
  def isJuly: Boolean = (this == July)

  /**
   * Is this instance representing August.
   *
   * @return true if this instance represents August
   */
  def isAugust: Boolean = (this == August)

  /**
   * Is this instance representing September.
   *
   * @return true if this instance represents September
   */
  def isSeptember: Boolean = (this == September)

  /**
   * Is this instance representing October.
   *
   * @return true if this instance represents October
   */
  def isOctober: Boolean = (this == October)

  /**
   * Is this instance representing November.
   *
   * @return true if this instance represents November
   */
  def isNovember: Boolean = (this == November)

  /**
   * Is this instance representing December.
   *
   * @return true if this instance represents December
   */
  def isDecember: Boolean = (this == December)

  /**
   * Gets the month-of-year {@code int} value.
   * <p>
   * The values are numbered following the ISO-8601 standard,
   * from 1 (January) to 12 (December).
   *
   * @return the month-of-year, from 1 (January) to 12 (December)
   */
//  def getValue: Int = ordinal

  /**
   * Gets the next month-of-year.
   * <p>
   * This calculates based on the time-line, thus it rolls around the end of
   * the year. The next month after December is January.
   *
   * @return the next month-of-year, never null
   */
  def next: MonthOfYear = roll(1)

  /**
   * Gets the previous month-of-year.
   * <p>
   * This calculates based on the time-line, thus it rolls around the end of
   * the year. The previous month before January is December.
   *
   * @return the previous month-of-year, never null
   */
  def previous: MonthOfYear = roll(-1)

  /**
   * Rolls the month-of-year, adding the specified number of months.
   * <p>
   * This calculates based on the time-line, thus it rolls around the end of
   * the year from December to January. The months to roll by may be negative.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param months the months to roll by, positive or negative
   * @return the resulting month-of-year, never null
   */
  def roll(months: Int): MonthOfYear = of((ordinal + (months % 12 + 12)) % 12)

  /**
   * Gets the value of the specified calendrical rule.
   * <p>
ordinalordinal   * This returns the one of the month values if the type of the rule
   * is {@code MonthOfYear}. Other rules will return {@code null}.
   *
   * @param rule the rule to use, not null
   * @return the value for the rule, null if the value cannot be returned
   */
  def get[T](rule: CalendricalRule[T]): Option[T] = {
    if (rule.getReifiedType != classOf[MonthOfYear]) None
    else rule.reify(this)
  }

  /**
   * Gets the minimum length of this month in days.
   * <p>
   * February has a minimum length of 28 days.
   * April, June, September and November have 30 days.
   * All other months have 31 days.
   *
   * @return the minimum length of this month in days, from 28 to 31
   */
  def minLengthInDays: Int = {
    this match {
      case February => 28
      case April | June | September | November => 30
      case _ => 31
    }
  }

  /**
   * Gets the maximum length of this month in days.
   * <p>
   * February has a maximum length of 29 days.
   * April, June, September and November have 30 days.
   * All other months have 31 days.
   *
   * @return the maximum length of this month in days, from 29 to 31
   */
  def maxLengthInDays: Int = {
    this match {
      case February => 29
      case April | June | September | November => 30
      case _ => 31
    }
  }

  /**
   * Gets the length of this month in days.
   * <p>
   * This takes a flag to determine whether to return the length for a leap year or not.
   * <p>
   * February has 28 days in a standard year and 29 days in a leap year.
   * April, June, September and November have 30 days.
   * All other months have 31 days.
   *
   * @param leapYear true if the length is required for a leap year
   * @return the length of this month in days, from 28 to 31
   */
  def lengthInDays(leapYear: Boolean): Int = {
    this match {
      case February => if (leapYear) 29 else 28
      case April | June | September | November => 30
      case _ => 31
    }
  }

  /**
   * Gets the full textual representation of this month-of-year, such as 'January' or 'December'.
   * <p>
   * This method is notionally specific to {@link ISOChronology} as it uses
   * the month-of-year rule to obtain the text. However, it is expected that
   * the text will be equivalent for all month-of-year rules, thus this aspect
   * of the implementation should be irrelevant to applications.
   * <p>
   * If there is no textual mapping for the locale, then the value is
   * returned as per {@link Integer#toString()}.
   *
   * @param locale the locale to use, not null
   * @return the full text value of the month-of-year, never null
   */
  def text(locale: Locale): String = ISOChronology.monthOfYearRule.getText(ordinal, locale, TextStyle.Full)

  /**
   * Gets the quarter that this month falls in.
   * <p>
   * January to March are Q1, April to June are Q2, July to September are Q3
   * and October to December are Q4.
   *
   * @return the quarter-of-year, never null
   */
  def quarterOfYear: QuarterOfYear = {
    if (ordinal < 4) QuarterOfYear.Q1
    else if (ordinal < 7) QuarterOfYear.Q2
    else if (ordinal < 10) QuarterOfYear.Q3
    else QuarterOfYear.Q4
  }

  /**
   * Gets the index of the month within the quarter.
   * <p>
   * January, April, July and October will return 1.
   * February, May, August and November will return 2.
   * March, June, September and December will return 3.
   *
   * @return the month of season, from 1 to 3
   */
  def monthOfQuarter: Int = {
    val result = (ordinal % 3)
    if (result == 0) 3 else result
  }

  /**
   * Gets the short textual representation of this month-of-year, such as 'Jan' or 'Dec'.
   * <p>
   * This method is notionally specific to {@link ISOChronology} as it uses
   * the month-of-year rule to obtain the text. However, it is expected that
   * the text will be equivalent for all month-of-year rules, thus this aspect
   * of the implementation should be irrelevant to applications.
   * <p>
   * If there is no textual mapping for the locale, then the value is
   * returned as per {@link Integer#toString()}.
   *
   * @param locale the locale to use, not null
   * @return the short text value of the month-of-year, never null
   */
  def shortText(locale: Locale): String = ISOChronology.monthOfYearRule.getText(ordinal, locale, TextStyle.Short)

  /**
   * Gets the day-of-month for last day of this month.
   * <p>
   * This is a synonym for {@link #lengthInDays ( boolean )} and exists to provide
   * a more meaningful API.
   *
   * @param leapYear true if the length is required for a leap year
   * @return the last day of this month, from 28 to 31
   */
  def lastDayOfMonth(leapYear: Boolean): Int = lengthInDays(leapYear)

  val name = this.getClass.getName

  def compare(other: MonthOfYear): Int = this.ordinal - other.ordinal

  /**
   * Gets the day-of-year for the first day of this month.
   * <p>
   * This returns the day-of-year that this month begins on, using the leap
   * year flag to determine the length of February.
   *
   * @param leapYear  true if the length is required for a leap year
   * @return the last day of this month, from 1 to 335
   */
  def monthStartDayOfYear(leapYear: Boolean): Int = {
    val leap: Int = if (leapYear) 1 else 0
    this match {
      case January => 1
      case February => 32
      case March => 60 + leap
      case April => 91 + leap
      case May => 121 + leap
      case June => 152 + leap
      case July => 182 + leap
      case August => 213 + leap
      case September => 244 + leap
      case October => 274 + leap
      case November => 305 + leap
      case December => 335 + leap
    }
  }

  /**
   * Gets the day-of-year for the last day of this month.
   * <p>
   * This returns the day-of-year that this month ends on, using the leap
   * year flag to determine the length of February.
   *
   * @param leapYear  true if the length is required for a leap year
   * @return the last day of this month, from 31 to 366
   */
  def monthEndDayOfYear(leapYear: Boolean): Int = monthStartDayOfYear(leapYear) + lengthInDays(leapYear) - 1;
}