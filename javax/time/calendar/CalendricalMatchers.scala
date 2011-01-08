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

/**
 * Provides common implementations of {@code CalendricalMatcher}.
 * <p>
 * These matchers are useful and common implementations of {@link CalendricalMatcher}.
 * A matcher allows any type of matching to be performed against a calendrical.
 * Examples might be checking of the calendrical represents Friday the Thirteenth,
 * or the last day of the month, or one of the American continent time-zones.
 * <p>
 * CalendricalMatchers is a utility class.
 * All matchers returned are immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object CalendricalMatchers {
  /**
   * Returns the weekend day matcher, which returns true if the date
   * is Saturday or Sunday.
   * <p>
   * Some territories have weekends that do not consist of Saturday and Sunday.
   * No implementation is supplied to support this, however a DateMatcher
   * can be easily written to do so.
   *
   * @return the non weekend day matcher, never null
   */
  def weekendDay: CalendricalMatcher = Impl.WeekendDay

  /**
   * Returns the non weekend day matcher, which returns true if the date
   * is between Monday and Friday inclusive.
   * <p>
   * Some territories have weekends that do not consist of Saturday and Sunday.
   * No implementation is supplied to support this, however a DateMatcher
   * can be easily written to do so.
   *
   * @return the non weekend day matcher, never null
   */
  def nonWeekendDay: CalendricalMatcher = Impl.NonWeekendDay

  /**
   * Enum implementing the adjusters.
   */
  private object Impl {

    /**Leap year matcher. */
    object LeapYear extends CalendricalMatcher {
      /**{@inheritDoc}*/
      override def matchesCalendrical(calendrical: Calendrical): Boolean = {
        val yearVal: Int = calendrical.get(ISOChronology.yearRule).getOrElse(return false)
        return ISOChronology.isLeapYear(yearVal)
      }
    }

    /**Leap day matcher. */
    object LeapDay extends CalendricalMatcher {
      /**{@inheritDoc}*/
      override def matchesCalendrical(calendrical: Calendrical): Boolean = {
        var moy: MonthOfYear = calendrical.get(ISOChronology.monthOfYearRule).get
        var domVal: Int = calendrical.get(ISOChronology.dayOfMonthRule).getOrElse(return false)
        return domVal == 29 && moy == MonthOfYear.February
      }
    }

    /**Non weekend matcher. */
    object NonWeekendDay extends CalendricalMatcher {
      /**{@inheritDoc}*/
      override def matchesCalendrical(calendrical: Calendrical): Boolean = {
        val dow: DayOfWeek = calendrical.get(ISOChronology.dayOfWeekRule).getOrElse(return false)
        return dow != DayOfWeek.Saturday && dow != DayOfWeek.Sunday
      }
    }

    /**Non weekend matcher. */
    object WeekendDay extends CalendricalMatcher {
      /**{@inheritDoc}*/
      override def matchesCalendrical(calendrical: Calendrical): Boolean = {
        var dow: DayOfWeek = calendrical.get(ISOChronology.dayOfWeekRule).getOrElse(return false)
        return dow == DayOfWeek.Saturday || dow == DayOfWeek.Sunday
      }
    }

    /**Last day-of-year matcher. */
    object LastDayOfYear extends CalendricalMatcher {
      /**{@inheritDoc}*/
      override def matchesCalendrical(calendrical: Calendrical): Boolean = {
        var moy: MonthOfYear = calendrical.get(ISOChronology.monthOfYearRule).getOrElse(return false)
        var domVal: Int = calendrical.get(ISOChronology.dayOfMonthRule).getOrElse(return false)
        return domVal == 31 && moy == MonthOfYear.December
      }
    }

    /**Last day-of-month matcher. */
    object LastDayOfMonth extends CalendricalMatcher {
      /**{@inheritDoc}*/
      override def matchesCalendrical(calendrical: Calendrical): Boolean = {
        val yearVal: Int = calendrical.get(ISOChronology.yearRule).getOrElse(return false)
        val moy: MonthOfYear = calendrical.get(ISOChronology.monthOfYearRule).getOrElse(return false)
        var domVal: Int = calendrical.get(ISOChronology.dayOfMonthRule).getOrElse(return false)
        return domVal == moy.getLastDayOfMonth(ISOChronology.isLeapYear(yearVal))
      }
    }

  }

  /**
   * Returns the last day-of-month matcher, which returns true if the date
   * is the last valid day of the month.
   *
   * @return the last day-of-month matcher, never null
   */
  def lastDayOfMonth: CalendricalMatcher = Impl.LastDayOfMonth

  /**
   * Returns the leap day matcher, which returns true if the date
   * is February 29th in a leap year.
   *
   * @return the leap day matcher, never null
   */
  def leapDay: CalendricalMatcher = Impl.LeapDay

  /**
   * Returns the first in month matcher, which returns true if the date
   * is the first occurrence of day-of-week in the month.
   *
   * @param dayOfWeek the day-of-week, not null
   * @return the first in month matcher, never null
   */
  def firstInMonth(dayOfWeek: DayOfWeek): CalendricalMatcher = {
    if (dayOfWeek == null) {
      throw new NullPointerException("DayOfWeek must not be null")
    }
    return new CalendricalMatchers.DayOfWeekInMonth(1, dayOfWeek)
  }

  /**
   * Returns the day-of-week in month matcher, which returns true if the
   * date is the ordinal occurrence of the day-of-week in the month.
   *
   * @param ordinal ordinal, from 1 to 5
   * @param dayOfWeek the day-of-week, not null
   * @return the day-of-week in month matcher, never null
   * @throws IllegalArgumentException if the ordinal is invalid
   */
  def dayOfWeekInMonth(ordinal: Int, dayOfWeek: DayOfWeek): CalendricalMatcher = {
    if (ordinal < 1 || ordinal > 5) {
      throw new IllegalArgumentException("Illegal value for ordinal, value " + ordinal + " is not in the range 1 to 5")
    }
    if (dayOfWeek == null) {
      throw new NullPointerException("DayOfWeek must not be null")
    }
    return new CalendricalMatchers.DayOfWeekInMonth(ordinal, dayOfWeek)
  }

  /**
   * Returns the last day-of-year matcher, which returns true if the date is
   * the last valid day of the year.
   *
   * @return the last day-of-year matcher, never null
   */
  def lastDayOfYear: CalendricalMatcher = Impl.LastDayOfYear

  /**
   * Returns the leap year matcher, which returns true if the date
   * is in a leap year.
   *
   * @return the leap year matcher, never null
   */
  def leapYear: CalendricalMatcher = Impl.LeapYear

  /**
   * Class implementing day-of-week in month matcher.
   *
   * Constructor.
   * @param ordinal ordinal, from 1 to 5
   * @param dayOfWeek the day-of-week, not null

   */
  @SerialVersionUID(1L)
  private final class DayOfWeekInMonth(val ordinal: Int, val dayOfWeek: DayOfWeek) extends CalendricalMatcher with Serializable {
    /**{@inheritDoc}*/
    def matchesCalendrical(calendrical: Calendrical): Boolean = {
      val domVal: Int = calendrical.get(ISOChronology.dayOfMonthRule).getOrElse(return false)
      val dow: DayOfWeek = calendrical.get(ISOChronology.dayOfWeekRule).getOrElse(return false)
      if (dow != dayOfWeek) return false
      else return (domVal - 1) / 7 == ordinal - 1
    }

    /**{@inheritDoc}*/
    override def hashCode: Int = ordinal + 8 * dayOfWeek.ordinal

    /**{@inheritDoc}*/
    override def equals(obj: Any): Boolean = {
      if (obj.isInstanceOf[CalendricalMatchers.DayOfWeekInMonth]) {
        val other: CalendricalMatchers.DayOfWeekInMonth = obj.asInstanceOf[CalendricalMatchers.DayOfWeekInMonth]
        return ordinal == other.ordinal && dayOfWeek == other.dayOfWeek
      }
      return false
    }
  }

}