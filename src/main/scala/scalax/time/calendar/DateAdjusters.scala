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

/**
 * Provides common implementations of {@code DateAdjuster}.
 * <p>
 * DateAdjusters is a utility class.
 * All adjusters returned are immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */

object DateAdjusters {
  /**
   * Returns the previous day-of-week adjuster, which adjusts the date to be
   * the previous of the specified day-of-week after the specified date.
   *
   * @param dow the day-of-week to move the date to, not null
   * @return the next day-of-week adjuster, never null
   */
  def previous(dow: DayOfWeek): DateAdjuster = {
    if (dow == null) throw new NullPointerException("dow must not be null")
    else new DateAdjusters.RelativeDayOfWeek(3, dow)
  }

  /**
   * Implementation of next, previous or current day-of-week.
   * @param relative whether the current date is a valid answer.
   * @param dow The day-of-week to find.
   */
  @SerialVersionUID(1L)
  private[DateAdjusters] final class RelativeDayOfWeek(val relative: Int, val dow: DayOfWeek) extends DateAdjuster with Serializable {
    override def hashCode: Int = {
      var hash: Int = 13
      hash = 19 * hash + relative
      hash = 19 * hash + dow.hashCode
      return hash
    }

    /**{@inheritDoc}*/
    override def adjustDate(date: Date): Date = {
      var dow: DayOfWeek = date.getDayOfWeek
      if (relative < 2 && dow == this.dow) {
        return date
      }
      if ((relative & 1) == 0) {
        val daysDiff: Int = dow.ordinal - this.dow.ordinal
        return date.plusDays(if (daysDiff >= 0) 7 - daysDiff else -daysDiff)
      }
      else {
        val daysDiff: Int = this.dow.ordinal - dow.ordinal
        return date.minusDays(if (daysDiff >= 0) 7 - daysDiff else -daysDiff)
      }
    }

    /**{@inheritDoc}*/
    override def equals(obj: Any): Boolean =
      if (obj == null) return false
      else
        obj match {
          case other: DateAdjusters.RelativeDayOfWeek => (this eq other) ||
            (relative == other.relative && dow == other.dow)
          case _ => false
        }
  }

  /**
   * Returns the next or current day-of-week adjuster, which adjusts the
   * date to be be the next of the specified day-of-week, returning the
   * input date if the day-of-week matched.
   *
   * @param dow the day-of-week to move the date to, not null
   * @return the next day-of-week adjuster, never null
   */
  def nextOrCurrent(dow: DayOfWeek): DateAdjuster = {
    if (dow == null) throw new NullPointerException("dow must not be null")
    else new DateAdjusters.RelativeDayOfWeek(0, dow)
  }

  /**
   * Enum implementing the adjusters.
   */
  private object Impl {

    /**Last day-of-year adjuster. */
    object LastDayOfYear extends DateAdjuster {
      /**{@inheritDoc}*/
      override def adjustDate(date: Date): Date = Date(date.getYear, MonthOfYear.December, 31)
    }

    /**First day-of-year adjuster. */
    object FirstDayOfMonth extends DateAdjuster {
      /**{@inheritDoc}*/
      override def adjustDate(date: Date): Date = date.copy(day = 1)
    }

    /**Last day-of-month adjuster. */
    object LastDayOfMonth extends DateAdjuster {
      /**{@inheritDoc}*/
      override def adjustDate(date: Date): Date = {
        val dom: Int = date.getMonthOfYear.lastDayOfMonth(ISOChronology.isLeapYear(date.getYear))
        return date.copy(month = MonthOfYear(dom))
      }
    }

    /**First day-of-year adjuster. */
    object FirstDayOfYear extends DateAdjuster {
      /**{@inheritDoc}*/
      override def adjustDate(date: Date): Date = Date(date.getYear, MonthOfYear.January, 1)
    }

    /**Next non weekend day adjuster. */
    object NextNonWeekend extends DateAdjuster {
      /**{@inheritDoc}*/
      override def adjustDate(date: Date): Date = {
        val dow: DayOfWeek = date.getDayOfWeek
        dow match {
          case DayOfWeek.Saturday =>
            return date.plusDays(2)
          case DayOfWeek.Friday =>
            return date.plusDays(3)
          case _ =>
            return date.plusDays(1)
        }
      }
    }

  }

  /**
   * Returns the last day-of-year adjuster, which returns a new date with
   * the day-of-year changed to be the last day of the year - December 31.
   * <p>
   * The input 2007-01-15 will return 2007-12-31.<br />
   * The input 2008-02-15 will return 2008-12-31.<br />
   *
   * @return the last day-of-year adjuster, never null
   */
  def lastDayOfYear: DateAdjuster = Impl.LastDayOfYear

  /**
   * Returns the next non weekend day adjuster, which adjusts the date one day
   * forward skipping Saturday and Sunday.
   *
   * @return the next working day adjuster, never null
   */
  def nextNonWeekendDay: DateAdjuster = Impl.NextNonWeekend

  /**
   * Returns the first day-of-year adjuster, which returns a new date with
   * the day-of-year changed to be the first day of the year - January 1.
   * <p>
   * The input 2007-01-15 will return 2007-01-01.<br />
   * The input 2008-02-15 will return 2008-01-01.<br />
   *
   * @return the first day-of-year adjuster, never null
   */
  def firstDayOfYear: DateAdjuster = Impl.FirstDayOfYear

  /**
   * Returns the day-of-week in month adjuster, which returns a new date
   * in the same month with the ordinal day-of-week. This is used for
   * expressions like 'second Tuesday in March'.
   * <p>
   * The input 2007-12-15 for (1,Monday) will return 2007-12-03.<br />
   * The input 2007-12-15 for (2,Tuesday) will return 2007-12-11.<br />
   * The input 2007-12-15 for (3,Tuesday) will return 2007-12-18.<br />
   * The input 2007-12-15 for (4,Tuesday) will return 2007-12-25.<br />
   * The input 2007-12-15 for (5,Tuesday) will return 2008-01-01.<br />
   * <p>
   * If the ordinal is 5 and there is no 5th of the requested day-of-week,
   * then the first of the next month is returned.
   *
   * @param ordinal ordinal, from 1 to 5
   * @param dayOfWeek the day-of-week, not null
   * @return the day-of-week in month adjuster, never null
   * @throws IllegalArgumentException if the ordinal is invalid
   */
  def dayOfWeekInMonth(ordinal: Int, dayOfWeek: DayOfWeek): DateAdjuster = {
    if (ordinal < 1 || ordinal > 5) {
      throw new IllegalArgumentException("Illegal value for ordinal, value " + ordinal + " is not in the range 1 to 5")
    }
    if (dayOfWeek == null) {
      throw new NullPointerException("DayOfWeek must not be null")
    }
    new DateAdjusters.DayOfWeekInMonth(ordinal, dayOfWeek)
  }

  /**
   * Class implementing day-of-week in month adjuster.
   */
  /**
   * Constructor.
   * @param ordinal ordinal, from 1 to 5
   * @param dayOfWeek the day-of-week, not null
   */
  @SerialVersionUID(1L)
  private[DateAdjusters] final class DayOfWeekInMonth(val ordinal: Int, val dayOfWeek: DayOfWeek) extends DateAdjuster with Serializable {
    /**{@inheritDoc}*/
    override def hashCode: Int = ordinal + 8 * dayOfWeek.ordinal

    /**{@inheritDoc}*/
    override def adjustDate(date: Date): Date = {
      val temp: Date = date.copy(day = 1)
      val curDow: Int = temp.getDayOfWeek.ordinal
      val newDow: Int = dayOfWeek.ordinal
      var dowDiff: Int = (newDow - curDow + 7) % 7
      dowDiff += (ordinal - 1) * 7
      return temp.plusDays(dowDiff)
    }

    /**{@inheritDoc}*/
    override def equals(obj: Any): Boolean = {
      if (obj.isInstanceOf[DateAdjusters.DayOfWeekInMonth]) {
        val other: DateAdjusters.DayOfWeekInMonth = obj.asInstanceOf[DateAdjusters.DayOfWeekInMonth]
        return ordinal == other.ordinal && dayOfWeek == other.dayOfWeek
      }
      return false
    }
  }

  /**
   * Returns the first in month adjuster, which returns a new date
   * in the same month with the first matching day-of-week. This is used for
   * expressions like 'first Tuesday in March'.
   * <p>
   * The input 2007-12-15 for (Monday) will return 2007-12-03.<br />
   * The input 2007-12-15 for (Tuesday) will return 2007-12-04.<br />
   *
   * @param dayOfWeek the day-of-week, not null
   * @return the first in month adjuster, never null
   */
  def firstInMonth(dayOfWeek: DayOfWeek): DateAdjuster = {
    if (dayOfWeek == null) {
      throw new NullPointerException("DayOfWeek must not be null")
    }
    new DateAdjusters.DayOfWeekInMonth(1, dayOfWeek)
  }

  /**
   * Returns the next day-of-week adjuster, which adjusts the date to be
   * the next of the specified day-of-week after the specified date.
   *
   * @param dow the day-of-week to move the date to, not null
   * @return the next day-of-week adjuster, never null
   */
  def next(dow: DayOfWeek): DateAdjuster = {
    if (dow == null) throw new NullPointerException("dow must not be null")
    else new DateAdjusters.RelativeDayOfWeek(2, dow)
  }

  /**
   * Returns the last day-of-month adjuster, which returns a new date with
   * the day-of-month changed to be the last valid day of the month.
   * <p>
   * The input 2007-01-15 will return 2007-01-31.<br />
   * The input 2007-02-15 will return 2007-02-28.<br />
   * The input 2007-03-15 will return 2007-03-31.<br />
   * The input 2007-04-15 will return 2007-04-30.<br />
   * The input 2008-02-15 will return 2008-02-29.
   *
   * @return the last day-of-month adjuster, never null
   */
  def lastDayOfMonth: DateAdjuster = Impl.LastDayOfMonth

  /**
   * Returns the first day-of-month adjuster, which returns a new date with
   * the day-of-month changed to be the first day of the month.
   * <p>
   * The input 2007-01-15 will return 2007-01-01.<br />
   * The input 2008-02-15 will return 2008-02-01.
   *
   * @return the first day-of-month adjuster, never null
   */
  def firstDayOfMonth: DateAdjuster = Impl.FirstDayOfMonth

  /**
   * Returns the previous or current day-of-week adjuster, which adjusts the
   * date to be be the previous of the specified day-of-week, returning the
   * input date if the day-of-week matched.
   *
   * @param dow the day-of-week to move the date to, not null
   * @return the next day-of-week adjuster, never null
   */
  def previousOrCurrent(dow: DayOfWeek): DateAdjuster = {
    if (dow == null) throw new NullPointerException("dow must not be null")
    else new DateAdjusters.RelativeDayOfWeek(1, dow)
  }
}