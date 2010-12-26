/*
 * Copyright (c) 2009-2010, Stephen Colebourne & Michael Nascimento Santos
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
package javax.time.calendar.zone

import util.control.Breaks._
import collection.mutable.{HashMap, ArrayBuffer}

import javax.time.calendar._

/**
 * A mutable builder used to create all the rules for a historic time-zone.
 * <p>
 * The rules of a time-zone describe how the offset changes over time.
 * The rules are created by building windows on the time-line within which
 * the different rules apply. The rules may be one of two kinds:
 * <ul>
 * <li>Fixed savings - A single fixed amount of savings from the standard offset will apply.</li>
 * <li>Rules - A set of one or more rules describe how daylight savings changes during the window.</li>
 * </ul>
 * <p>
 * TransitionRulesBuilder is a mutable class used to create instances of TimeZone.
 * It must only be used from a single thread.
 * The created TimeZone instances are immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object ZoneRulesBuilder {

  /**
   * A definition of the way a local time can be converted to an offset time.
   * <p>
   * Time zone rules are expressed in one of three ways:
   * <ul>
   * <li>Relative to UTC</li>
   * <li>Relative to the standard offset in force</li>
   * <li>Relative to the wall offset (what you would see on a clock on the wall)</li>
   * </ul>
   */

  sealed abstract class TimeDefinition(val ordinal: Int) {
    /**
     * Creates the offset date-time from the specified local date-time.
     * <p>
     * This method converts a local date-time to an offset date-time using an
     * algorithm based on the definition type.
     * The UTC type builds the offset date-time using the UTC offset.
     * The Standard type builds the offset date-time using the standard offset.
     * The Wall type builds the offset date-time using the wall offset.
     * The result always uses the wall-offset, thus a conversion may occur.
     *
     * @param dateTime the local date-time, not null
     * @param standardOffset the standard offset, not null
     * @param wallOffset the wall offset, not null
     * @return the created offset date-time in the wall offset, never null
     */
    def createDateTime(dateTime: LocalDateTime, standardOffset: ZoneOffset, wallOffset: ZoneOffset): OffsetDateTime = {
      this match {
        case TimeDefinition.UTC =>
          OffsetDateTime.of(dateTime, ZoneOffset.UTC).withOffsetSameInstant(wallOffset)
        case TimeDefinition.Standard =>
          OffsetDateTime.of(dateTime, standardOffset).withOffsetSameInstant(wallOffset)
        case TimeDefinition.Wall =>
          OffsetDateTime.of(dateTime, wallOffset)
      }
    }
  }

  object TimeDefinition {

    /**The local date-time is expressed in terms of the UTC offset. */
    object UTC extends TimeDefinition(0)

    /**The local date-time is expressed in terms of the wall offset. */
    object Wall extends TimeDefinition(1)

    /**The local date-time is expressed in terms of the standard offset. */
    object Standard extends TimeDefinition(2)

    def of(ordinal: Int): TimeDefinition = {
      ordinal match {
        case 0 => UTC
        case 1 => Wall
        case 2 => Standard
        case _ => throw new IllegalCalendarFieldValueException(null, ordinal, 0, 2)
      }
    }
  }

  /**
   * The maximum date-time.
   */
  private val MaxDateTime: LocalDateTime = LocalDateTime.of(Year.MaxYear, 12, 31, 23, 59, 59, 999999999)

  /**
   * Validates that the input value is not null.
   *
   * @param object the object to check
   * @param errorMessage the error to throw
   * @throws NullPointerException if the object is null
   */
  private def checkNotNull(obj: AnyRef, errorMessage: String): Unit =
    if (obj == null) throw new NullPointerException(errorMessage)
}

/**
 * Constructs an instance of the builder that can be used to create zone rules.
 * <p>
 * The builder is used by adding one or more windows representing portions
 * of the time-line. The standard offset from UTC will be constant within a window,
 * although two adjacent windows can have the same standard offset.
 * <p>
 * Within each window, there can either be a
 * {@link #setFixedSavingsToWindow fixed savings amount} or a
 * {@link #addRuleToWindow list of rules}.
 */
@SerialVersionUID(1L)
class ZoneRulesBuilder {

  import ZoneRulesBuilder._

  /**
   * Adds a multi-year transition rule to the current window.
   * <p>
   * This adds a rule such that the offset, expressed as a daylight savings amount,
   * changes at the specified date-time for each year in the range.
   *
   * @param startYear the start year of the rule, from MIN_YEAR to MAX_YEAR
   * @param endYear the end year of the rule, from MIN_YEAR to MAX_YEAR
   * @param month the month of the transition, not null
   * @param dayOfMonthIndicator the day-of-month of the transition, adjusted by dayOfWeek,
   *   from 1 to 31 adjusted later, or -1 to -28 adjusted earlier from the last day of the month
   * @param dayOfWeek the day-of-week to adjust to, null if day-of-month should not be adjusted
   * @param time the time that the transition occurs as defined by timeDefintion, not null
   * @param timeEndOfDay whether midnight is at the end of day
   * @param timeDefinition the definition of how to convert local to actual time, not null
   * @param savingAmount the amount of saving from the standard offset after the transition, not null
   * @return this, for chaining
   * @throws IllegalCalendarFieldValueException if a date-time field is out of range
   * @throws IllegalArgumentException if the day of month indicator is invalid
   * @throws IllegalArgumentException if the end of day midnight flag does not match the time
   * @throws IllegalStateException if no window has yet been added
   * @throws IllegalStateException if the window already has fixed savings
   * @throws IllegalStateException if the window has reached the maximum capacity of 2000 rules
   */
  def addRuleToWindow(startYear: Int, endYear: Int, month: MonthOfYear, dayOfMonthIndicator: Int, dayOfWeek: DayOfWeek, time: LocalTime, timeEndOfDay: Boolean, timeDefinition: ZoneRulesBuilder.TimeDefinition, savingAmount: Period): ZoneRulesBuilder = {
    checkNotNull(month, "Rule end month must not be null")
    checkNotNull(time, "Rule end time must not be null")
    checkNotNull(timeDefinition, "Time definition must not be null")
    checkNotNull(savingAmount, "Savings amount must not be null")
    ISOChronology.yearRule.checkValue(startYear)
    ISOChronology.yearRule.checkValue(endYear)
    if (dayOfMonthIndicator < -28 || dayOfMonthIndicator > 31 || dayOfMonthIndicator == 0) {
      throw new IllegalArgumentException("Day of month indicator must be between -28 and 31 inclusive excluding zero")
    }
    if (timeEndOfDay && time.equals(LocalTime.Midnight) == false) {
      throw new IllegalArgumentException("Time must be midnight when end of day flag is true")
    }
    if (windowList.isEmpty) {
      throw new IllegalStateException("Must add a window before adding a rule")
    }
    val window: ZoneRulesBuilder#TZWindow = windowList(windowList.size - 1)
    window.addRule(startYear, endYear, month, dayOfMonthIndicator, dayOfWeek, time, timeEndOfDay, timeDefinition, savingAmount)
    return this
  }

  /**
   * The list of windows.
   */
  private var windowList = new ArrayBuffer[ZoneRulesBuilder#TZWindow]
  /**
   * Adds a window to the builder that can be used to filter a set of rules.
   * <p>
   * This method defines and adds a window to the zone where the standard offset is specified.
   * The window limits the effect of subsequent additions of transition rules
   * or fixed savings. If neither rules or fixed savings are added to the window
   * then the window will default to no savings.
   * <p>
   * Each window must be added sequentially, as the start instant of the window
   * is derived from the until instant of the previous window.
   *
   * @param standardOffset the standard offset, not null
   * @param until the date-time that the offset applies until, not null
   * @param untilDefinition the time type for the until date-time, not null
   * @return this, for chaining
   * @throws IllegalStateException if the window order is invalid
   */
  def addWindow(standardOffset: ZoneOffset, until: LocalDateTime, untilDefinition: ZoneRulesBuilder.TimeDefinition): ZoneRulesBuilder = {
    checkNotNull(standardOffset, "Standard offset must not be null")
    checkNotNull(until, "Until date-time must not be null")
    checkNotNull(untilDefinition, "Time definition must not be null")
    val window: ZoneRulesBuilder#TZWindow = new TZWindow(standardOffset, until, untilDefinition)
    if (windowList.size > 0) {
      val previous: ZoneRulesBuilder#TZWindow = windowList(windowList.size - 1)
      window.validateWindowOrder(previous)
    }
    windowList += window
    return this
  }

  /**
   * Completes the build converting the builder to a set of time-zone rules.
   * <p>
   * Calling this method alters the state of the builder.
   * Further rules should not be added to this builder once this method is called.
   *
   * @param id the time-zone id, not null
   * @return the zone rules, never null
   * @throws IllegalStateException if no windows have been added
   * @throws IllegalStateException if there is only one rule defined as being forever for any given window
   */
  def toRules(id: String): ZoneRules = toRules(id, new HashMap[Any, Any])

  /**
   * A definition of the way a local time can be converted to an offset time.
   */
  /**
   * Constructor.
   *
   * @param year the year
   * @param month the month, not null
   * @param dayOfMonthIndicator the day-of-month of the transition, adjusted by dayOfWeek,
   *   from 1 to 31 adjusted later, or -1 to -28 adjusted earlier from the last day of the month
   * @param dayOfWeek the day-of-week, null if day-of-month is exact
   * @param time the time, not null
   * @param timeEndOfDay whether midnight is at the end of day
   * @param timeDefinition the time definition, not null
   * @param savingAfter the savings amount, not null
   */
  private[zone] class TZRule(var year: Int,
                             var month: MonthOfYear,
                             var dayOfMonthIndicator: Int,
                             var dayOfWeek: DayOfWeek,
                             var time: LocalTime,
                             var timeEndOfDay: Boolean,
                             var timeDefinition: ZoneRulesBuilder.TimeDefinition,
                             var savingAmount: Period)
    extends Ordered[ZoneRulesBuilder#TZRule] {
    /**
     * Converts this to a transition rule.
     *
     * @param standardOffset the active standard offset, not null
     * @param savingsBefore the active savings before the transition, not null
     * @return the transition, never null
     */
    private[zone] def toTransitionRule(standardOffset: ZoneOffset, savingsBefore: Period): ZoneOffsetTransitionRule = {
      if (dayOfMonthIndicator < 0) {
        if (month != MonthOfYear.February) {
          dayOfMonthIndicator = month.maxLengthInDays - 6
        }
      }
      if (timeEndOfDay && dayOfMonthIndicator > 0 && (dayOfMonthIndicator == 28 && month == MonthOfYear.February) == false) {
        val date: LocalDate = LocalDate.of(2004, month, dayOfMonthIndicator).plusDays(1)
        month = date.getMonthOfYear
        dayOfMonthIndicator = date.getDayOfMonth
        if (dayOfWeek != null) {
          dayOfWeek = dayOfWeek.next
        }
        timeEndOfDay = false
      }
      val trans: ZoneOffsetTransition = toTransition(standardOffset, savingsBefore)
      return new ZoneOffsetTransitionRule(month, dayOfMonthIndicator, dayOfWeek, time, timeEndOfDay, timeDefinition, standardOffset, trans.getOffsetBefore, trans.getOffsetAfter)
    }

    /**
     * Converts this to a transition.
     *
     * @param standardOffset the active standard offset, not null
     * @param savingsBefore the active savings, not null
     * @return the transition, never null
     */
    private[zone] def toTransition(standardOffset: ZoneOffset, savingsBefore: Period): ZoneOffsetTransition = {
      var date: LocalDate = null
      if (dayOfMonthIndicator < 0) {
        date = LocalDate.of(year, month, month.getLastDayOfMonth(ISOChronology.isLeapYear(year)) + 1 + dayOfMonthIndicator)
        if (dayOfWeek != null) {
          date = date.`with`(DateAdjusters.previousOrCurrent(dayOfWeek))
        }
      }
      else {
        date = LocalDate.of(year, month, dayOfMonthIndicator)
        if (dayOfWeek != null) {
          date = date.`with`(DateAdjusters.nextOrCurrent(dayOfWeek))
        }
      }
      if (timeEndOfDay) {
        date = date.plusDays(1)
      }
      date = deduplicate(date)
      val ldt: LocalDateTime = deduplicate(LocalDateTime.of(date, time))
      val wallOffset: ZoneOffset = deduplicate(standardOffset.plus(savingsBefore))
      val dt: OffsetDateTime = deduplicate(timeDefinition.createDateTime(ldt, standardOffset, wallOffset))
      val offsetAfter: ZoneOffset = deduplicate(standardOffset.plus(savingAmount))
      return new ZoneOffsetTransition(dt, offsetAfter)
    }

    /**{ @inheritDoc}. */
    def compare(other: ZoneRulesBuilder#TZRule): Int = {
      var cmp: Int = year - other.year
      cmp = (if (cmp == 0) month.compareTo(other.month) else cmp)
      cmp = (if (cmp == 0) dayOfMonthIndicator - other.dayOfMonthIndicator else cmp)
      cmp = (if (cmp == 0) time.compareTo(other.time) else cmp)
      return cmp
    }
  }

  /**
   * Adds a single transition rule to the current window.
   * <p>
   * This adds a rule such that the offset, expressed as a daylight savings amount,
   * changes at the specified date-time.
   *
   * @param dateTime the date-time that the transition occurs as defined by timeDefintion, not null
   * @param timeDefinition the definition of how to convert local to actual time, not null
   * @param savingAmount the amount of saving from the standard offset after the transition, not null
   * @return this, for chaining
   * @throws IllegalStateException if no window has yet been added
   * @throws IllegalStateException if the window already has fixed savings
   * @throws IllegalStateException if the window has reached the maximum capacity of 2000 rules
   */
  def addRuleToWindow(dateTime: LocalDateTime, timeDefinition: ZoneRulesBuilder.TimeDefinition, savingAmount: Period): ZoneRulesBuilder = {
    checkNotNull(dateTime, "Rule end date-time must not be null")
    return addRuleToWindow(dateTime.getYear, dateTime.getYear, dateTime.getMonthOfYear, dateTime.getDayOfMonth, null, dateTime.toLocalTime, false, timeDefinition, savingAmount)
  }

  /**
   * A definition of a window in the time-line.
   * The window will have one standard offset and will either have a
   * fixed DST savings or a set of rules.
   */
  /**
   * Constructor.
   *
   * @param standardOffset the standard offset applicable during the window, not null
   * @param windowEnd the end of the window, relative to the time definition, null if forever
   * @param timeDefinition the time definition for calculating the true end, not null
   */
  private[zone] class TZWindow(val standardOffset: ZoneOffset,
                               val windowEnd: LocalDateTime,
                               timeDefinition: ZoneRulesBuilder.TimeDefinition) {

    /**The fixed amount of the saving to be applied during this window. */
    private[zone] var fixedSavingAmount: Period = null
    /**The last rules. */
    private[ZoneRulesBuilder] var lastRuleList = new ArrayBuffer[ZoneRulesBuilder#TZRule]
    /**
     * Sets the fixed savings amount for the window.
     *
     * @param fixedSavingAmount the amount of daylight saving to apply throughout the window, may be null
     * @throws IllegalStateException if the window already has rules
     */
    private[zone] def setFixedSavings(fixedSavingAmount: Period): Unit = {
      if (ruleList.size > 0 || lastRuleList.size > 0) {
        throw new IllegalStateException("Window has DST rules, so cannot have fixed savings")
      }
      this.fixedSavingAmount = fixedSavingAmount
    }

    /**
     * Validates that this window is after the previous one.
     *
     * @param previous the previous window, not null
     * @throws IllegalStateException if the window order is invalid
     */
    private[zone] def validateWindowOrder(previous: ZoneRulesBuilder#TZWindow): Unit = {
      if (windowEnd.isBefore(previous.windowEnd)) {
        throw new IllegalStateException("Windows must be added in date-time order: " + windowEnd + " < " + previous.windowEnd)
      }
    }

    /**The latest year that the last year starts at. */
    private var maxLastRuleStartYear: Int = Year.MinYear
    /**The rules for the current window. */
    private[ZoneRulesBuilder] var ruleList = new ArrayBuffer[ZoneRulesBuilder#TZRule]
    /**
     * Adds rules to make the last rules all start from the same year.
     * Also add one more year to avoid weird case where penultimate year has odd offset.
     *
     * @param windowStartYear the window start year
     * @throws IllegalStateException if there is only one rule defined as being forever
     */
    private[zone] def tidy(windowStartYear: Int): Unit = {
      if (lastRuleList.size == 1) {
        throw new IllegalStateException("Cannot have only one rule defined as being forever")
      }
      if (windowEnd.equals(MaxDateTime)) {
        maxLastRuleStartYear = math.max(maxLastRuleStartYear, windowStartYear) + 1
        for (lastRule <- lastRuleList) {
          addRule(lastRule.year, maxLastRuleStartYear, lastRule.month, lastRule.dayOfMonthIndicator, lastRule.dayOfWeek, lastRule.time, lastRule.timeEndOfDay, lastRule.timeDefinition, lastRule.savingAmount)
          lastRule.year = maxLastRuleStartYear + 1
        }
        if (maxLastRuleStartYear == Year.MaxYear) {
          lastRuleList.clear
        }
        else {
          maxLastRuleStartYear += 1;
        }
      }
      else {
        val endYear: Int = windowEnd.getYear
        for (lastRule <- lastRuleList) {
          addRule(lastRule.year, endYear + 1, lastRule.month, lastRule.dayOfMonthIndicator, lastRule.dayOfWeek, lastRule.time, lastRule.timeEndOfDay, lastRule.timeDefinition, lastRule.savingAmount)
        }
        lastRuleList.clear
        maxLastRuleStartYear = Year.MaxYear
      }
      ruleList = ruleList.sorted
      lastRuleList = lastRuleList.sorted
      if (ruleList.size == 0 && fixedSavingAmount == null) {
        fixedSavingAmount = Period.Zero
      }
    }

    /**
     * Adds a rule to the current window.
     *
     * @param startYear the start year of the rule, from MIN_YEAR to MAX_YEAR
     * @param endYear the end year of the rule, from MIN_YEAR to MAX_YEAR
     * @param month the month of the transition, not null
     * @param dayOfMonthIndicator the day-of-month of the transition, adjusted by dayOfWeek,
     *   from 1 to 31 adjusted later, or -1 to -28 adjusted earlier from the last day of the month
     * @param dayOfWeek the day-of-week to adjust to, null if day-of-month should not be adjusted
     * @param time the time that the transition occurs as defined by timeDefintion, not null
     * @param timeEndOfDay whether midnight is at the end of day
     * @param timeDefinition the definition of how to convert local to actual time, not null
     * @param savingAmount the amount of saving from the standard offset, not null
     * @throws IllegalStateException if the window already has fixed savings
     * @throws IllegalStateException if the window has reached the maximum capacity of 2000 rules
     */
    private[zone] def addRule(startYear: Int, _endYear: Int, month: MonthOfYear, dayOfMonthIndicator: Int, dayOfWeek: DayOfWeek, time: LocalTime, timeEndOfDay: Boolean, timeDefinition: ZoneRulesBuilder.TimeDefinition, savingAmount: Period): Unit = {
      if (fixedSavingAmount != null) {
        throw new IllegalStateException("Window has a fixed DST saving, so cannot have DST rules")
      }
      if (ruleList.size >= 2000) {
        throw new IllegalStateException("Window has reached the maximum number of allowed rules")
      }
      var lastRule: Boolean = false
      var endYear = _endYear
      if (endYear == Year.MaxYear) {
        lastRule = true
        endYear = startYear
      }
      var year: Int = startYear
      while (year <= endYear) {
        val rule: ZoneRulesBuilder#TZRule = new TZRule(year, month, dayOfMonthIndicator, dayOfWeek, time, timeEndOfDay, timeDefinition, savingAmount)
        if (lastRule) {
          lastRuleList += rule
          maxLastRuleStartYear = math.max(startYear, maxLastRuleStartYear)
        }
        else {
          ruleList += rule
        }
        year += 1;
      }
    }

    /**
     * Checks if the window is empty.
     *
     * @return true if the window is only a standard offset
     */
    private[zone] def isSingleWindowStandardOffset: Boolean = {
      return windowEnd.equals(MaxDateTime) && timeDefinition == TimeDefinition.Wall && fixedSavingAmount == null && lastRuleList.isEmpty && ruleList.isEmpty
    }

    /**
     * Creates the offset date-time for the local date-time at the end of the window.
     *
     * @param savings the amount of savings in use, not null
     * @return the created offset date-time in the wall offset, never null
     */
    private[zone] def createDateTime(savings: Period): OffsetDateTime = {
      val wallOffset: ZoneOffset = standardOffset.plus(savings)
      timeDefinition.createDateTime(windowEnd, standardOffset, wallOffset)
    }
  }

  /**
   * Adds a single transition rule to the current window.
   * <p>
   * This adds a rule such that the offset, expressed as a daylight savings amount,
   * changes at the specified date-time.
   *
   * @param year the year of the transition, from MIN_YEAR to MAX_YEAR
   * @param month the month of the transition, not null
   * @param dayOfMonthIndicator the day-of-month of the transition, adjusted by dayOfWeek,
   *   from 1 to 31 adjusted later, or -1 to -28 adjusted earlier from the last day of the month
   * @param time the time that the transition occurs as defined by timeDefintion, not null
   * @param timeEndOfDay whether midnight is at the end of day
   * @param timeDefinition the definition of how to convert local to actual time, not null
   * @param savingAmount the amount of saving from the standard offset after the transition, not null
   * @return this, for chaining
   * @throws IllegalCalendarFieldValueException if a date-time field is out of range
   * @throws IllegalStateException if no window has yet been added
   * @throws IllegalStateException if the window already has fixed savings
   * @throws IllegalStateException if the window has reached the maximum capacity of 2000 rules
   */
  def addRuleToWindow(year: Int, month: MonthOfYear, dayOfMonthIndicator: Int, time: LocalTime, timeEndOfDay: Boolean, timeDefinition: ZoneRulesBuilder.TimeDefinition, savingAmount: Period): ZoneRulesBuilder = {
    addRuleToWindow(year, year, month, dayOfMonthIndicator, null, time, timeEndOfDay, timeDefinition, savingAmount)
  }

  /**
   * Deduplicates an object instance.
   *
   * @param <T> the generic type
   * @param object the object to deduplicate
   * @return the deduplicated object
   */
  private[zone] def deduplicate[T](obj: T): T = {
    if (deduplicateMap.contains(obj) == false) deduplicateMap.put(obj, obj).getOrElse(null)
    else deduplicateMap.get(obj).asInstanceOf[T]
  }

  /**
   * A map for deduplicating the output.
   */
  private var deduplicateMap: HashMap[Any, Any] = null
  /**
   * Completes the build converting the builder to a set of time-zone rules.
   * <p>
   * Calling this method alters the state of the builder.
   * Further rules should not be added to this builder once this method is called.
   *
   * @param id the time-zone id, not null
   * @param deduplicateMap a map for deduplicating the values, not null
   * @return the zone rules, never null
   * @throws IllegalStateException if no windows have been added
   * @throws IllegalStateException if there is only one rule defined as being forever for any given window
   */
  private[zone] def toRules(id: String, deduplicateMap: HashMap[Any, Any]): ZoneRules = {
    checkNotNull(id, "Time zone id must not be null")
    this.deduplicateMap = deduplicateMap
    if (windowList.isEmpty) {
      throw new IllegalStateException("No windows have been added to the builder")
    }
    if (windowList.size == 1) {
      val window: ZoneRulesBuilder#TZWindow = windowList(0)
      if (window.isSingleWindowStandardOffset) {
        return ZoneRules.ofFixed(window.standardOffset)
      }
    }
    val standardOffsetList = new ArrayBuffer[OffsetDateTime](4)
    val transitionList = new ArrayBuffer[ZoneOffsetTransition](256)
    val lastTransitionRuleList = new ArrayBuffer[ZoneOffsetTransitionRule](2)
    val firstWindow: ZoneRulesBuilder#TZWindow = windowList(0)
    var standardOffset: ZoneOffset = firstWindow.standardOffset
    var savings: Period = Period.Zero
    if (firstWindow.fixedSavingAmount != null) {
      savings = firstWindow.fixedSavingAmount
    }
    val firstWallOffset: ZoneOffset = deduplicate(standardOffset.plus(savings))
    var windowStart: OffsetDateTime = deduplicate(OffsetDateTime.of(Year.MinYear, 1, 1, 0, 0, firstWallOffset))
    for (window <- windowList) {
      window.tidy(windowStart.getYear)
      var effectiveSavings: Period = window.fixedSavingAmount
      if (effectiveSavings == null) {
        effectiveSavings = Period.Zero
        breakable{
          for (rule <- window.ruleList) {
            val trans: ZoneOffsetTransition = rule.toTransition(standardOffset, savings)
            if (trans.getDateTime.isAfter(windowStart)) {
              break
            }
            effectiveSavings = rule.savingAmount
          }
        }
      }
      if (standardOffset.equals(window.standardOffset) == false) {
        standardOffset = deduplicate(window.standardOffset)
        standardOffsetList += deduplicate(windowStart.withOffsetSameInstant(standardOffset))
      }
      val effectiveWallOffset: ZoneOffset = deduplicate(standardOffset.plus(effectiveSavings))
      if (windowStart.getOffset.equals(effectiveWallOffset) == false) {
        val trans: ZoneOffsetTransition = deduplicate(new ZoneOffsetTransition(windowStart, effectiveWallOffset))
        transitionList += trans
      }
      savings = effectiveSavings
      for (rule <- window.ruleList) {
        val trans: ZoneOffsetTransition = deduplicate(rule.toTransition(standardOffset, savings))
        if (trans.getDateTime.isBefore(windowStart) == false && trans.getDateTime.isBefore(window.createDateTime(savings)) && trans.getOffsetBefore.equals(trans.getOffsetAfter) == false) {
          transitionList += trans
          savings = rule.savingAmount
        }
      }
      for (lastRule <- window.lastRuleList) {
        val transitionRule: ZoneOffsetTransitionRule = deduplicate(lastRule.toTransitionRule(standardOffset, savings))
        lastTransitionRuleList += transitionRule
        savings = lastRule.savingAmount
      }
      windowStart = deduplicate(window.createDateTime(savings))
    }
    return new StandardZoneRules(firstWindow.standardOffset, firstWallOffset, standardOffsetList, transitionList, lastTransitionRuleList)
  }

  /**
   * Sets the previously added window to have fixed savings.
   * <p>
   * Setting a window to have fixed savings simply means that a single daylight
   * savings amount applies throughout the window. The window could be small,
   * such as a single summer, or large, such as a multi-year daylight savings.
   * <p>
   * A window can either have fixed savings or rules but not both.
   *
   * @param fixedSavingAmount the amount of saving to use for the whole window, not null
   * @return this, for chaining
   * @throws IllegalStateException if no window has yet been added
   * @throws IllegalStateException if the window already has rules
   */
  def setFixedSavingsToWindow(fixedSavingAmount: Period): ZoneRulesBuilder = {
    checkNotNull(fixedSavingAmount, "Fixed savings amount must not be null")
    if (windowList.isEmpty) {
      throw new IllegalStateException("Must add a window before setting the fixed savings")
    }
    val window: ZoneRulesBuilder#TZWindow = windowList(windowList.size - 1)
    window.setFixedSavings(fixedSavingAmount)
    return this
  }

  /**
   * Adds a window that applies until the end of time to the builder that can be
   * used to filter a set of rules.
   * <p>
   * This method defines and adds a window to the zone where the standard offset is specified.
   * The window limits the effect of subsequent additions of transition rules
   * or fixed savings. If neither rules or fixed savings are added to the window
   * then the window will default to no savings.
   * <p>
   * This must be added after all other windows.
   * No more windows can be added after this one.
   *
   * @param standardOffset the standard offset, not null
   * @return this, for chaining
   * @throws IllegalStateException if a forever window has already been added
   */
  def addWindowForever(standardOffset: ZoneOffset): ZoneRulesBuilder = addWindow(standardOffset, MaxDateTime, TimeDefinition.Wall)
}