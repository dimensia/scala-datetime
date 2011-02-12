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

import java.io.DataInput
import java.io.DataOutput
import javax.time.calendar.DateAdjusters
import javax.time.calendar.DayOfWeek
import javax.time.calendar.ISOChronology
import javax.time.calendar.LocalDate
import javax.time.calendar.LocalDateTime
import javax.time.calendar.LocalTime
import javax.time.calendar.MonthOfYear
import javax.time.calendar.OffsetDateTime
import javax.time.calendar.ZoneOffset
import javax.time.calendar.zone.ZoneRulesBuilder.TimeDefinition

/**
 * A rule expressing how to create a transition.
 * <p>
 * This class allows rules for identifying future transitions to be expressed.
 * A rule might be written in many forms:
 * <ul>
 * <li>the 16th March
 * <li>the Sunday on or after the 16th March
 * <li>the Sunday on or before the 16th March
 * <li>the last Sunday in February
 * </ul>
 * These different rule types can be expressed and queried.
 * <p>
 * ZoneOffsetTransitionRule is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object ZoneOffsetTransitionRule {
  /**
   * Creates an instance defining the yearly rule to create transitions between two offsets.
   * <p>
   * Applications should normally obtain an instance from {@link ZoneRules}.
   * This constructor is intended for use by implementors of {@code ZoneRules}.
   *
   * @param month the month of the month-day of the first day of the cutover week, not null
   * @param dayOfMonthIndicator the day of the month-day of the cutover week, positive if the week is that
   *  day or later, negative if the week is that day or earlier, counting from the last day of the month,
   *  from -28 to 31 excluding 0
   * @param dayOfWeek the required day-of-week, null if the month-day should not be changed
   * @param time the cutover time in the 'before' offset, not null
   * @param timeEndOfDay whether the time is midnight at the end of day
   * @param timeDefnition how to interpret the cutover
   * @param standardOffset the standard offset in force at the cutover, not null
   * @param offsetBefore the offset before the cutover, not null
   * @param offsetAfter the offset after the cutover, not null
   * @throws IllegalArgumentException if the day of month indicator is invalid
   * @throws IllegalArgumentException if the end of day flag is true when the time is not midnight
   */
  def of(month: MonthOfYear, dayOfMonthIndicator: Int, dayOfWeek: DayOfWeek, time: LocalTime, timeEndOfDay: Boolean, timeDefnition: ZoneRulesBuilder.TimeDefinition, standardOffset: ZoneOffset, offsetBefore: ZoneOffset, offsetAfter: ZoneOffset): ZoneOffsetTransitionRule = {
    ZoneRules.checkNotNull(month, "MonthOfYear must not be null")
    ZoneRules.checkNotNull(time, "LocalTime must not be null")
    ZoneRules.checkNotNull(timeDefnition, "TimeDefinition must not be null")
    ZoneRules.checkNotNull(standardOffset, "Standard offset must not be null")
    ZoneRules.checkNotNull(offsetBefore, "Offset before must not be null")
    ZoneRules.checkNotNull(offsetAfter, "Offset after must not be null")
    if (dayOfMonthIndicator < -28 || dayOfMonthIndicator > 31 || dayOfMonthIndicator == 0) {
      throw new IllegalArgumentException("Day of month indicator must be between -28 and 31 inclusive excluding zero")
    }
    if (timeEndOfDay && time.equals(LocalTime.Midnight) == false) {
      throw new IllegalArgumentException("Time must be midnight when end of day flag is true")
    }
    return new ZoneOffsetTransitionRule(month, dayOfMonthIndicator, dayOfWeek, time, timeEndOfDay, timeDefnition, standardOffset, offsetBefore, offsetAfter)
  }

  /**
   * Reads the state from the stream.
   * @param in the input stream, not null
   * @return the created object, never null
   * @throws IOException if an error occurs
   */
  private[zone] def readExternal(in: DataInput): ZoneOffsetTransitionRule = {
    val data: Int = in.readInt
    val month: MonthOfYear = MonthOfYear.of(data >>> 28)
    val dom: Int = ((data & (63 << 22)) >>> 22) - 32
    val dowByte: Int = (data & (7 << 19)) >>> 19
    val dow: DayOfWeek = if (dowByte == 0) null else DayOfWeek.of(dowByte)
    val timeByte: Int = (data & (31 << 14)) >>> 14
    val defn: ZoneRulesBuilder.TimeDefinition = TimeDefinition.of((data & (3 << 12)) >>> 12)
    var stdByte: Int = (data & (255 << 4)) >>> 4
    var beforeByte: Int = (data & (3 << 2)) >>> 2
    var afterByte: Int = (data & 3)
    val time: LocalTime = (if (timeByte == 31) LocalTime.ofSecondOfDay(in.readInt) else LocalTime.of(timeByte % 24, 0))
    val std: ZoneOffset = (if (stdByte == 255) ZoneOffset.ofTotalSeconds(in.readInt) else ZoneOffset.ofTotalSeconds((stdByte - 128) * 900))
    val before: ZoneOffset = (if (beforeByte == 3) ZoneOffset.ofTotalSeconds(in.readInt) else ZoneOffset.ofTotalSeconds(std.getAmountSeconds + beforeByte * 1800))
    val after: ZoneOffset = (if (afterByte == 3) ZoneOffset.ofTotalSeconds(in.readInt) else ZoneOffset.ofTotalSeconds(std.getAmountSeconds + afterByte * 1800))
    return ZoneOffsetTransitionRule.of(month, dom, dow, time, timeByte == 24, defn, std, before, after)
  }
}

/**
 * Creates an instance defining the yearly rule to create transitions between two offsets.
 *
 * @param month the month of the month-day of the first day of the cutover week, not null
 * @param dayOfMonthIndicator The day-of-month of the month-day of the cutover week.
 * If positive, it is the start of the week where the cutover can occur.
 * If negative, it represents the end of the week where cutover can occur.
 * The value is the number of days from the end of the month, such that
 * {@code -1} is the last day of the month,  {@code -2} is the second
 * to last day, and so on.
 * @param dayOfWeek the required day-of-week, null if the month-day should not be changed
 * @param time the cutover time in the 'before' offset, not null
 * @param timeEndOfDay whether the time is midnight at the end of day
 * @param timeDefnition how to interpret the cutover
 * @param standardOffset the standard offset in force at the cutover, not null
 * @param offsetBefore the offset before the cutover, not null
 * @param offsetAfter the offset after the cutover, not null
 * @throws IllegalArgumentException if the day of month indicator is invalid
 * @throws IllegalArgumentException if the end of day flag is true when the time is not midnight
 */
@SerialVersionUID(-32352886665458L)
final class ZoneOffsetTransitionRule(val month: MonthOfYear,
                                     val dayOfMonthIndicator: Byte,
                                     val dayOfWeek: DayOfWeek,
                                     val time: LocalTime,
                                     val timeEndOfDay: Boolean,
                                     val timeDefinition: ZoneRulesBuilder.TimeDefinition,
                                     val standardOffset: ZoneOffset,
                                     val offsetBefore: ZoneOffset,
                                     val offsetAfter: ZoneOffset)
  extends Serializable {

  def this(month: MonthOfYear,
           dayOfMonthIndicator: Int,
           dayOfWeek: DayOfWeek,
           time: LocalTime,
           timeEndOfDay: Boolean,
           timeDefinition: ZoneRulesBuilder.TimeDefinition,
           standardOffset: ZoneOffset,
           offsetBefore: ZoneOffset,
           offsetAfter: ZoneOffset) = this (month, dayOfMonthIndicator.toByte, dayOfWeek, time, timeEndOfDay, timeDefinition, standardOffset, offsetBefore, offsetAfter)

  /**
   * Uses a serialization delegate.
   *
   * @return the replacing object, never null
   */
  private def writeReplace: AnyRef = new Ser(Ser.ZOTRule, this)

  /**
   * Gets the month of the transition.
   * <p>
   * If the rule defines an exact date then the month is the month of that date.
   * <p>
   * If the rule defines a week where the transition might occur, then the month
   * if the month of either the earliest or latest possible date of the cutover.
   *
   * @return the month of the transition, never null
   */
  def getMonthOfYear: MonthOfYear = month

  /**
   * Checks if this object equals another.
   * <p>
   * The entire state of the object is compared.
   *
   * @param other the other object to compare to, null returns false
   * @return true if equal
   */
  override def equals(other: Any): Boolean =
    other match {
      case rule: ZoneOffsetTransitionRule => (this eq rule) ||
        (month == rule.month && dayOfMonthIndicator == rule.dayOfMonthIndicator && dayOfWeek == rule.dayOfWeek &&
          timeDefinition == rule.timeDefinition && time == rule.time && timeEndOfDay == rule.timeEndOfDay &&
          standardOffset == rule.standardOffset && offsetBefore == rule.offsetBefore && offsetAfter == rule.offsetAfter)
      case _ => false
    }

  /**
   * Returns a suitable hash code.
   *
   * @return the hash code
   */
  override def hashCode: Int = {
    val hash: Int = ((time.toSecondOfDay + (if (timeEndOfDay) 1 else 0)) << 15) + (month.ordinal << 11) + ((dayOfMonthIndicator + 32) << 5) + ((if (dayOfWeek == null) 7 else dayOfWeek.ordinal) << 2) + (timeDefinition.ordinal)
    return hash ^ standardOffset.hashCode ^ offsetBefore.hashCode ^ offsetAfter.hashCode
  }

  /**
   * Writes the state to the stream.
   * @param out the output stream, not null
   * @throws IOException if an error occurs
   */
  private[zone] def writeExternal(out: DataOutput): Unit = {
    val timeSecs: Int = (if (timeEndOfDay) 86400 else time.toSecondOfDay)
    val stdOffset: Int = standardOffset.getAmountSeconds
    val beforeDiff: Int = offsetBefore.getAmountSeconds - stdOffset
    val afterDiff: Int = offsetAfter.getAmountSeconds - stdOffset
    val timeByte: Int = (if (timeSecs % 3600 == 0) (if (timeEndOfDay) 24 else time.getHourOfDay) else 31)
    val stdOffsetByte: Int = (if (stdOffset % 900 == 0) stdOffset / 900 + 128 else 255)
    val beforeByte: Int = (if (beforeDiff == 0 || beforeDiff == 1800 || beforeDiff == 3600) beforeDiff / 1800 else 3)
    val afterByte: Int = (if (afterDiff == 0 || afterDiff == 1800 || afterDiff == 3600) afterDiff / 1800 else 3)
    val dowByte: Int = (if (dayOfWeek == null) 0 else dayOfWeek.ordinal)
    var b: Int = (month.ordinal << 28) + ((dayOfMonthIndicator + 32) << 22) + (dowByte << 19) + (timeByte << 14) + (timeDefinition.ordinal << 12) + (stdOffsetByte << 4) + (beforeByte << 2) + afterByte
    out.writeInt(b)
    if (timeByte == 31) {
      out.writeInt(timeSecs)
    }
    if (stdOffsetByte == 255) {
      out.writeInt(stdOffset)
    }
    if (beforeByte == 3) {
      out.writeInt(offsetBefore.getAmountSeconds)
    }
    if (afterByte == 3) {
      out.writeInt(offsetAfter.getAmountSeconds)
    }
  }

  /**
   * Creates a transition instance for the specified year.
   * <p>
   * Calculations are performed using the ISO-8601 chronology.
   *
   * @param year the year to create a transition for, not null
   * @return the transition instance, never null
   */
  def createTransition(year: Int): ZoneOffsetTransition = {
    var date: LocalDate = null
    if (dayOfMonthIndicator < 0) {
      date = LocalDate(year, month, month.lastDayOfMonth(ISOChronology.isLeapYear(year)) + 1 + dayOfMonthIndicator)
      if (dayOfWeek != null) {
        date = date.`with`(DateAdjusters.previousOrCurrent(dayOfWeek))
      }
    }
    else {
      date = LocalDate(year, month, dayOfMonthIndicator)
      if (dayOfWeek != null) {
        date = date.`with`(DateAdjusters.nextOrCurrent(dayOfWeek))
      }
    }
    if (timeEndOfDay) {
      date = date.plusDays(1)
    }
    val localDT: LocalDateTime = LocalDateTime.of(date, time)
    val transition: OffsetDateTime = timeDefinition.createDateTime(localDT, standardOffset, offsetBefore)
    new ZoneOffsetTransition(transition, offsetAfter)
  }

  /**
   * Gets the indicator of the day-of-month of the transition.
   * <p>
   * If the rule defines an exact date then the day is the month of that date.
   * <p>
   * If the rule defines a week where the transition might occur, then the day
   * defines either the start of the end of the transition week.
   * <p>
   * If the value is positive, then it represents a normal day-of-month, and is the
   * earliest possible date that the transition can be.
   * The date may refer to 29th February which should be treated as 1st March in non-leap years.
   * <p>
   * If the value is negative, then it represents the number of days back from the
   * end of the month where {@code -1} is the last day of the month.
   * In this case, the day identified is the latest possible date that the transition can be.
   *
   * @return the day-of-month indicator, from -28 to 31 excluding 0
   */
  def getDayOfMonthIndicator: Int = dayOfMonthIndicator

  /**
   * Gets the day-of-week of the transition.
   * <p>
   * If the rule defines an exact date then this returns null.
   * <p>
   * If the rule defines a week where the cutover might occur, then this method
   * returns the day-of-week that the month-day will be adjusted to.
   * If the day is positive then the adjustment is later.
   * If the day is negative then the adjustment is earlier.
   *
   * @return the day-of-week that the transition occurs, null if the rule defines an exact date
   */
  def getDayOfWeek: DayOfWeek = dayOfWeek

  /**
   * Gets the standard offset in force at the transition.
   *
   * @return the standard offset, never null
   */
  def getStandardOffset: ZoneOffset = standardOffset

  /**
   * Gets the offset before the transition.
   *
   * @return the offset before, never null
   */
  def getOffsetBefore: ZoneOffset = offsetBefore

  /**
   * Returns a string describing this object.
   *
   * @return a string for debugging, never null
   */
  override def toString: String = {
    val buf: java.lang.StringBuilder = new java.lang.StringBuilder
    buf.append("TransitionRule[").append(if (offsetBefore.compareTo(offsetAfter) > 0) "Gap " else "Overlap ").append(offsetBefore).append(" to ").append(offsetAfter).append(", ")
    if (dayOfWeek != null) {
      if (dayOfMonthIndicator == -1) {
        buf.append(dayOfWeek.name).append(" on or before last day of ").append(month.name)
      }
      else if (dayOfMonthIndicator < 0) {
        buf.append(dayOfWeek.name).append(" on or before last day minus ").append(-dayOfMonthIndicator - 1).append(" of ").append(month.name)
      }
      else {
        buf.append(dayOfWeek.name).append(" on or after ").append(month.name).append(' ').append(dayOfMonthIndicator)
      }
    }
    else {
      buf.append(month.name).append(' ').append(dayOfMonthIndicator)
    }
    buf.append(" at ").append(if (timeEndOfDay) "24:00" else time.toString).append(" ").append(timeDefinition).append(", standard offset ").append(standardOffset).append(']')
    return buf.toString
  }

  /**
   * Gets the time definition, specifying how to convert the time to an instant.
   * <p>
   * The local time can be converted to an instant using the standard offset,
   * the wall offset or UTC.
   *
   * @return the time definition, never null
   */
  def getTimeDefinition: ZoneRulesBuilder.TimeDefinition = timeDefinition

  /**
   * Is the transition local time midnight at the end of day.
   * <p>
   * The transition may be represented as occurring at 24:00.
   *
   * @return whether a local time of midnight is at the start or end of the day
   */
  def isMidnightEndOfDay: Boolean = timeEndOfDay

  /**
   * Gets the offset after the transition.
   *
   * @return the offset after, never null
   */
  def getOffsetAfter: ZoneOffset = offsetAfter

  /**
   * Gets the local time of day of the transition which must be checked with
   * {@link #isMidnightEndOfDay()}.
   * <p>
   * The time is converted into an instant using the time definition.
   *
   * @return the local time of day of the transition, never null
   */
  def getLocalTime: LocalTime = time
}