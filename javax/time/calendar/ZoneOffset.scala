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
import java.util.HashMap
import java.util.Map
import java.util.concurrent.locks.ReadWriteLock
import java.util.concurrent.locks.ReentrantReadWriteLock
import javax.time.CalendricalException

/**
 * A time-zone offset from UTC, such as '+02:00'.
 * <p>
 * A time-zone offset is the period of time that a time-zone differs from UTC.
 * This is usually a fixed number of hours and minutes.
 * <p>
 * Different parts of the world have different time-zone offsets.
 * The rules for how offsets vary by place and time of year are captured in the
 * { @link TimeZone } class.
 * <p>
 * For example, Paris is one hours ahead of UTC in winter and two hours ahead in
 * summer. The    { @code TimeZone } instance for Paris will reference two
 * { @code ZoneOffset } instances - a    { @code +01:00 } instance for winter,
 * and a    { @code +02:00 } instance for summer.
 * <p>
 * In 2008, time-zone offsets around the world extended from -12:00 to +14:00.
 * To prevent any problems with that range being extended, yet still provide
 * validation, the range of offsets is restricted to -18:00 to 18:00 inclusive.
 * <p>
 * This class is designed primarily for use with the    { @link ISOChronology }.
 * The fields of hours, minutes and seconds make assumptions that are valid for the
 * standard ISO definitions of those fields. This class may be used with other
 * calendar systems providing the definition of the time fields matches those
 * of the ISO calendar system.
 * <p>
 * Instances of ZoneOffset must be compared using    { @link # equals }.
 * Implementations may choose to cache certain common offsets, however
 * applications must not rely on such caching.
 * <p>
 * ZoneOffset is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object ZoneOffset {
  /**Cache of time-zone offset by id. */
  private val ID_CACHE: Map[String, ZoneOffset] = new HashMap[String, ZoneOffset]
  /**
   * Parse a two digit zero-prefixed number.
   *
   * @param offsetID the offset id, not null
   * @param pos the position to parse, valid
   * @param precededByColon should this number be prefixed by a precededByColon
   * @return the parsed number, from 0 to 99
   */
  private def parseNumber(offsetID: String, pos: Int, precededByColon: Boolean): Int = {
    if (precededByColon && offsetID.charAt(pos - 1) != ':') {
      throw new IllegalArgumentException("Zone offset id '" + offsetID + "' is invalid: Colon not found when expected")
    }
    var ch1: Char = offsetID.charAt(pos)
    var ch2: Char = offsetID.charAt(pos + 1)
    if (ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9') {
      throw new IllegalArgumentException("Zone offset id '" + offsetID + "' is invalid: Non numeric characters found")
    }
    return (ch1 - 48) * 10 + (ch2 - 48)
  }

  /**
   * The number of seconds per minute.
   */
  private val SECONDS_PER_MINUTE: Int = 60
  /**
   * Obtains an instance of    { @code ZoneOffset } using an offset in hours.
   *
   * @param hours the time-zone offset in hours, from -18 to +18
   * @return the ZoneOffset, never null
   * @throws IllegalArgumentException if the offset is not in the required range
   */
  def ofHours(hours: Int): ZoneOffset = ofHoursMinutesSeconds(hours, 0, 0)

  /**
   * Obtains an instance of    { @code ZoneOffset } specifying the total offset in seconds
   * <p>
   * The offset must be in the range    { @code -18:00 } to    { @code +18:00 }, which corresponds to -64800 to +64800.
   *
   * @param totalSeconds the total time-zone offset in seconds, from -64800 to +64800
   * @return the ZoneOffset, never null
   * @throws IllegalArgumentException if the offset is not in the required range
   */
  def ofTotalSeconds(totalSeconds: Int): ZoneOffset = {
    if (Math.abs(totalSeconds) > (18 * SECONDS_PER_HOUR)) {
      throw new IllegalArgumentException("Zone offset not in valid range: -18:00 to +18:00")
    }
    if (totalSeconds % (15 * SECONDS_PER_MINUTE) == 0) {
      var totalSecs: Integer = totalSeconds
      CACHE_LOCK.readLock.lock
      try {
        var result: ZoneOffset = SECONDS_CACHE.get(totalSecs)
        if (result != null) {
          return result
        }
      }
      finally {
        CACHE_LOCK.readLock.unlock
      }
      CACHE_LOCK.writeLock.lock
      try {
        var result: ZoneOffset = SECONDS_CACHE.get(totalSecs)
        if (result == null) {
          result = new ZoneOffset(totalSeconds)
          SECONDS_CACHE.put(totalSecs, result)
          ID_CACHE.put(result.getID, result)
        }
        return result
      }
      finally {
        CACHE_LOCK.writeLock.unlock
      }
    }
    else {
      return new ZoneOffset(totalSeconds)
    }
  }

  /**
   * Gets the field rule for the zone-offset.
   *
   * @return the field rule for the zone-offset, never null
   */
  def rule: CalendricalRule[ZoneOffset] = Rule.INSTANCE

  /**
   * The number of minutes per hour.
   */
  private val MINUTES_PER_HOUR: Int = 60
  /**
   * Calculates the total offset in seconds.
   *
   * @param hours the time-zone offset in hours, from -18 to +18
   * @param minutes the time-zone offset in minutes, from 0 to &plusmn;59, sign matches hours and seconds
   * @param seconds the time-zone offset in seconds, from 0 to &plusmn;59, sign matches hours and minutes
   * @return the total in seconds
   */
  private def totalSeconds(hours: Int, minutes: Int, seconds: Int): Int = {
    hours * SECONDS_PER_HOUR + minutes * SECONDS_PER_MINUTE + seconds
  }

  /**
   * The time-zone offset for UTC, with an id of 'Z'.
   */
  val UTC: ZoneOffset = ofHoursMinutesSeconds(0, 0, 0)
  /**
   * The number of seconds per hour.
   */
  private val SECONDS_PER_HOUR: Int = 60 * 60

  /**
   * Validates the offset fields.
   *
   * @param hours the time-zone offset in hours, from -18 to +18
   * @param minutes the time-zone offset in minutes, from 0 to &plusmn;59
   * @param seconds the time-zone offset in seconds, from 0 to &plusmn;59
   * @throws IllegalArgumentException if the offset is not in the required range
   */
  private def validate(hours: Int, minutes: Int, seconds: Int): Unit = {
    if (hours < -18 || hours > 18) {
      throw new IllegalArgumentException("Zone offset hours not in valid range: value " + hours + " is not in the range -18 to 18")
    }
    if (hours > 0) {
      if (minutes < 0 || seconds < 0) {
        throw new IllegalArgumentException("Zone offset minutes and seconds must be positive because hours is positive")
      }
    }
    else if (hours < 0) {
      if (minutes > 0 || seconds > 0) {
        throw new IllegalArgumentException("Zone offset minutes and seconds must be negative because hours is negative")
      }
    }
    else if ((minutes > 0 && seconds < 0) || (minutes < 0 && seconds > 0)) {
      throw new IllegalArgumentException("Zone offset minutes and seconds must have the same sign")
    }
    if (Math.abs(minutes) > 59) {
      throw new IllegalArgumentException("Zone offset minutes not in valid range: value " + Math.abs(minutes) + " is not in the range 0 to 59")
    }
    if (Math.abs(seconds) > 59) {
      throw new IllegalArgumentException("Zone offset seconds not in valid range: value " + Math.abs(seconds) + " is not in the range 0 to 59")
    }
    if (Math.abs(hours) == 18 && (Math.abs(minutes) > 0 || Math.abs(seconds) > 0)) {
      throw new IllegalArgumentException("Zone offset not in valid range: -18:00 to +18:00")
    }
  }

  /**
   * Rule implementation.
   */
  private[calendar] object Rule extends Rule

  @SerialVersionUID(1L)
  private[calendar] final class Rule private
    extends CalendricalRule[ZoneOffset](classOf[ZoneOffset], ISOChronology.INSTANCE, "ZoneOffset", null, null)
    with Serializable {
    private def readResolve: AnyRef = Rule

    protected override def derive(calendrical: Calendrical): ZoneOffset = {
      var odt: OffsetDateTime = calendrical.get(OffsetDateTime.rule)
      if (odt != null) {
        return odt.getOffset
      }
      var od: OffsetDate = calendrical.get(OffsetDate.rule)
      if (od != null) {
        return od.getOffset
      }
      var ot: OffsetTime = calendrical.get(OffsetTime.rule)
      if (ot != null) {
        return ot.getOffset
      }
      return null
    }

    /**
     * Obtains an instance of    { @code ZoneOffset } using an offset in
     * hours, minutes and seconds.
     * <p>
     * The sign of the hours, minutes and seconds components must match.
     * Thus, if the hours is negative, the minutes and seconds must be negative or zero.
     *
     * @param hours the time-zone offset in hours, from -18 to +18
     * @param minutes the time-zone offset in minutes, from 0 to &plusmn;59, sign matches hours and seconds
     * @param seconds the time-zone offset in seconds, from 0 to &plusmn;59, sign matches hours and minutes
     * @return the ZoneOffset, never null
     * @throws IllegalArgumentException if the offset is not in the required range
     */
    def ofHoursMinutesSeconds(hours: Int, minutes: Int, seconds: Int): ZoneOffset = {
      validate(hours, minutes, seconds)
      val totalSecs: Int = totalSeconds(hours, minutes, seconds)
      return ofTotalSeconds(totalSecs)
    }

    /**
     * Obtains an instance of    { @code ZoneOffset } using the id.
     * <p>
     * This method parses the string id of a    { @code ZoneOffset } to
     * return an instance. The parsing accepts all the formats generated by
     * { @link # getID ( ) }, plus some additional formats:
     * <ul>
     * <li>   { @code Z } - for UTC
     * <li>   { @code +hh:mm }
     * <li>   { @code -hh:mm }
     * <li>   { @code +hhmm }
     * <li>   { @code -hhmm }
     * <li>   { @code +hh:mm:ss }
     * <li>   { @code -hh:mm:ss }
     * <li>   { @code +hhmmss }
     * <li>   { @code -hhmmss }
     * </ul>
     * Note that &plusmn; means either the plus or minus symbol.
     * <p>
     * The ID of the returned offset will be normalized to one of the formats
     * described by    { @link # getID ( ) }.
     * <p>
     * The maximum supported range is from +18:00 to -18:00 inclusive.
     *
     * @param offsetID the offset id, not null
     * @return the ZoneOffset, never null
     * @throws IllegalArgumentException if the offset id is invalid
     */
    def of(offsetID: String): ZoneOffset = {
      if (offsetID == null) {
        throw new NullPointerException("The offset ID must not be null")
      }
      CACHE_LOCK.readLock.lock
      try {
        var offset: ZoneOffset = ID_CACHE.get(offsetID)
        if (offset != null) {
          return offset
        }
      }
      finally {
        CACHE_LOCK.readLock.unlock
      }
      var hours: Int = 0
      var minutes: Int = 0
      var seconds: Int = 0
      var len: Int = offsetID.length
      len match {
        case 3 =>
          hours = parseNumber(offsetID, 1, false)
          minutes = 0
          seconds = 0
        case 5 =>
          hours = parseNumber(offsetID, 1, false)
          minutes = parseNumber(offsetID, 3, false)
          seconds = 0
        case 6 =>
          hours = parseNumber(offsetID, 1, false)
          minutes = parseNumber(offsetID, 4, true)
          seconds = 0
        case 7 =>
          hours = parseNumber(offsetID, 1, false)
          minutes = parseNumber(offsetID, 3, false)
          seconds = parseNumber(offsetID, 5, false)
        case 9 =>
          hours = parseNumber(offsetID, 1, false)
          minutes = parseNumber(offsetID, 4, true)
          seconds = parseNumber(offsetID, 7, true)
        case _ =>
          throw new IllegalArgumentException("Zone offset id '" + offsetID + "' is invalid")
      }
      var first: Char = offsetID.charAt(0)
      if (first != '+' && first != '-') {
        throw new IllegalArgumentException("Zone offset id '" + offsetID + "' is invalid: Plus/minus not found when expected")
      }
      if (first == '-') {
        return ofHoursMinutesSeconds(-hours, -minutes, -seconds)
      }
      else {
        return ofHoursMinutesSeconds(hours, minutes, seconds)
      }
    }

    /**
     * Obtains an instance of    { @code ZoneOffset } using an offset in
     * hours and minutes.
     * <p>
     * The sign of the hours and minutes components must match.
     * Thus, if the hours is negative, the minutes must be negative or zero.
     * If the hours is zero, the minutes may be positive, negative or zero.
     *
     * @param hours the time-zone offset in hours, from -18 to +18
     * @param minutes the time-zone offset in minutes, from 0 to &plusmn;59, sign matches hours
     * @return the ZoneOffset, never null
     * @throws IllegalArgumentException if the offset is not in the required range
     */
    def ofHoursMinutes(hours: Int, minutes: Int): ZoneOffset = {
      return ofHoursMinutesSeconds(hours, minutes, 0)
    }

    /**Cache of time-zone offset by offset in seconds. */
    private val SECONDS_CACHE: Map[Int, ZoneOffset] = new HashMap[Int, ZoneOffset]
    /**
     * Obtains an instance of    { @code ZoneOffset } from a period.
     * <p>
     * This creates an offset from the specified period, converting using
     * { @link Period # of ( PeriodProvider ) }.
     * Only the hour, minute and second fields from the period are used - other fields are ignored.
     * The sign of the hours, minutes and seconds components must match.
     * Thus, if the hours is negative, the minutes and seconds must be negative or zero.
     *
     * @param periodProvider the period to use, not null
     * @return the ZoneOffset, never null
     * @throws CalendricalException if the specified period cannot be converted to a    { @code Period }
     * @throws IllegalArgumentException if the offset is not in the required range
     */
    def of(periodProvider: PeriodProvider): ZoneOffset = {
      var period: Period = Period.of(periodProvider)
      return ofHoursMinutesSeconds(period.getHours, period.getMinutes, period.getSeconds)
    }

    /**Cache of time-zone offset by offset in seconds. */
    private val CACHE_LOCK: ReadWriteLock = new ReentrantReadWriteLock
  }

}

/**
 * Constructor.
 *
 * @param amountSeconds the total time-zone offset in seconds, from -64800 to +64800
 */
@SerialVersionUID(1L)
final class ZoneOffset private(amountSeconds: Int) extends Calendrical with Comparable[ZoneOffset] with Serializable {

  /**
   * The string form of the time-zone offset.
   */
  @transient
  private lazy val id: String = {
    if (amountSeconds == 0) "Z"
    else {
      var absTotalSeconds: Int = math.abs(amountSeconds)
      var buf: StringBuilder = new StringBuilder
      var absHours: Int = absTotalSeconds / SECONDS_PER_HOUR
      var absMinutes: Int = (absTotalSeconds / SECONDS_PER_MINUTE) % MINUTES_PER_HOUR
      buf.append(if (amountSeconds < 0) "-" else "+")
        .append(if (absHours < 10) "0" else "")
        .append(absHours)
        .append(if (absMinutes < 10) ":0" else ":")
        .append(absMinutes)
      var absSeconds: Int = absTotalSeconds % SECONDS_PER_MINUTE
      if (absSeconds != 0) {
        buf.append(if (absSeconds < 10) ":0" else ":").append(absSeconds)
      }
      buf.toString
    }
  }

  /**
   * Gets the value of the specified calendrical rule.
   * <p>
   * This method queries the value of the specified calendrical rule.
   * If the value cannot be returned for the rule from this offset then
   * { @code null } will be returned.
   *
   * @param rule the rule to use, not null
   * @return the value for the rule, null if the value cannot be returned
   */
  def get[T](rule: CalendricalRule[T]): T = rule.deriveValueFor(rule, this, this)

  /**
   * Gets the total zone offset in seconds.
   * <p>
   * This is the primary way to access the offset amount.
   * It returns the total of the hours, minutes and seconds fields as a
   * single offset that can be added to a time.
   *
   * @return the total zone offset amount in seconds
   */
  def getAmountSeconds: Int = amountSeconds

  /**
   * Converts this offset to a time-zone.
   * <p>
   * The returned time-zone will use this offset for all instants.
   *
   * @return the time-zone, never null
   */
  def toTimeZone: TimeZone = TimeZone.of(this)

  /**
   * Gets the hours field of the zone offset.
   * <p>
   * This method only has meaning when considered with the minutes and seconds
   * fields. Most applications are advised to use    { @link # toPeriod ( ) }
   * or    { @link # getAmountSeconds ( ) }.
   * <p>
   * The zone offset is divided into three fields - hours, minutes and seconds.
   * This method returns the value of the hours field.
   * The sign of the value returned by this method will match that of the
   * minutes and seconds fields.
   *
   * @return the hours field of the zone offset amount, from -18 to 18
   */
  def getHoursField: Int = amountSeconds / SECONDS_PER_HOUR

  /**
   * Resolves singletons.
   *
   * @return the singleton instance
   */
  private def readResolve: AnyRef = ZoneOffset.ofTotalSeconds(amountSeconds)

  /**
   * Gets the minutes field of the zone offset.
   * <p>
   * This method only has meaning when considered with the hours and minutes
   * fields. Most applications are advised to use    { @link # toPeriod ( ) }
   * or    { @link # getAmountSeconds ( ) }.
   * <p>
   * The zone offset is divided into three fields - hours, minutes and seconds.
   * This method returns the value of the minutes field.
   * The sign of the value returned by this method will match that of the
   * hours and seconds fields.
   *
   * @return the minutes field of the zone offset amount,
   *      from -59 to 59 where the sign matches the hours and seconds
   */
  def getMinutesField: Int = (amountSeconds / SECONDS_PER_MINUTE) % MINUTES_PER_HOUR

  /**
   * Returns a string representation of the zone offset, which is the same
   * as the normalized id.
   *
   * @return the id
   */
  override def toString: String = id

  /**
   * Gets the normalized zone offset id.
   * <p>
   * The id is minor variation to the standard ISO-8601 formatted string
   * for the offset. There are three formats:
   * <ul>
   * <li>   { @code Z } - for UTC (ISO-8601)
   * <li>   { @code +hh:mm } or    { @code -hh:mm } - if the seconds are zero (ISO-8601)
   * <li>   { @code +hh:mm:ss } or    { @code -hh:mm:ss } - if the seconds are non-zero (not ISO-8601)
   * </ul>
   *
   * @return the zone offset ID, never null
   */
  def getID: String = id

  /**
   * A hash code for the zone offset.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = amountSeconds

  /**
   * Gets the seconds field of the zone offset.
   * <p>
   * This method only has meaning when considered with the hours and minutes
   * fields. Most applications are advised to use    { @link # toPeriod ( ) }
   * or    { @link # getAmountSeconds ( ) }.
   * <p>
   * The zone offset is divided into three fields - hours, minutes and seconds.
   * This method returns the value of the seconds field.
   * The sign of the value returned by this method will match that of the
   * hours and minutes fields.
   *
   * @return the seconds field of the zone offset amount,
   *      from -59 to 59 where the sign matches the hours and minutes
   */
  def getSecondsField: Int = amountSeconds % SECONDS_PER_MINUTE

  /**
   * Converts this offset to a period.
   * <p>
   * The period returned will have fields for hour, minute and second.
   * For negative offsets, the values in the period will all be negative.
   * <p>
   * For example,    { @code +02:45 } will be converted to    { @code P2H45M },
   * while    { @code -01:15 } will be converted to    { @code P -1H-15M }.
   *
   * @return the period equivalent to the zone offset amount, never null
   */
  def toPeriod: Period = Period.ofTimeFields(getHoursField, getMinutesField, getSecondsField)

  /**
   * Compares this offset to another offset in descending order.
   * <p>
   * The offsets are compared in the order that they occur for the same time
   * of day around the world. Thus, an offset of    { @code +10:00 } comes before an
   * offset of    { @code +09:00 } and so on down to    { @code -18:00 }.
   *
   * @param other the other date to compare to, not null
   * @return the comparator value, negative if less, postive if greater
   * @throws NullPointerException if    { @code other } is null
   */
  def compareTo(other: ZoneOffset): Int = other.amountSeconds - amountSeconds

  /**
   * Checks if this instance is equal to the specified offset, comparing
   * the amount of the offset in seconds.
   *
   * @param other the other zone offset, null returns false
   * @return true if this offset is the same as that specified
   */
  override def equals(other: AnyRef): Boolean = {
    if (this == other) true
    else if (other.isInstanceOf[ZoneOffset]) amountSeconds == (other.asInstanceOf[ZoneOffset]).amountSeconds
    else false
  }

  /**
   * Returns a copy of this offset with the specified period added.
   * <p>
   * This adds the amount in hours, minutes and seconds from the specified period to this offset.
   * This converts the period using    { @link Period # of ( PeriodProvider ) }.
   * Only the hour, minute and second fields from the period are used - other fields are ignored.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to add, not null
   * @return a { @code ZoneOffset } based on this offset with the period added, never null
   * @throws CalendricalException if the specified period cannot be converted to a    { @code Period }
   * @throws IllegalArgumentException if the offset is not in the required range
   */
  def plus(periodProvider: PeriodProvider): ZoneOffset = {
    var otherPeriod: Period = Period.of(periodProvider).withTimeFieldsOnly.withNanos(0)
    var thisPeriod: Period = toPeriod
    var combined: Period = thisPeriod.plus(otherPeriod).normalized
    return of(combined)
  }
}