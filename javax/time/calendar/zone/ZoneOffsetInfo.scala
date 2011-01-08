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
package javax.time.calendar.zone

import javax.time.calendar.LocalDateTime
import javax.time.calendar.ZoneOffset

/**
 * Information about the valid offsets applicable for a local date-time.
 * <p>
 * The mapping from a local date-time to an offset is not straightforward.
 * There are three cases:
 * <ul>
 * <li>Normal. Where there is a single offset for the local date-time.</li>
 * <li>Gap. Where there is a gap in the local time-line typically caused by the
 * spring cutover to daylight savings. There are no valid offsets within the gap</li>
 * <li>Overlap. Where there is a gap in the local time-line typically caused by the
 * autumn cutover from daylight savings. There are two valid offsets during the overlap.</li>
 * </ul>
 * When using this class, it is vital to check the {@link #isTransition()}
 * method to handle the gap and overlap. Alternatively use one of the general
 * methods {@link #getEstimatedOffset()} or {@link #isValidOffset ( ZoneOffset )}.
 * <p>
 * OffsetInfo is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object ZoneOffsetInfo {
  /**
   * Creates an instance representing a simple single offset or a transition.
   * <p>
   * Applications should normally obtain an instance from {@link ZoneRules}.
   * This constructor is intended for use by implementors of {@code ZoneRules}.
   * <p>
   * One, and only one, of the {@code offset} or {@code transition} parameters must be specified.
   *
   * @param dateTime the local date-time that this info applies to, not null
   * @param offset the offset applicable at the date-time
   * @param transition the details of the transition including the offset before and after
   */
  def of(dateTime: LocalDateTime, offset: ZoneOffset, transition: ZoneOffsetTransition): ZoneOffsetInfo = {
    ZoneRules.checkNotNull(dateTime, "LocalDateTime must not be null")
    if ((offset == null && transition == null) || (offset != null && transition != null)) {
      throw new IllegalArgumentException("One, but not both, of offset or transition must be specified")
    }
    return new ZoneOffsetInfo(dateTime, offset, transition)
  }

  def apply(dateTime: LocalDateTime, offset: ZoneOffset) = {
    ZoneRules.checkNotNull(dateTime, "LocalDateTime must not be null")
    new ZoneOffsetInfo(dateTime, offset, null)
  }

  def apply(dateTime: LocalDateTime, transition: ZoneOffsetTransition) = {
    ZoneRules.checkNotNull(dateTime, "LocalDateTime must not be null")
    new ZoneOffsetInfo(dateTime, null, transition)
  }
}

/**
 * Creates an instance handling a simple single offset.
 *
 * @param dateTime the local date-time that this info applies to, not null
 * @param offset the offset applicable at the date-time, not null
 * @param transition the transition between two offsets on the local time-line.
 */

final class ZoneOffsetInfo private[zone](val dateTime: LocalDateTime, val offset: ZoneOffset, val transition: ZoneOffsetTransition) {

  /**
   * Is a transition occurring on the local time-line.
   * <p>
   * A transition may be a gap or overlap and is normally caused by
   * daylight savings cutover.
   *
   * @return true if there is a transition occurring on the local time-line,
   *  false if there is a single valid offset
   */
  def isTransition: Boolean = transition != null

  /**
   * A suitable hash code for this object.
   *
   * @return the hash code
   */
  override def hashCode: Int = dateTime.hashCode ^ (if (transition != null) transition.hashCode else offset.hashCode)

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
      case info: ZoneOffsetInfo => (this eq info) ||
        (dateTime == info.dateTime && (if (transition != null) transition == info.transition else offset == info.offset))
      case _ => false
    }

  /**
   * Gets the local date-time that this info is applicable to.
   *
   * @return the date-time that this is the information for, not null
   */
  def getLocalDateTime: LocalDateTime = dateTime

  /**
   * Returns a string describing this object.
   *
   * @return a string for debugging, never null
   */
  override def toString: String = {
    val buf: StringBuilder = new StringBuilder
    buf.append("OffsetInfo[").append(dateTime).append(' ').append(if (isTransition) transition else offset).append(']')
    buf.toString
  }

  /**
   * Gets information about the transition occurring on the local time-line.
   * <p>
   * This method is intended for use when {@link #isTransition()} returns {@code true }
   *
   * @return the transition on the local-time line, null if not a transition
   */
  def getTransition: ZoneOffsetTransition = transition

  /**
   * Gets an estimated offset for the local date-time.
   * <p>
   * This returns an offset that applies at the local date-time or just after.
   * During a gap the offset after the gap will be returned.
   * During an overlap the offset after the transition will be returned.
   *
   * @return a suitable estimated offset, never null
   */
  def getEstimatedOffset: ZoneOffset = if (isTransition) getTransition.getOffsetAfter else offset

  /**
   * Checks if the specified offset is valid for this date-time.
   * <p>
   * The date-time will typically have a single valid offset.
   * During a gap, there will be no valid offsets.
   * During an overlap, there will be two valid offsets.
   * This method returns {@code true} if the specified offset is one of the
   * valid offsets.
   *
   * @param offset the offset to check, null returns false
   * @return true if the offset is one of those allowed by the date-time
   */
  def isValidOffset(offset: ZoneOffset): Boolean =
    if (isTransition) transition.isValidOffset(offset)
    else this.offset.equals(offset)


  /**
   * Gets the offset applicable at this point on the local time-line.
   * <p>
   * This method is intended for use when {@link #isTransition()} returns {@code false}.
   *
   * @return the offset applicable when there is not a transition on the
   *  local-time line, null if it is a transition
   */
  def getOffset: ZoneOffset = offset
}