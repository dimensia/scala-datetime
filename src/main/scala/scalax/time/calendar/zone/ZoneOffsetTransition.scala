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
package scalax.time.calendar.zone

import java.io.DataInput
import java.io.DataOutput
import scalax.time.Instant
import scalax.time.calendar.DateTime
import scalax.time.calendar.OffsetDateTime
import scalax.time.calendar.Period
import scalax.time.calendar.ZoneOffset

/**
 * A transition between two offsets caused by a discontinuity in the local time-line.
 * <p>
 * A transition between two offsets is normally the result of a daylight savings cutover.
 * The discontinuity is normally a gap in spring and an overlap in autumn.
 * {@code ZoneOffsetTransition} models the transition between the two offsets.
 * <p>
 * Gaps occur where there are local date-times that simply do not not exist.
 * An example would be when the offset changes from {@code +01:00} to {@code +02:00}.
 * This might be described as 'the clocks will move forward one hour tonight at 1am'.
 * <p>
 * Overlaps occur where there are local date-times that exist twice.
 * An example would be when the offset changes from {@code +02:00} to {@code +01:00}.
 * This might be described as 'the clocks will move back one hour tonight at 2am'.
 * <p>
 * ZoneOffsetTransition is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object ZoneOffsetTransition {
  /**
   * Obtains an instance defining a transition between two offsets.
   * <p>
   * Applications should normally obtain an instance from {@link ZoneRules}.
   * This constructor is intended for use by implementors of {@code ZoneRules}.
   *
   * @param transition the transition date-time with the offset before the transition, not null
   * @param offsetAfter the offset at and after the transition, not null
   */
  def of(transition: OffsetDateTime, offsetAfter: ZoneOffset): ZoneOffsetTransition = {
    ZoneRules.checkNotNull(transition, "OffsetDateTime must not be null")
    ZoneRules.checkNotNull(transition, "ZoneOffset must not be null")
    if (transition.getOffset.equals(offsetAfter)) {
      throw new IllegalArgumentException("Offsets must not be equal")
    }
    return new ZoneOffsetTransition(transition, offsetAfter)
  }

  /**
   * Reads the state from the stream.
   * @param in the input stream, not null
   * @return the created object, never null
   * @throws IOException if an error occurs
   */
  private[zone] def readExternal(in: DataInput): ZoneOffsetTransition = {
    var epochSeconds: Long = Ser.readEpochSecs(in)
    var before: ZoneOffset = Ser.readOffset(in)
    var after: ZoneOffset = Ser.readOffset(in)
    return ZoneOffsetTransition.of(OffsetDateTime.ofEpochSeconds(epochSeconds, before), after)
  }
}

/**
 * Creates an instance defining a transition between two offsets.
 *
 * @param transition the transition date-time with the offset before the transition, not null
 * @param offsetAfter the offset at and after the transition, not null
 */
@SerialVersionUID(1L)
final class ZoneOffsetTransition private[zone](val transition: OffsetDateTime, offsetAfter: ZoneOffset)
  extends Ordered[ZoneOffsetTransition] with Serializable {


  /**
   * The transition date-time with the offset after the transition.
   */
  private lazy val transitionAfter: OffsetDateTime = transition.withOffsetSameInstant(offsetAfter)
  /**
   * Returns a string describing this object.
   *
   * @return a string for debugging, never null
   */
  override def toString: String = {
    val buf: StringBuilder = new StringBuilder
    buf.append("Transition[").append(if (isGap) "Gap" else "Overlap").append(" at ").append(transition).append(" to ").append(transitionAfter.getOffset).append(']')
    buf.toString
  }

  /**
   * Compares this transition to another based on the transition instant.
   * <p>
   * This compares the instants of each transition.
   * The offsets are ignored, making this order inconsistent with equals.
   *
   * @param transition  the transition to compare to, not null
   * @return the comparator value, negative if less, positive if greater
   */
  def compare(transition: ZoneOffsetTransition): Int = this.getInstant.compareTo(transition.getInstant)

  /**
   * Writes the state to the stream.
   * @param out the output stream, not null
   * @throws IOException if an error occurs
   */
  private[zone] def writeExternal(out: DataOutput): Unit = {
    Ser.writeEpochSecs(transition.toEpochSeconds, out)
    Ser.writeOffset(transition.getOffset, out)
    Ser.writeOffset(transitionAfter.getOffset, out)
  }

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
      case zot: ZoneOffsetTransition => (this eq zot) ||
        (transition == zot.transition && transitionAfter.getOffset == zot.transitionAfter.getOffset)
      case _ => false
    }

  /**
   * Gets the transition instant.
   * <p>
   * This is the instant of the discontinuity, which is defined as the first
   * instant that the 'after' offset applies.
   *  <p>
   * The methods {@link #getInstant()}, {@link #getDateTimeBefore()} and {@link #getDateTimeAfter()}
   * all represent the same instant.
   *
   * @return the transition instant, not null
   */
  def getInstant: Instant = transition.toInstant

  /**
   * Uses a serialization delegate.
   *
   * @return the replacing object, never null
   */
  private def writeReplace: AnyRef = new Ser(Ser.ZOT, this)

  /**
   * Gets the transition date-time expressed with the 'after' offset.
   * <p>
   * This is the first date-time after the discontinuity, when the new offset applies.
   * <p>
   * The methods {@link #getInstant()}, {@link #getDateTimeBefore()} and {@link #getDateTimeAfter()}
   * all represent the same instant.
   *
   * @return the transition date-time expressed with the after offset, not null
   */
  def getDateTimeAfter: OffsetDateTime = transitionAfter

  /**
   * Does this transition represent a gap in the local time-line.
   * <p>
   * Overlaps occur where there are local date-times that exist twice.
   * An example would be when the offset changes from {@code +02:00} to {@code +01:00}.
   * This might be described as 'the clocks will move back one hour tonight at 2am'.
   *
   * @return true if this transition is an overlap, false if it is a gap
   */
  def isOverlap: Boolean = getOffsetAfter.getAmountSeconds < getOffsetBefore.getAmountSeconds

  /**
   * Gets the local date-time at the transition which is expressed relative to
   * the 'before' offset.
   * <p>
   * This is the date-time where the discontinuity begins.
   * For a gap, this local date-time never occurs, whereas for an overlap it occurs
   * just once after the entire transition is complete.
   * This method is simply {@code getDateTimeBefore().toLocalDateTime()}
   * <p>
   * This value expresses the date-time normally used in verbal communications.
   * For example 'the clocks will move forward one hour tonight at 1am' (a gap) or
   * 'the clocks will move back one hour tonight at 2am' (an overlap).
   *
   * @return the local date-time of the transition, expressed relative to the before offset, not null
   */
  def getLocal: DateTime = transition.toLocalDateTime

  /**
   * Gets the offset before the transition.
   * <p>
   * This is the offset in use before the instant of the transition.
   *
   * @return the offset before the transition, not null
   */
  def getOffsetBefore: ZoneOffset = transition.getOffset

  /**
   * Gets the offset after the transition.
   * <p>
   * This is the offset in use on and after the instant of the transition.
   *
   * @return the offset after the transition, not null
   */

  def getOffsetAfter: ZoneOffset = transitionAfter.getOffset

  /**
   * Gets the length of the transition as a {@code Period}.
   * <p>
   * This will typically be one hour, but might not be.
   * It will be positive for a gap and negative for an overlap.
   *
   * @return the length of the transition, positive for gaps, negative for overlaps
   */
  def getTransitionSize: Period = {
    val secs: Int = getOffsetAfter.getAmountSeconds - getOffsetBefore.getAmountSeconds
    Period.ofSeconds(secs).normalized
  }

  /**
   * Checks if the specified offset is valid during this transition.
   * <p>
   * This checks to see if the given offset will be valid at some point in the transition.
   * A gap will always return false.
   * An overlap will return true if the offset is either the before or after offset.
   *
   * @param offset  the offset to check, null returns false
   * @return true if the offset is valid during the transition
   */
  def isValidOffset(offset: ZoneOffset): Boolean =
    if (isGap) false
    else (getOffsetBefore.equals(offset) || getOffsetAfter.equals(offset))

  /**
   * Gets the transition instant date-time expressed with the 'before' offset.
   * <p>
   * This is the date-time where the discontinuity begins, and as such it never
   * actually occurs (as the 'after' offset is actually used at this instant).
   * This is the same instant as {@link #getDateTimeAfter()} but with the 'before' offset.
   *
   * @return the transition date-time expressed with the before offset, not null
   */
  def getDateTimeBefore: OffsetDateTime = transition

  /**
   * Does this transition represent a gap in the local time-line.
   * <p>
   * Gaps occur where there are local date-times that simply do not not exist.
   * An example would be when the offset changes from {@code +01:00} to {@code +02:00}.
   * This might be described as 'the clocks will move forward one hour tonight at 1am'.
   *
   * @return true if this transition is a gap, false if it is an overlap
   */
  def isGap: Boolean = getOffsetAfter.getAmountSeconds > getOffsetBefore.getAmountSeconds

  /**
   * Returns a suitable hash code.
   *
   * @return the hash code
   */
  override def hashCode: Int = transition.hashCode ^ transitionAfter.getOffset.hashCode
}