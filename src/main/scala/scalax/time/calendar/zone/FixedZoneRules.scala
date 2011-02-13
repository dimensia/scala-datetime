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
package scalax.time.calendar.zone

import java.io.DataInput
import java.io.DataOutput

import collection.mutable.ArrayBuffer

import scalax.time.InstantProvider
import scalax.time.calendar.DateTime
import scalax.time.calendar.ZoneOffset

/**
 * Implementation of zone rules for fixed offsets.
 *
 * @author Stephen Colebourne
 */
object FixedZoneRules {
  /**
   * Reads the state from the stream.
   * @param in the input stream, not null
   * @return the created object, never null
   * @throws IOException if an error occurs
   */
  private[zone] def readExternal(in: DataInput): FixedZoneRules = {
    val offset: ZoneOffset = Ser.readOffset(in)
    return new FixedZoneRules(offset)
  }
}


/**
 * Constructor.
 *
 * @param offset the fixed zone offset, not null
 */
@SerialVersionUID(1L)
final class FixedZoneRules private[zone](val offset: ZoneOffset) extends ZoneRules with Serializable {
  ZoneRules.checkNotNull(offset, "ZoneOffset must not be null")

  /**
   * Writes the state to the stream.
   * @param out the output stream, not null
   * @throws IOException if an error occurs
   */
  private[zone] def writeExternal(out: DataOutput): Unit = Ser.writeOffset(offset, out)

  /**{@inheritDoc}*/
  def getStandardOffset(instant: InstantProvider): ZoneOffset = offset

  /**{@inheritDoc}*/
  override def isFixedOffset: Boolean = true

  /**
   * Uses a serialization delegate.
   *
   * @return the replacing object, never null
   */
  private def writeReplace: AnyRef = new Ser(Ser.FZR, this)

  /**{@inheritDoc}*/
  def getOffset(instant: InstantProvider): ZoneOffset = offset

  /**{@inheritDoc}*/
  def previousTransition(instantProvider: InstantProvider): ZoneOffsetTransition = null

  /**{@inheritDoc}*/
  def getTransitionRules: Seq[ZoneOffsetTransitionRule] = new ArrayBuffer[ZoneOffsetTransitionRule]

  /**{@inheritDoc}*/
  def getOffsetInfo(dateTime: DateTime): ZoneOffsetInfo = new ZoneOffsetInfo(dateTime, offset, null)

  /**{@inheritDoc}*/
  def getTransitions: Seq[ZoneOffsetTransition] = new ArrayBuffer[ZoneOffsetTransition]

  /**
   * Returns a string describing this object.
   *
   * @return a string for debugging, never null
   */
  override def toString: String = if (offset == ZoneOffset.UTC) "UTC" else "UTC" + offset.getID

  /**
   * Returns a suitable hash code.
   *
   * @return the hash code
   */
  override def hashCode: Int = offset.hashCode

  /**
   * Checks if this object equals another, comparing the offset.
   * <p>
   * The entire state of the object is compared.
   *
   * @param other the other object to compare to, null returns false
   * @return true if equal
   */
  override def equals(other: Any): Boolean =
    other match {
      case rules: FixedZoneRules => (this eq rules) ||
        (offset == rules.offset)
      case _ => false
    }

  /**{@inheritDoc}*/
  def nextTransition(instantProvider: InstantProvider): ZoneOffsetTransition = null
}