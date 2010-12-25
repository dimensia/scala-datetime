/*
 * Copyright (c) 2010, Stephen Colebourne & Michael Nascimento Santos
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
import java.io.Externalizable
import java.io.InvalidClassException
import java.io.ObjectInput
import java.io.ObjectOutput
import java.io.StreamCorruptedException
import javax.time.calendar.ZoneOffset

/**
 * The shared serialization delegate for this package.
 *
 * @author Stephen Colebourne
 */
object Ser {
  private def readInternal(tpe: Byte, in: DataInput): AnyRef = {
    tpe match {
      case FZR => return FixedZoneRules.readExternal(in)
      case SZR => return StandardZoneRules.readExternal(in)
      case ZOT => return ZoneOffsetTransition.readExternal(in)
      case ZOTRule => return ZoneOffsetTransitionRule.readExternal(in)
      case _ => throw new StreamCorruptedException("Unknown serialized type")
    }
  }

  /**Type for FixedZoneRules. */
  private[zone] val FZR: Byte = 0
  /**Type for StandardZoneRules. */
  private[zone] val SZR: Byte = 1
  /**Type for ZoneOffsetTransition. */
  private[zone] val ZOT: Byte = 2
  /**Type for ZoneOffsetTransition. */
  private[zone] val ZOTRule: Byte = 3

  private[zone] def read(in: DataInput): AnyRef = {
    var _type: Byte = in.readByte
    return readInternal(_type, in)
  }

  /**
   * Writes the state to the stream.
   * @param offset the offset, not null
   * @param out the output stream, not null
   * @throws IOException if an error occurs
   */
  private[zone] def writeOffset(offset: ZoneOffset, out: DataOutput): Unit = {
    val offsetSecs: Int = offset.getAmountSeconds
    var offsetByte: Int = if (offsetSecs % 900 == 0) offsetSecs / 900 else 127
    out.writeByte(offsetByte)
    if (offsetByte == 127) {
      out.writeInt(offsetSecs)
    }
  }

  /**
   * Reads the state from the stream.
   * @param in the input stream, not null
   * @return the created object, never null
   * @throws IOException if an error occurs
   */
  private[zone] def readOffset(in: DataInput): ZoneOffset = {
    var offsetByte: Int = in.readByte
    return (if (offsetByte == 127) ZoneOffset.ofTotalSeconds(in.readInt) else ZoneOffset.ofTotalSeconds(offsetByte * 900))
  }

  /**
   * Writes the state to the stream.
   * @param epochSecs the epoch seconds, not null
   * @param out the output stream, not null
   * @throws IOException if an error occurs
   */
  private[zone] def writeEpochSecs(epochSecs: Long, out: DataOutput): Unit = {
    if (epochSecs >= -4575744000L && epochSecs < 10413792000L && epochSecs % 900 == 0) {
      var store: Int = ((epochSecs + 4575744000L) / 900).toInt
      out.writeByte((store >>> 16) & 255)
      out.writeByte((store >>> 8) & 255)
      out.writeByte(store & 255)
    }
    else {
      out.writeByte(255)
      out.writeLong(epochSecs)
    }
  }

  private[zone] def write(obj: AnyRef, out: DataOutput): Unit = {
    if (obj.isInstanceOf[StandardZoneRules]) {
      writeInternal(SZR, obj, out)
    }
    else {
      writeInternal(FZR, obj, out)
    }
  }


  private def writeInternal(tpe: Byte, obj: AnyRef, out: DataOutput): Unit = {
    out.writeByte(tpe)
    tpe match {
      case FZR => (obj.asInstanceOf[FixedZoneRules]).writeExternal(out)
      case SZR => (obj.asInstanceOf[StandardZoneRules]).writeExternal(out)
      case ZOT => (obj.asInstanceOf[ZoneOffsetTransition]).writeExternal(out)
      case ZOTRule => (obj.asInstanceOf[ZoneOffsetTransitionRule]).writeExternal(out)
      case _ => throw new InvalidClassException("Unknown serialized type")
    }
  }

  /**
   * Reads the state from the stream.
   * @param in the input stream, not null
   * @return the epoch seconds, never null
   * @throws IOException if an error occurs
   */
  private[zone] def readEpochSecs(in: DataInput): Long = {
    var hiByte: Int = in.readByte & 255
    if (hiByte == 255) {
      return in.readLong
    }
    else {
      var midByte: Int = in.readByte & 255
      var loByte: Int = in.readByte & 255
      var tot: Long = ((hiByte << 16) + (midByte << 8) + loByte)
      return (tot * 900) - 4575744000L
    }
  }
}

/**
 * Constructor.
 *
 * @param type the type
 * @param object the object
 */
final class Ser private[zone](private var tpe: Byte, private var obj: AnyRef) extends Externalizable {

  /**
   * Returns the object that will replace this one.
   *
   * @return the read object, should never be null
   */
  private def readResolve: AnyRef = obj

  def writeExternal(out: ObjectOutput): Unit = Ser.writeInternal(tpe, obj, out)

  def readExternal(in: ObjectInput): Unit = {
    tpe = in.readByte
    obj = Ser.readInternal(tpe, in)
  }
}