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

package javax.time

import calendar.LocalDate
import java.io._
import java.util.ConcurrentModificationException
import java.util.Arrays
import java.util.concurrent.atomic.AtomicReference

/**
 * System default UTC rules.
 * <p>
 * SystemUTCRules is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */

@SerialVersionUID(1L)
sealed class SystemUTCRules private extends UTCRules with Serializable {

  import SystemUTCRules._
  import UTCRules._

  /**
   * Resolves singleton.
   *
   * @return the resolved instance, never null
   */
  private def readResolve: AnyRef = SystemUTCRules

  /**
   * The table of leap second dates.
   */
  private def dataRef: AtomicReference[Data] = new AtomicReference[Data](loadLeapSeconds);

  //-----------------------------------------------------------------------
  /**
   * Adds a new leap second to these rules.
   *
   * @param mjDay the modified julian date that the leap second occurs at the end of
   * @param leapAdjustment the leap seconds to add/remove at the end of the day, either -1 or 1
   * @throws IllegalArgumentException if the leap adjustment is invalid
   * @throws IllegalArgumentException if the day is before or equal the last known leap second day
   *  and the definition does not match a previously registered leap
   * @throws ConcurrentModificationException if another thread updates the rules at the same time
   */
  private[time] def registerLeapSecond(mjDay: Long, leapAdjustment: Int): Unit = {
    if (leapAdjustment != -1 && leapAdjustment != 1) {
      throw new IllegalArgumentException("Leap adjustment must be -1 or 1")
    }
    val data: SystemUTCRules.Data = dataRef.get
    val pos: Int = Arrays.binarySearch(data.dates, mjDay)
    var currentAdj: Int = if (pos > 0) data.offsets(pos) - data.offsets(pos - 1) else 0
    if (currentAdj == leapAdjustment) {
      return
    }
    if (mjDay <= data.dates(data.dates.length - 1)) {
      throw new IllegalArgumentException("Date must be after the last configured leap second date")
    }
    val dates: Array[Long] = Arrays.copyOf(data.dates, data.dates.length + 1)
    val offsets: Array[Int] = Arrays.copyOf(data.offsets, data.offsets.length + 1)
    val taiSeconds: Array[Long] = Arrays.copyOf(data.taiSeconds, data.taiSeconds.length + 1)
    val offset: Int = offsets(offsets.length - 2) + leapAdjustment
    dates(dates.length - 1) = mjDay
    offsets(offsets.length - 1) = offset
    taiSeconds(taiSeconds.length - 1) = tai(mjDay, offset)
    val newData: SystemUTCRules.Data = new SystemUTCRules.Data(dates, offsets, taiSeconds)
    if (dataRef.compareAndSet(data, newData) == false) {
      throw new ConcurrentModificationException("Unable to update leap second rules as they have already been updated")
    }
  }

  def getName: String = "System"

  def getLeapSecondAdjustment(mjDay: Long): Int = {
    val data: SystemUTCRules.Data = dataRef.get
    val pos: Int = Arrays.binarySearch(data.dates, mjDay)
    if (pos > 0) data.offsets(pos) - data.offsets(pos - 1) else 0
  }

  def getTAIOffset(mjDay: Long): Int = {
    val data: SystemUTCRules.Data = dataRef.get
    var pos: Int = Arrays.binarySearch(data.dates, mjDay)
    pos = (if (pos < 0) ~pos else pos)
    if (pos > 0) data.offsets(pos - 1) else 10
  }

  def getLeapSecondDates: Array[Long] = {
    val data: SystemUTCRules.Data = dataRef.get
    data.dates.clone
  }

  protected def convertToUTC(taiInstant: TAIInstant): UTCInstant = {
    val data: SystemUTCRules.Data = dataRef.get
    val mjds: Array[Long] = data.dates
    val tais: Array[Long] = data.taiSeconds
    var pos: Int = Arrays.binarySearch(tais, taiInstant.getTAISeconds)
    pos = (if (pos >= 0) pos else ~pos - 1)
    val taiOffset: Int = (if (pos >= 0) data.offsets(pos) else 10)
    val adjustedTaiSecs: Long = taiInstant.getTAISeconds - taiOffset
    var mjd: Long = MathUtils.floorDiv(adjustedTaiSecs, SecondsPerDay) + ModifiedJulianDaysTAIOffset
    var nod: Long = MathUtils.floorMod(adjustedTaiSecs, SecondsPerDay) * NanosPerSecond + taiInstant.getNanoOfSecond
    var mjdNextRegionStart: Long = (if (pos + 1 < mjds.length) mjds(pos + 1) + 1 else Long.MaxValue)
    if (mjd == mjdNextRegionStart) {
      mjd -= 1;
      nod = SecondsPerDay * NanosPerSecond + (nod / NanosPerSecond) * NanosPerSecond + nod % NanosPerSecond
    }
    return UTCInstant.ofModifiedJulianDays(mjd, nod, this)
  }
}

object SystemUTCRules extends SystemUTCRules {

  @SerialVersionUID(1L)
  private class Data(val dates: Array[Long], val offsets: Array[Int], val taiSeconds: Array[Long]) extends Serializable

  /**
   * Loads the leap seconds from file.
   * @return an array of two arrays - leap seconds dates and amounts
   */
  private def loadLeapSeconds: SystemUTCRules.Data = {
    var in: InputStream = classOf[SystemUTCRules].getResourceAsStream("/javax/time/LeapSeconds.txt")
    if (in == null) {
      throw new CalendricalException("LeapSeconds.txt resource missing")
    }
    try {
      val reader: BufferedReader = new BufferedReader(new InputStreamReader(in, "UTF-8"))
      val leaps: java.util.Map[Long, Int] = new java.util.TreeMap[Long, Int]
      while (true) {
        var line: String = reader.readLine
        if (line != null) {
          line = line.trim
          if (line.length > 0 && line.charAt(0) != '#') {
            val split: Array[String] = line.split(" ")
            if (split.length != 2) {
              throw new CalendricalException("LeapSeconds.txt has invalid line format")
            }
            val date: LocalDate = LocalDate.parse(split(0))
            val offset: Int = split(1).toInt
            leaps.put(date.toModifiedJulianDays, offset)
          }
        }
      }
      val dates: Array[Long] = new Array[Long](leaps.size)
      val offsets: Array[Int] = new Array[Int](leaps.size)
      val taiSeconds: Array[Long] = new Array[Long](leaps.size)
      var i: Int = 0
      for (entry <- leaps.entrySet) {
        var changeMjd: Long = entry.getKey - 1
        var offset: Int = entry.getValue
        if (i > 0) {
          val adjust: Int = offset - offsets(i - 1)
          if (adjust < -1 || adjust > 1) {
            throw new CalendricalException("Leap adjustment must be -1 or 1")
          }
        }
        dates(i) = changeMjd
        offsets(i) = offset
        taiSeconds(({
          i += 1;
          i
        })) = tai(changeMjd, offset)
      }
      return new SystemUTCRules.Data(dates, offsets, taiSeconds)
    }
    catch {
      case ex: IOException => {
        try {
          in.close
        }
        catch {
          case ignored: IOException => {
          }
        }
        throw new CalendricalException("Exception reading LeapSeconds.txt", ex)
      }
    }
  }

  /**
   * Gets the TAI seconds for the start of the day following the day passed in.
   * @param changeMjd the MJD that the leap second is added to
   * @param offset the new offset after the leap
   * @return the TAI seconds
   */
  private def tai(changeMjd: Long, offset: Int): Long = (changeMjd + 1 - UTCRules.ModifiedJulianDaysTAIOffset) * UTCRules.SecondsPerDay + offset
}