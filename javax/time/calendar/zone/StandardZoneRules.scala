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
import java.io.Serializable
import java.util.ArrayList
import java.util.Arrays
import java.util.List
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentMap
import javax.time.Instant
import javax.time.InstantProvider
import javax.time.calendar.LocalDateTime
import javax.time.calendar.OffsetDateTime
import javax.time.calendar.Year
import javax.time.calendar.ZoneOffset

/**
 * The rules describing how the zone offset varies through the year and historically.
 * <p>
 * ZoneRules is immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object StandardZoneRules {
  /**
   * The last year to have its transitions cached.
   */
  private val LAST_CACHED_YEAR: Year = Year.of(2100)

  /**
   * Reads the state from the stream.
   * @param in the input stream, not null
   * @return the created object, never null
   * @throws IOException if an error occurs
   */
  private[zone] def readExternal(in: DataInput): StandardZoneRules = {
    var stdSize: Int = in.readInt
    var stdTrans: Array[Long] = new Array[Long](stdSize) {
      var i: Int = 0
      while (i < stdSize) {
          stdTrans(i) = Ser.readEpochSecs(in)
          i += 1;
      }
    }
    var stdOffsets: Array[ZoneOffset] = new Array[ZoneOffset](stdSize + 1) {
      var i: Int = 0
      while (i < stdOffsets.length) {
          stdOffsets(i) = Ser.readOffset(in)
          i += 1;
      }
    }
    var savSize: Int = in.readInt
    var savTrans: Array[Long] = new Array[Long](savSize) {
      var i: Int = 0
      while (i < savSize) {
          savTrans(i) = Ser.readEpochSecs(in)
          i += 1;
      }
    }
    var savOffsets: Array[ZoneOffset] = new Array[ZoneOffset](savSize + 1) {
      var i: Int = 0
      while (i < savOffsets.length) {
          savOffsets(i) = Ser.readOffset(in)
          i += 1;
      }
    }
    var ruleSize: Int = in.readByte
    var rules: Array[ZoneOffsetTransitionRule] = new Array[ZoneOffsetTransitionRule](ruleSize) {
      var i: Int = 0
      while (i < ruleSize) {
          rules(i) = ZoneOffsetTransitionRule.readExternal(in)
          i += 1;
      }
    }
    return new StandardZoneRules(stdTrans, stdOffsets, savTrans, savOffsets, rules)
  }
}


/**
 * Constructor.
 *
 * @param standardTransitions the standard transitions, not null
 * @param standardOffsets the standard offsets, not null
 * @param savingsLocalTransitions the standard transitions, not null
 * @param wallOffsets the wall offsets, not null
 * @param lastRules the recurring last rules, size 15 or less, not null
 */
@SerialVersionUID(1L)
final class StandardZoneRules private[zone](val standardTransitions: Array[Long],
                                      val standardOffsets: Array[ZoneOffset],
                                      val savingsInstantTransitions: Array[Long],
                                      val wallOffsets: Array[ZoneOffset],
                                      val lastRules: Array[ZoneOffsetTransitionRule])
  extends ZoneRules with Serializable {

  var localTransitionList: List[LocalDateTime] = new ArrayList[LocalDateTime] {
    var i: Int = 0
    while (i < savingsInstantTransitions.length) {
      var before: ZoneOffset = wallOffsets(i)
      var after: ZoneOffset = wallOffsets(i + 1)
      var odt: OffsetDateTime = OffsetDateTime.ofEpochSeconds(savingsInstantTransitions(i), before)
      var trans: ZoneOffsetTransition = new ZoneOffsetTransition(odt, after)
      if (trans.isGap) {
        localTransitionList.add(trans.getDateTime.toLocalDateTime)
        localTransitionList.add(trans.getDateTimeAfter.toLocalDateTime)
      }
      else {
        localTransitionList.add(trans.getDateTimeAfter.toLocalDateTime)
        localTransitionList.add(trans.getDateTime.toLocalDateTime)
      }
      i += 1;
    }
  }

  //FIXME
  /**
   * Constructor.
   *
   * @param baseStandardOffset the standard offset to use before legal rules were set, not null
   * @param baseWallOffset the wall offset to use before legal rules were set, not null
   * @param standardOffsetTransitionList the list of changes to the standard offset, not null
   * @param transitionList the list of transitions, not null
   * @param lastRules the recurring last rules, size 15 or less, not null
   */
  //  private[zone] def this(baseStandardOffset: ZoneOffset,
  //                         baseWallOffset: ZoneOffset,
  //                         standardOffsetTransitionList: List[OffsetDateTime],
  //                         transitionList: List[ZoneOffsetTransition],
  //                         lastRules: List[ZoneOffsetTransitionRule]) {
  //
  //  //    this.standardTransitions = new Array[Long] (standardOffsetTransitionList.size)
  //
  //  this.standardOffsets = new Array[ZoneOffset](standardOffsetTransitionList.size + 1)
  //  this.standardOffsets(0) = baseStandardOffset
  //
  //  {
  //    var i: Int = 0
  //    while (i < standardOffsetTransitionList.size) {
  //      {
  //        this.standardTransitions(i) = standardOffsetTransitionList.get(i).toEpochSeconds
  //        this.standardOffsets(i + 1) = standardOffsetTransitionList.get(i).getOffset
  //      }
  //      ({
  //        i += 1;
  //        i
  //      })
  //    }
  //  }
  //  var localTransitionList: List[LocalDateTime] = new ArrayList[LocalDateTime]
  //  var localTransitionOffsetList: List[ZoneOffset] = new ArrayList[ZoneOffset]
  //  localTransitionOffsetList.add(baseWallOffset)
  //  for (trans <- transitionList) {
  //    if (trans.isGap) {
  //      localTransitionList.add(trans.getDateTime.toLocalDateTime)
  //      localTransitionList.add(trans.getDateTimeAfter.toLocalDateTime)
  //    }
  //    else {
  //      localTransitionList.add(trans.getDateTimeAfter.toLocalDateTime)
  //      localTransitionList.add(trans.getDateTime.toLocalDateTime)
  //    }
  //    localTransitionOffsetList.add(trans.getOffsetAfter)
  //  }
  //  this.savingsLocalTransitions = localTransitionList.toArray(new Array[LocalDateTime](localTransitionList.size))
  //  this.wallOffsets = localTransitionOffsetList.toArray(new Array[ZoneOffset](localTransitionOffsetList.size))
  //  this.savingsInstantTransitions = new Array[Long](transitionList.size)
  //
  //  {
  //    var i: Int = 0
  //    while (i < transitionList.size) {
  //      {
  //        this.savingsInstantTransitions(i) = transitionList.get(i).getInstant.getEpochSeconds
  //      }
  //      ({
  //        i += 1;
  //        i
  //      })
  //    }
  //  }
  //  if (lastRules.size > 15) throw new IllegalArgumentException("Too many transition rules")
  //  this.lastRules = lastRules.toArray(new Array[ZoneOffsetTransitionRule](lastRules.size))
  //  }

  /**
   * Returns a suitable hash code.
   *
   * @return the hash code
   */
  override def hashCode: Int = {
    Arrays.hashCode(standardTransitions) ^ Arrays.hashCode(standardOffsets.asInstanceOf[Array[AnyRef]]) ^ Arrays.hashCode(savingsInstantTransitions) ^ Arrays.hashCode(wallOffsets.asInstanceOf[Array[AnyRef]]) ^ Arrays.hashCode(lastRules.asInstanceOf[Array[AnyRef]])
  }

  /**
   * Gets the list of transition rules for years beyond those defined in the transition list.
   * <p>
   * The list represents all the transitions that are expected in each year
   * beyond those in the transition list. Normally, there are two transitions
   * per year - into and out of daylight savings time. If daylight savings
   * time does not occur then the list will be empty.
   *
   * @return independent, modifiable copy of the list of transition rules, never null
   */
  def getTransitionRules: List[ZoneOffsetTransitionRule] = new ArrayList[ZoneOffsetTransitionRule](Arrays.asList(lastRules))

  /**{ @inheritDoc }*/
  def getOffset(instantProvider: InstantProvider): ZoneOffset = {
    var instant: Instant = Instant.of(instantProvider)
    var epochSecs: Long = instant.getEpochSeconds
    if (lastRules.length > 0 && epochSecs > savingsInstantTransitions(savingsInstantTransitions.length - 1)) {
      var dt: OffsetDateTime = OffsetDateTime.ofInstant(instant, wallOffsets(wallOffsets.length - 1))
      var transArray: Array[ZoneOffsetTransition] = findTransitionArray(dt.toYear)
      var trans: ZoneOffsetTransition = null
        var i: Int = 0
        while (i < transArray.length) {
            trans = transArray(i)
            if (instant.isBefore(trans.getInstant)) {
              return trans.getOffsetBefore
            }
            i += 1;
        }
      return trans.getOffsetAfter
    }
    var index: Int = Arrays.binarySearch(savingsInstantTransitions, epochSecs)
    if (index < 0) {
      index = -index - 2
    }
    return wallOffsets(index + 1)
  }

  /**
   * Writes the state to the stream.
   * @param out the output stream, not null
   * @throws IOException if an error occurs
   */
  private[zone] def writeExternal(out: DataOutput): Unit = {
    out.writeInt(standardTransitions.length)
    for (trans <- standardTransitions) {
      Ser.writeEpochSecs(trans, out)
    }
    for (offset <- standardOffsets) {
      Ser.writeOffset(offset, out)
    }
    out.writeInt(savingsInstantTransitions.length)
    for (trans <- savingsInstantTransitions) {
      Ser.writeEpochSecs(trans, out)
    }
    for (offset <- wallOffsets) {
      Ser.writeOffset(offset, out)
    }
    out.writeByte(lastRules.length)
    for (rule <- lastRules) {
      rule.writeExternal(out)
    }
  }

  /**
   * Finds the offset info for a local date-time and transition.
   *
   * @param dt the date-time, not null
   * @param trans the transition, not null
   * @return the offset info, never null
   */
  private def findOffsetInfo(dt: LocalDateTime, trans: ZoneOffsetTransition): ZoneOffsetInfo = {
    if (trans.isGap) {
      if (dt.isBefore(trans.getLocal)) {
        return new ZoneOffsetInfo(dt, trans.getOffsetBefore, null)
      }
      if (dt.isBefore(trans.getDateTimeAfter.toLocalDateTime)) {
        return new ZoneOffsetInfo(dt, null, trans)
      }
      else {
        return new ZoneOffsetInfo(dt, trans.getOffsetAfter, null)
      }
    }
    else {
      if (dt.isBefore(trans.getLocal) == false) {
        return new ZoneOffsetInfo(dt, trans.getOffsetAfter, null)
      }
      if (dt.isBefore(trans.getDateTimeAfter.toLocalDateTime)) {
        return new ZoneOffsetInfo(dt, trans.getOffsetBefore, null)
      }
      else {
        return new ZoneOffsetInfo(dt, null, trans)
      }
    }
  }

  /**
   * Uses a serialization delegate.
   *
   * @return the replacing object, never null
   */
  private def writeReplace: AnyRef = new Ser(Ser.SZR, this)

  /**
   * Gets the next transition after the specified transition.
   *
   * @param instantProvider the instant to get the next transition after, not null
   * @return the next transition after the specified instant, null if this is after the last transition
   */
  def nextTransition(instantProvider: InstantProvider): ZoneOffsetTransition = {
    var instant: Instant = Instant.of(instantProvider)
    var epochSecs: Long = instant.getEpochSeconds
    if (epochSecs >= savingsInstantTransitions(savingsInstantTransitions.length - 1)) {
      if (lastRules.length == 0) {
        return null
      }
      var dt: OffsetDateTime = OffsetDateTime.ofInstant(instant, wallOffsets(wallOffsets.length - 1))

      {
        var year: Year = dt.toYear
        while (true) {
          {
            var transArray: Array[ZoneOffsetTransition] = findTransitionArray(year)
            for (trans <- transArray) {
              if (instant.isBefore(trans.getInstant)) {
                return trans
              }
            }
            if (year.getValue == Year.MAX_YEAR) {
              return null
            }
          }
          year = year.next
        }
      }
    }
    var index: Int = Arrays.binarySearch(savingsInstantTransitions, epochSecs)
    if (index < 0) {
      index = -index - 1
    }
    else {
      index += 1
    }
    var transitionInstant: Instant = Instant.ofEpochSeconds(savingsInstantTransitions(index))
    var trans: OffsetDateTime = OffsetDateTime.ofInstant(transitionInstant, wallOffsets(index))
    return new ZoneOffsetTransition(trans, wallOffsets(index + 1))
  }

  /**
   * Checks if this object equals another, comparing the complete set of rules.
   * <p>
   * The entire state of the object is compared.
   *
   * @param other the other object to compare to, null returns false
   * @return true if equal
   */
  override def equals(otherRules: AnyRef): Boolean = {
    if (this == otherRules) true
    else if (otherRules.isInstanceOf[StandardZoneRules]) {
      val other: StandardZoneRules = otherRules.asInstanceOf[StandardZoneRules]
      Arrays.equals(standardTransitions, other.standardTransitions) && Arrays.equals(standardOffsets.asInstanceOf[Array[AnyRef]], other.standardOffsets.asInstanceOf[Array[AnyRef]]) && Arrays.equals(savingsInstantTransitions, other.savingsInstantTransitions) && Arrays.equals(wallOffsets.asInstanceOf[Array[AnyRef]], other.wallOffsets.asInstanceOf[Array[AnyRef]]) && Arrays.equals(lastRules.asInstanceOf[Array[AnyRef]], other.lastRules.asInstanceOf[Array[AnyRef]])
    }
    else false
  }

  /**
   * Returns a string describing this object.
   *
   * @return a string for debugging, never null
   */
  override def toString: String = {
    "StandardZoneRules[currentStandardOffset=" + standardOffsets(standardOffsets.length - 1) + "]"
  }

  /**
   * The map of recent transitions.
   */
  private val lastRulesCache: ConcurrentMap[Year, Array[ZoneOffsetTransition]] = new ConcurrentHashMap[Year, Array[ZoneOffsetTransition]]

  /**
   * Gets the previous transition after the specified transition.
   *
   * @param instantProvider the instant to get the previous transition after, not null
   * @return the previous transition after the specified instant, null if this is before the first transition
   */
  def previousTransition(instantProvider: InstantProvider): ZoneOffsetTransition = {
    var instant: Instant = Instant.of(instantProvider)
    var epochSecs: Long = instant.getEpochSeconds
    if (instant.getNanoOfSecond > 0 && epochSecs < Long.MaxValue) {
      epochSecs += 1
    }
    var lastHistoric: Long = savingsInstantTransitions(savingsInstantTransitions.length - 1)
    if (lastRules.length > 0 && epochSecs > lastHistoric) {
      var lastHistoricOffset: ZoneOffset = wallOffsets(wallOffsets.length - 1)
      var dt: OffsetDateTime = OffsetDateTime.ofInstant(instant, lastHistoricOffset)
      var lastHistoricDT: OffsetDateTime = OffsetDateTime.ofInstant(Instant.ofEpochSeconds(lastHistoric), lastHistoricOffset)

      {
        var year: Year = dt.toYear
        while (year.getValue > lastHistoricDT.getYear) {
            var transArray: Array[ZoneOffsetTransition] = findTransitionArray(year)

              var i: Int = transArray.length - 1
              while (i >= 0) {
                  if (instant.isAfter(transArray(i).getInstant)) {
                    return transArray(i)
                  }
                  i -= 1;
              }
          year = year.previous
        }
      }
    }
    var index: Int = Arrays.binarySearch(savingsInstantTransitions, epochSecs)
    if (index < 0) {
      index = -index - 1
    }
    if (index <= 0) {
      return null
    }
    var transitionInstant: Instant = Instant.ofEpochSeconds(savingsInstantTransitions(index - 1))
    var trans: OffsetDateTime = OffsetDateTime.ofInstant(transitionInstant, wallOffsets(index - 1))
    return new ZoneOffsetTransition(trans, wallOffsets(index))
  }


  /**
   * Finds the appropriate transition array for the given year.
   *
   * @param year the year, not null
   * @return the transition array, never null
   */
  private def findTransitionArray(year: Year): Array[ZoneOffsetTransition] = {
    var transArray: Array[ZoneOffsetTransition] = lastRulesCache.get(year)
    if (transArray != null) {
      return transArray
    }
    var ruleArray: Array[ZoneOffsetTransitionRule] = lastRules
    transArray = new Array[ZoneOffsetTransition](ruleArray.length) {
      var i: Int = 0
      while (i < ruleArray.length) {
          transArray(i) = ruleArray(i).createTransition(year.getValue)
          i += 1;
      }
    }
    if (year.isBefore(StandardZoneRules.LAST_CACHED_YEAR)) {
      lastRulesCache.putIfAbsent(year, transArray)
    }
    return transArray
  }

  /**{ @inheritDoc }*/
  def getOffsetInfo(dt: LocalDateTime): ZoneOffsetInfo = {
    if (lastRules.length > 0 && dt.isAfter(savingsLocalTransitions(savingsLocalTransitions.length - 1))) {
      var transArray: Array[ZoneOffsetTransition] = findTransitionArray(dt.toYear)
      var info: ZoneOffsetInfo = null
      for (trans <- transArray) {
        info = findOffsetInfo(dt, trans)
        if (info.isTransition || info.getOffset.equals(trans.getOffsetBefore)) {
          return info
        }
      }
      return info
    }
    var index: Int = Arrays.binarySearch(savingsLocalTransitions.asInstanceOf[Array[AnyRef]], dt)
    if (index == -1) {
      return new ZoneOffsetInfo(dt, wallOffsets(0), null)
    }
    if (index < 0) {
      index = -index - 2
    }
    else if (index < savingsLocalTransitions.length - 1 && savingsLocalTransitions(index).equals(savingsLocalTransitions(index + 1))) {
        index += 1;
    }
    if ((index & 1) == 0) {
      var dtBefore: LocalDateTime = savingsLocalTransitions(index)
      var dtAfter: LocalDateTime = savingsLocalTransitions(index + 1)
      var offsetBefore: ZoneOffset = wallOffsets(index / 2)
      var offsetAfter: ZoneOffset = wallOffsets(index / 2 + 1)
      if (offsetAfter.getAmountSeconds > offsetBefore.getAmountSeconds) {
        return new ZoneOffsetInfo(dt, null, new ZoneOffsetTransition(OffsetDateTime.of(dtBefore, offsetBefore), offsetAfter))
      }
      else {
        return new ZoneOffsetInfo(dt, null, new ZoneOffsetTransition(OffsetDateTime.of(dtAfter, offsetBefore), offsetAfter))
      }
    }
    else {
      return new ZoneOffsetInfo(dt, wallOffsets(index / 2 + 1), null)
    }
  }

  /**{ @inheritDoc }*/
  def getStandardOffset(instantProvider: InstantProvider): ZoneOffset = {
    var instant: Instant = Instant.of(instantProvider)
    var epochSecs: Long = instant.getEpochSeconds
    var index: Int = Arrays.binarySearch(standardTransitions, epochSecs)
    if (index < 0) {
      index = -index - 2
    }
    return standardOffsets(index + 1)
  }

  /**
   * The transitions between local date-times, sorted.
   * This is a paired array, where the first entry is the start of the transition
   * and the second entry is the end of the transition.
   */
  private val savingsLocalTransitions: Array[LocalDateTime] = {
    localTransitionList.toArray(new Array[LocalDateTime](localTransitionList.size))
  }

  /**
   * Gets the complete list of transitions.
   * <p>
   * This list normally contains a complete historical set of transitions
   * that have occurred. Some transitions may be in the future, although
   * generally the transition rules handle future years.
   *
   * @return independent, modifiable copy of the list of transitions, never null
   */
  def getTransitions: List[ZoneOffsetTransition] = {
    var list: List[ZoneOffsetTransition] = new ArrayList[ZoneOffsetTransition] {
      var i: Int = 0
      while (i < savingsInstantTransitions.length) {
          var instant: Instant = Instant.ofEpochSeconds(savingsInstantTransitions(i))
          var trans: OffsetDateTime = OffsetDateTime.ofInstant(instant, wallOffsets(i))
          list.add(new ZoneOffsetTransition(trans, wallOffsets(i + 1)))
          i += 1;
      }
    }
    return list
  }
}