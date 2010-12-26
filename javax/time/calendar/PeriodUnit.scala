/*
 * Copyright (c) 2010 Stephen Colebourne & Michael Nascimento Santos
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

import javax.time.Duration

/**
 * A unit of time for measuring a period, such as 'Days' or 'Minutes'.
 * <p>
 * {@code PeriodUnit} is an immutable definition of a unit of human-scale time.
 * For example, humans typically measure periods of time in units of years, months,
 * days, hours, minutes and seconds. These concepts are defined by instances of
 * this class defined in the chronology classes.
 * <p>
 * Units are either basic or derived. A derived unit can be converted accurately to
 * another smaller unit. A basic unit is fundamental, and has no smaller representation.
 * For example years are a derived unit consisting of 12 months, where a month is
 * a basic unit.
 * <p>
 * PeriodUnit is an abstract class and must be implemented with care
 * to ensure other classes in the framework operate correctly.
 * All instantiable implementations must be final, immutable and thread-safe.
 * <p>
 * The subclass is fully responsible for serialization as all fields in this class are
 * transient. The subclass must use {@code readResolve} to replace the deserialized
 * class with a valid one created via a constructor.
 *
 * @author Stephen Colebourne
 */
object PeriodUnit {
  /**
   * Helper method for constructors to built the equivalent periods.
   *
   * @param equivalentPeriod the period this is derived from, null if no equivalent
   * @return the list of equivalent periods, never null
   */
  private[calendar] def buildEquivalentPeriods(equivalentPeriod: PeriodField): Seq[PeriodField] = {
    if (equivalentPeriod == null) {
      List()
    } else {
      val multiplier: Long = equivalentPeriod.getAmount
      val baseEquivalents = equivalentPeriod.getUnit.getEquivalentPeriods
      val equivalents = baseEquivalents.map(_ * multiplier)
      equivalentPeriod :: equivalents.toList
    }
  }
}

/**
 * @param name the name of the unit, never null
 * @param equivalentPeriod the cache of periods equivalent to this unit, not null
 * @param estimatedDuration the estimated duration of the unit, not null
 * @param hashcode the cache of the unit hash code
 */
@SerialVersionUID(1L)
abstract class PeriodUnit private[calendar](@transient val name: String, @transient val equivalentPeriods: Seq[PeriodField], @transient val estimatedDuration: Duration, @transient val hashCode: Int)
  extends Ordered[PeriodUnit] with Serializable {

  import PeriodUnit._

  ISOChronology.checkNotNull(name, "Name must not be null")

  /**
   * Constructor used by ISOChronology.
   *
   * @param name  the name of the type, not null
   * @param equivalentPeriod  the period this is derived from, null if no equivalent
   * @param estimatedDuration  the estimated duration of one unit of this period, not null
   * @throws ArithmeticException if the equivalent period calculation overflows
   */
  def this(name: String, equivalentPeriod: PeriodField, estimatedDuration: Duration) {
    // input known to be valid
    this (name, buildEquivalentPeriods(equivalentPeriod), estimatedDuration, name.hashCode ^ estimatedDuration.hashCode ^ (if (equivalentPeriod == null) 0 else equivalentPeriod.hashCode))
  }

  /**
   * Constructor to create a unit that is derived from another smaller unit.
   * <p>
   * A derived unit is created as a multiple of a smaller unit.
   * For example, an ISO year period can be derived as 12 ISO month periods.
   * <p>
   * The estimated duration is calculated using {@link PeriodField#toEstimatedDuration()}.
   * <p>
   * This method is typically only used when writing a {@link Chronology}.
   *
   * @param name the name of the type, not null
   * @param equivalentPeriod the period this is derived from, not null
   * @throws IllegalArgumentException if the period is zero or negative
   * @throws ArithmeticException if the equivalent period calculation overflows
   */
  protected def this(name: String, equivalentPeriod: PeriodField) {
    this (name, buildEquivalentPeriods(equivalentPeriod), equivalentPeriod.toEstimatedDuration, name.hashCode ^ estimatedDuration.hashCode ^ equivalentPeriod.hashCode)
    ISOChronology.checkNotNull(equivalentPeriod, "Equivalent period must not be null")
    if (equivalentPeriod.getAmount <= 0) {
      throw new IllegalArgumentException("Equivalent period must not be negative or zero")
    }
  }

  /**
   * Constructor to create a base unit that cannot be derived.
   * <p>
   * A base unit cannot be derived from any smaller unit.
   * For example, an ISO month period cannot be derived from any other smaller period.
   * <p>
   * This method is typically only used when writing a {@link Chronology}.
   *
   * @param name the name of the type, not null
   * @param estimatedDuration the estimated duration of one unit of this period, not null
   * @throws IllegalArgumentException if the duration is zero or negative
   */
  protected def this(name: String, estimatedDuration: Duration) {
    this (name, buildEquivalentPeriods(null), estimatedDuration, name.hashCode ^ estimatedDuration.hashCode ^ 0)
    ISOChronology.checkNotNull(estimatedDuration, "Estimated duration must not be null")
    if (estimatedDuration.isNegative || estimatedDuration.isZero) {
      throw new IllegalArgumentException("Alternate period must not be negative or zero")
    }
  }

  /**
   * Gets the periods that are equivalent to this unit.
   * <p>
   * Most units are related to other units.
   * For example, an hour might be represented as 60 minutes or 3600 seconds.
   * Thus, if this is the 'Hour' unit, then this method would return a list
   * including both '60 Minutes', '3600 Seconds' and any other equivalent periods.
   * <p>
   * Registered conversion is stored from larger units to smaller units.
   * Thus, {@code monthsUnit.getEquivalentPeriods()} will not contain the unit for years.
   * Note that the returned list does <i>not</i> contain this unit.
   * <p>
   * The list will be unmodifiable and sorted, from largest unit to smallest.
   *
   * @return the equivalent periods, may be empty, never null
   */
  def getEquivalentPeriods: Seq[PeriodField] = equivalentPeriods

  /**
   * Gets an estimate of the duration of the unit in seconds.
   * <p>
   * Each unit has a duration which is a reasonable estimate.
   * For those units which can be derived ultimately from nanoseconds, the
   * estimated duration will be accurate. For other units, it will be an estimate.
   * <p>
   * One key use for the estimated duration is to implement {@link Ordered}.
   *
   * @return the estimate of the duration in seconds, never null
   */
  def getEstimatedDuration: Duration = estimatedDuration

  /**
   * Checks whether this unit can be converted to the specified unit.
   * <p>
   * Most units are related to other units.
   * For example, an hour might be represented as 60 minutes or 3600 seconds.
   * This method checks if this unit has a registered conversion to the specified unit.
   * <p>
   * Registered conversion is stored from larger units to smaller units.
   * Thus, {@code monthsUnit.isConvertibleTo ( yearsUnit )} will return false.
   * Note that this unit is convertible to itself.
   *
   * @param unit the unit, null returns false
   * @return true if this unit is convertible or equal to the specified unit
   */
  def isConvertibleTo(unit: PeriodUnit): Boolean = {
    if(equivalentPeriods.exists(_.getUnit == unit)) true
    else this == unit
  }

  /**
   * Compares this unit to another.
   * <p>
   * The comparison is based primarily on the {@link #getEstimatedDuration ( ) estimated duration}.
   * If that is equal, the name is compared using standard string comparison.
   * Finally, the first equivalent period is checked, with basic units before derived ones.
   *
   * @param other the other type to compare to, not null
   * @return the comparator result, negative if less, positive if greater, zero if equal
   * @throws NullPointerException if other is null
   */
  def compare(other: PeriodUnit): Int = {
    var cmp: Int = estimatedDuration.compareTo(other.estimatedDuration)
    if (cmp == 0) {
      cmp = name.compareTo(other.name)
      if (cmp == 0) {
        cmp = (equivalentPeriods.size - other.equivalentPeriods.size)
        if (cmp == 0 && equivalentPeriods.size > 0) {
          cmp = (equivalentPeriods(0).compareTo(other.equivalentPeriods(0)))
        }
      }
    }
    return cmp
  }

  /**
   * Gets the base unit of this unit.
   * <p>
   * Most units are related to other units.
   * For example, an hour might be represented as 60 minutes or 3600 seconds.
   * The base unit is the smallest unit that this unit defines an equivalence to.
   * <p>
   * For example, most time units are ultimately convertible to nanoseconds,
   * thus nanoseconds is the base unit.
   *
   * @return the base unit, never null
   */
  def getBaseUnit: PeriodUnit = {
    if (equivalentPeriods.isEmpty) this
    else equivalentPeriods(equivalentPeriods.size - 1).getUnit
  }

  /**
   * Compares two units based on the name, estimated duration and equivalent period.
   *
   * @return true if the units are the same
   */
  override def equals(obj: AnyRef): Boolean = {
    if (obj eq this) true
    else if (obj.isInstanceOf[PeriodUnit]) {
      val other: PeriodUnit = obj.asInstanceOf[PeriodUnit]
      name.equals(other.name) && estimatedDuration.equals(other.estimatedDuration) && equivalentPeriods.size == other.equivalentPeriods.size && (equivalentPeriods.size == 0 || equivalentPeriods(0).equals(other.equivalentPeriods(0)))
    } else false
  }

  /**
   * Gets the name of the unit, used as an identifier for the unit.
   * <p>
   * Implementations should use the name that best represents themselves.
   * Most units will have a plural name, such as 'Years' or 'Minutes'.
   * The name is not localized.
   *
   * @return the name of the unit, never null
   */
  def getName: String = name

  /**
   * Gets the period in the specified unit that is equivalent to this unit.
   * <p>
   * Most units are related to other units.
   * For example, an hour might be represented as 60 minutes or 3600 seconds.
   * Thus, if this is the 'Hour' unit and the 'Seconds' unit is requested,
   * then this method would return '3600 Seconds'.
   * <p>
   * Registered conversion is stored from larger units to smaller units.
   * Thus, {@code monthsUnit.getEquivalentPeriod ( yearsUnit )} will return null.
   * Note that if the unit specified is this unit, then a period of 1 of this unit is returned.
   *
   * @param requiredUnit the required unit, not null
   * @return the equivalent period, null if no equivalent in that unit
   */
  def getEquivalentPeriod(requiredUnit: PeriodUnit): PeriodField = {
    ISOChronology.checkNotNull(requiredUnit, "PeriodUnit must not be null")
    for (equivalent <- equivalentPeriods) {
      if (equivalent.getUnit.equals(requiredUnit)) {
        return equivalent
      }
    }
    if (requiredUnit.equals(this)) {
      return PeriodField.of(1, this)
    }
    return null
  }

  /**
   * Returns a string representation of the unit.
   * <p>
   * The string representation is the same as the name.
   *
   * @return the unit name, never null
   */
  override def toString: String = name
}