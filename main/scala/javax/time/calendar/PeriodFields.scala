/*
 * Copyright (c) 2008-2010, Stephen Colebourne & Michael Nascimento Santos
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

import javax.time.CalendricalException
import javax.time.Duration
import util.control.Breaks._
import collection.immutable.{TreeSet, TreeMap}

/**
 * A period of time measured using a number of different units,
 * such as '3 Months, 4 Days and 7 Hours'.
 * <p>
 * {@code PeriodFields} is an immutable period that stores an amount of human-scale
 * time for a number of units. For example, humans typically measure periods of time
 * in units of years, months, days, hours, minutes and seconds. These concepts are
 * defined by instances of {@link PeriodUnit} in the chronology classes. This class
 * allows an amount to be specified for a number of the units, such as '3 Days and 65 Seconds'.
 * <p>
 * Basic mathematical operations are provided - plus(), minus(), multipliedBy(),
 * dividedBy() and negated(), all of which return a new instance
 * <p>
 * A value of zero can also be stored for any unit. This means that a
 * period of zero hours is not equal to a period of zero minutes.
 * However, an empty instance constant exists to represent zero irrespective of unit.
 * The {@link #withZeroesRemoved()} method removes zero values.
 * <p>
 * {@code PeriodFields} can store units of any kind which makes it usable with
 * any calendar system.
 * <p>
 * PeriodFields is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object PeriodFields {
  /**
   * Validates that the input value is not null.
   *
   * @param object the object to check
   * @param errorMessage the error to throw
   * @throws NullPointerException if the object is null
   */
  private[calendar] def checkNotNull(obj: AnyRef, errorMessage: String): Unit = {
    if (obj eq null) throw new NullPointerException(errorMessage)
  }

  /**
   * Obtains a {@code PeriodFields} from a single-unit period.
   *
   * @param period the single-unit period, not null
   * @return the {@code PeriodFields} instance, never null
   */
  def of(period: PeriodField): PeriodFields = {
    checkNotNull(period, "PeriodField must not be null")
    val internalMap: TreeMap[PeriodUnit, PeriodField] = createMap
    internalMap.updated(period.getUnit,period)
    return create(internalMap)
  }

  /**
   * Obtains a {@code PeriodFields} from an amount and unit.
   * <p>
   * The parameters represent the two parts of a phrase like '6 Days'.
   *
   * @param amount the amount of create with, positive or negative
   * @param unit the period unit, not null
   * @return the {@code PeriodFields} instance, never null
   */
  def of(amount: Long, unit: PeriodUnit): PeriodFields = {
    checkNotNull(unit, "PeriodUnit must not be null")
    val internalMap: TreeMap[PeriodUnit, PeriodField] = createMap
    internalMap.updated(unit, PeriodField.of(amount, unit))
    return create(internalMap)
  }

  /**
   * Obtains a {@code PeriodFields} from an array of single-unit periods.
   * <p>
   * The period fields must all have different units.
   *
   * @param periods the array of single-unit periods, not null
   * @return the {@code PeriodFields} instance, never null
   * @throws IllegalArgumentException if the same period unit occurs twice
   */
  def of(periods: Seq[PeriodField]): PeriodFields = {
    checkNotNull(periods, "PeriodField array must not be null")
    val internalMap: TreeMap[PeriodUnit, PeriodField] = createMap
    for (period <- periods) {
      checkNotNull(period, "PeriodField array must not contain null")
      if (internalMap.contains(period.getUnit)) {
        throw new IllegalArgumentException("PeriodField array contains the same unit twice")
      }
      else internalMap.updated(period.getUnit, period)
    }
    return create(internalMap)
  }

  /**
   * Creates a new empty map.
   *
   * @return ordered representation of internal map
   */
  private def createMap = new TreeMap[PeriodUnit, PeriodField]()(implicitly[Ordering[PeriodUnit]].reverse)

  /**
   * Obtains a {@code PeriodFields} by totalling the amounts in a list of
   * {@code PeriodProvider} instances.
   * <p>
   * This method returns a period with all the unit-amount pairs from the providers
   * totalled. Thus a period of '2 Months and 5 Days' combined with a period of
   * '7 Days and 21 Hours' will yield a result of '2 Months, 12 Days and 21 Hours'.
   *
   * @param periodProviders the providers to total, not null
   * @return the {@code PeriodFields} instance, never null
   * @throws NullPointerException if any period provider is null or returns null
   */
  def ofTotal(periodProviders: Array[PeriodProvider]): PeriodFields = {
    checkNotNull(periodProviders, "PeriodProvider[] must not be null")
    if (periodProviders.length == 1) of(periodProviders(0))
    else {
      throw new Exception("Not implemented!")
    }
    //    val map: TreeMap[PeriodUnit, PeriodField] = createMap
    //    for (periodProvider <- periodProviders) {
    //      val periods: PeriodFields = of(periodProvider)
    //      for (period <- periods.unitFieldMap.values) {
    //        val old: PeriodField = map(period.getUnit)
    //        period = (if (old != null) old.plus(period) else period)
    //        map(period.getUnit) = period
    //      }
    //    }
    //    return create(map)
  }

  /**
   * A constant for a period of zero.
   * This constant is independent of any unit.
   */
  object Zero extends PeriodFields(new TreeMap[PeriodUnit, PeriodField])

  /**
   * Obtains a {@code PeriodFields} from a {@code PeriodProvider}.
   * <p>
   * This method provides null-checking around {@link PeriodProvider#toPeriodFields()}.
   *
   * @param periodProvider the provider to create from, not null
   * @return the {@code PeriodFields} instance, never null
   * @throws NullPointerException if the period provider is null or returns null
   */
  def of(periodProvider: PeriodProvider): PeriodFields = {
    checkNotNull(periodProvider, "PeriodProvider must not be null")
    val result: PeriodFields = periodProvider.toPeriodFields
    checkNotNull(result, "PeriodProvider implementation must not return null")
    return result
  }

  /**
   * Obtains a {@code PeriodFields} from a {@code Duration} based on the standard
   * durations of seconds and nanoseconds.
   * <p>
   * The conversion will create an instance with two units - the {@code ISOChronology }
   * seconds and nanoseconds units. This matches the {@link #toDuration()} method.
   *
   * @param duration the duration to create from, not null
   * @return the {@code PeriodFields} instance, never null
   */
  def of(duration: Duration): PeriodFields = {
    checkNotNull(duration, "Duration must not be null")
    val internalMap: TreeMap[PeriodUnit, PeriodField] = createMap
    internalMap.updated(ISOChronology.periodSeconds, PeriodField.of(duration.seconds, ISOChronology.periodSeconds))
    internalMap.updated(ISOChronology.periodNanos, PeriodField.of(duration.nanos, ISOChronology.periodNanos))
    return create(internalMap)
  }

  /**
   * Internal factory to create an instance using a pre-built map.
   * The map must not be used by the calling code after calling the constructor.
   *
   * @param periodMap the unit-amount map, not null, assigned not cloned
   * @return the created period, never null
   */
  private[calendar] def create(periodMap: TreeMap[PeriodUnit, PeriodField]): PeriodFields = {
    if (periodMap.isEmpty) Zero
    else new PeriodFields(periodMap)
  }
}


/**
 * Constructs an instance using a pre-built map.
 * The map must not be used by the calling code after calling the constructor.
 *
 * @param periodMap the map of periods to represent, not null and safe to assign
 */
@SerialVersionUID(1L)
sealed class PeriodFields private(val unitFieldMap: TreeMap[PeriodUnit, PeriodField])
  extends PeriodProvider with Iterable[PeriodField] with Serializable {

  import PeriodFields._

  /**
   * Converts this period to a {@code PeriodFields}, trivially
   * returning {@code this}.
   *
   * @return {@code this}, never null
   */
  override def toPeriodFields: PeriodFields = this

  /**
   * Returns a copy of this period with the specified values altered.
   * <p>
   * This method operates on each unit in the input in turn.
   * If this period already contains an amount for the unit then the amount
   * is replaced. Otherwise, the unit-amount pair is added.
   * <p>
   * For example, '6 Years, 7 Months' with '2 Months 3 Days' will return
   * '6 Years, 2 Months, 3 Days'.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to merge over this period, not null
   * @return a {@code PeriodFields} based on this period with the specified period overlaid, never null
   */
  def `with`(periodProvider: PeriodProvider): PeriodFields = {
    val periods: PeriodFields = of(periodProvider)
    if (this == Zero) periods
    else if (periods == Zero) this
    else {
      create(unitFieldMap ++ periods.unitFieldMap)
    }
  }

  /**
   * Checks if this period is fully positive, including zero.
   * <p>
   * This checks whether all the amounts in this period are positive,
   * defined as greater than or equal to zero.
   *
   * @return true if this period is fully positive including zero
   */
  def isPositiveOrZero: Boolean = {
    if (unitFieldMap.valuesIterator.exists(_.getAmount < 0)) false
    else true
  }

  /**
   * Gets the amount of this period for the specified unit.
   * <p>
   * This method allows the amount to be queried by unit, like a map.
   * If the unit is not found then zero is returned.
   *
   * @param unit the unit to query, not null
   * @return the period amount, 0 if no period stored for the unit
   * @throws CalendricalException if there is no amount for the unit
   */
  def getAmount(unit: PeriodUnit): Long = {
    var field: PeriodField = get(unit)
    if (field == null) 0
    else field.getAmount
  }

  /**
   * Returns a copy of this period with the specified period subtracted.
   * <p>
   * The result will contain the units and amounts from this period minus the
   * specified unit and amount.
   * The specified unit will always be in the result even if the amount is zero.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param amount the amount to subtract, measured in the specified unit, positive or negative
   * @param unit the unit defining the amount, not null
   * @return a {@code PeriodFields} based on this period with the specified period subtracted, never null
   * @throws ArithmeticException if the calculation overflows
   */
  def minus(amount: Long, unit: PeriodUnit): PeriodFields = {
    checkNotNull(unit, "PeriodRule must not be null")
    if (amount == 0 && contains(unit)) this
    else {
      val old: PeriodField = unitFieldMap(unit)
      create((unitFieldMap.updated(unit, if (old != null) old - amount else PeriodField.of(amount, unit).negated)))
    }
  }

  /**
   * Returns a string representation of the period, such as '[6 Days, 13 Hours]'.
   *
   * @return a descriptive representation of the period, not null
   */
  override def toString: String = {
    if (unitFieldMap.size == 0) {
      return "[]"
    }
    val buf: StringBuilder = new StringBuilder
    buf.append('[')
    for (field <- this) {
      buf.append(field.toString).append(',').append(' ')
    }
    buf.setLength(buf.length - 2)
    buf.append(']')
    return buf.toString
  }

  /**
   * Estimates the duration of this period.
   * <p>
   * Each {@link PeriodUnit} contains an estimated duration for that unit.
   * The per-unit estimate allows an estimate to be calculated for the whole period
   * including fields of variable duration. The estimate will equal the
   * {@link #toDuration accurate} calculation if all the fields are based on seconds.
   *
   * @return the estimated duration of this period, never null
   * @throws ArithmeticException if the calculation overflows
   */
  def toEstimatedDuration: Duration = {
    this.foldLeft(Duration.Zero)(_ + _.toEstimatedDuration)
    //    var dur: Duration = Duration.Zero
    //    for (field <- this) {
    //      dur = dur.plus(field.toEstimatedDuration)
    //    }
    //    return dur
  }

  /**
   * Returns a copy of this period with the specified period added.
   * <p>
   * The result will contain the units and amounts from this period plus the
   * specified unit and amount.
   * The specified unit will always be in the result even if the amount is zero.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param amount the amount to add, measured in the specified unit, positive or negative
   * @param unit the unit defining the amount, not null
   * @return a {@code PeriodFields} based on this period with the specified period added, never null
   * @throws ArithmeticException if the calculation overflows
   */
  def plus(amount: Long, unit: PeriodUnit): PeriodFields = {
    checkNotNull(unit, "PeiodRule must not be null")
    if (amount == 0 && contains(unit)) this
    else {
      val old: PeriodField = unitFieldMap(unit)
      create((unitFieldMap.updated(unit, if (old != null) old + amount else PeriodField.of(amount, unit))))
    }
    //    val copy: TreeMap[PeriodUnit, PeriodField] = clonedMap
    //    val old: PeriodField = copy(unit)
    //    val field: PeriodField = (if (old != null) old.plus(amount) else PeriodField.of(amount, unit))
    //    copy(unit) = field
    //    return create(copy)
  }

  /**
   * Checks if this period is zero-length.
   * <p>
   * This checks whether all the amounts in this period are zero.
   *
   * @return true if this period is zero-length
   */
  def isZero: Boolean = !unitFieldMap.valuesIterator.exists(_.isZero)

  //  {
  //    for (field <- unitFieldMap.valuesIterator) {
  //      if (field.isZero == false) {
  //        return false
  //      }
  //    }
  //    return true
  //  }

  /**
   * Returns a copy of this period with each amount in this period multiplied
   * by the specified scalar.
   *
   * @param scalar the scalar to multiply by, not null
   * @return a {@code PeriodFields} based on this period with the amounts multiplied by the scalar, never null
   * @throws ArithmeticException if the calculation overflows
   */
  def multipliedBy(scalar: Long): PeriodFields = {
    if (scalar == 1 || isZero) this
    //    else create(this.map(_ * scalar))
    else throw new Exception("Not implemented!")

    //    for (field <- this) {
    //      copy(field.getUnit) = field.multipliedBy(scalar)
    //    }
    //    return create(copy)
  }

  def *(scalar: Long): PeriodFields = multipliedBy(scalar)

  /**
   * Checks whether this period contains an amount for the unit.
   *
   * @param unit the unit to query, null returns false
   * @return true if the map contains an amount for the unit
   */
  def contains(unit: PeriodUnit): Boolean = unitFieldMap.keysIterator.contains(unit)

  /**
   * Gets the period for the specified unit.
   * <p>
   * This method allows the period to be queried by unit, like a map.
   * If the unit is not found then {@code null} is returned.
   *
   * @param unit the unit to query, not null
   * @return the period, null if no period stored for the unit
   */
  def get(unit: PeriodUnit): PeriodField = {
    checkNotNull(unit, "PeriodUnit must not be null")
    return unitFieldMap(unit)
  }

  /**
   * Returns a copy of this period with all zero amounts removed.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @return a {@code PeriodFields} based on this period with zero amounts removed, never null
   */
  def withZeroesRemoved: PeriodFields = {
    if (isZero) return Zero
    else create(unitFieldMap.filter(e => {
      val (k, v) = e;
      v.isZero
    }))

    //    val copy: TreeMap[PeriodUnit, PeriodField] = clonedMap
    //    val it: Iterator[PeriodField] = copy.values.iterator
    //    while (it.hasNext) {
    //      if (it.next.isZero) it.remove
    //    }
    //    return create(copy)
  }

  /**
   * Iterates through all the single-unit periods in this period.
   * <p>
   * This method fulfills the {@link Iterable} interface and allows looping
   * around the contained single-unit periods using the for-each loop.
   *
   * @return an iterator over the single-unit periods in this period, never null
   */
  def iterator: Iterator[PeriodField] = unitFieldMap.values.iterator

  /**
   * Returns a copy of this period with the specified period added.
   * <p>
   * The returned period will take each unit in the provider and add the value
   * to the amount already stored in this period, returning a new one.
   * If this period does not contain an amount for the unit then the unit and
   * amount are simply returned directly in the result. The result will have
   * the union of the units in this instance and the units in the specified instance.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to add, not null
   * @return a {@code PeriodFields} based on this period with the specified period added, never null
   * @throws ArithmeticException if the calculation overflows
   */
  def plus(periodProvider: PeriodProvider): PeriodFields = {
    val periods: PeriodFields = of(periodProvider)
    if (this == Zero) periods
    else {
      throw new Exception("Not implemented!")
    }

    //    val copy: TreeMap[PeriodUnit, PeriodField] = unitFieldMap
    //    for (period <- periods.unitFieldMap.values) {
    //      val old: PeriodField = copy(period.getUnit)
    //      val newPeriod = (if (old != null) old + period else period)
    //      copy(newPeriod.getUnit) = newPeriod
    //    }
    //    return create(copy)
  }

  def +(periodProvider: PeriodProvider): PeriodFields = plus(periodProvider)

  /**
   * Returns a copy of this period with each amount in this period negated.
   *
   * @return a {@code PeriodFields} based on this period with the amounts negated, never null
   * @throws ArithmeticException if the calculation overflows
   */
  def negated: PeriodFields = multipliedBy(-1)

  def unary_- : PeriodFields = multipliedBy(-1)

  /**
   * Gets the amount of this period for the specified unit converted
   * to an {@code int}.
   * <p>
   * This method allows the amount to be queried by unit, like a map.
   * If the unit is not found then zero is returned.
   *
   * @param unit the unit to query, not null
   * @return the period amount, 0 if no period stored for the unit
   * @throws CalendricalException if there is no amount for the unit
   * @throws ArithmeticException if the amount is too large to be returned in an int
   */
  def getAmountInt(unit: PeriodUnit): Int = {
    val field: PeriodField = get(unit)
    if (field == null) {
      return 0
    }
    return field.getAmountInt
  }

  /**
   * Returns the hash code for this period.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = unitFieldMap.hashCode

  /**
   * Calculates the accurate duration of this period.
   * <p>
   * The conversion is based on the {@code ISOChronology} definition of the seconds and
   * nanoseconds units. If all the fields in this period can be converted to either seconds
   * or nanoseconds then the conversion will succeed, subject to calculation overflow.
   * If any field cannot be converted to these fields above then an exception is thrown.
   *
   * @return the duration of this period based on {@code ISOChronology} fields, never null
   * @throws ArithmeticException if the calculation overflows
   */
  def toDuration: Duration = {
    val period: PeriodFields = toEquivalent(ISOChronology.periodSeconds, ISOChronology.periodNanos)
    Duration.ofSeconds(period.getAmount(ISOChronology.periodSeconds), period.getAmount(ISOChronology.periodNanos))
  }

  /**
   * Returns the size of the set of units in this period.
   * <p>
   * This returns the number of different units that are stored.
   *
   * @return number of unit-amount pairs
   */
  override def size: Int = unitFieldMap.size

  /**
   * Returns a copy of this period with the specified unit removed.
   * <p>
   * If this period already contains an amount for the unit then the amount
   * is removed. Otherwise, no action occurs.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param unit the unit to remove, not null
   * @return a {@code PeriodFields} based on this period with the specified unit removed, never null
   */
  def without(unit: PeriodUnit): PeriodFields = {
    checkNotNull(unit, "PeriodUnit must not be null")
    if (unitFieldMap.contains(unit) == false) this
    else create(unitFieldMap - (unit))
  }

  /**
   * Checks if this instance equal to the specified period.
   * <p>
   * Two {@code PeriodFields} instances are equal if all the contained
   * {@code PeriodField} instances are equal.
   *
   * @param obj the other period to compare to, null returns false
   * @return true if this instance is equal to the specified period
   */
  override def equals(obj: Any): Boolean = {
    obj match {
      case other: PeriodFields => (this eq other) || unitFieldMap == other.unitFieldMap
      case _ => false
    }
  }

  /**
   * Clone the internal data storage map.
   *
   * @return the cloned map, never null
   */
  //private def clonedMap: TreeMap[PeriodUnit, PeriodField] = unitFieldMap.clone.asInstanceOf[TreeMap[PeriodUnit, PeriodField]]

  /**
   * Resolves singletons.
   *
   * @return the resolved instance
   */
  private def readResolve: AnyRef = {
    if (unitFieldMap.size == 0) Zero
    else this
  }

  /**
   * Returns a copy of this period with the modular division remainder of each field
   * calculated with respect to the specified period.
   * <p>
   * This method will return a new period where every field represents a period less
   * than the specified period. If this period contains a period that cannot be converted
   * to the specified unit then an exception is thrown.
   * <p>
   * For example, if this period is '37 Hours, 7 Minutes' and the specified period is
   * '24 Hours' then the output will be '13 Hours, 7 Minutes'.
   * <p>
   * This method requires this period to be convertible to the specified period.
   * To ensure this is true, call {@link #retainConvertible}, with the base unit of the
   * period passed into this method, before calling this method.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param period the period to calculate the remainder against, not null
   * @return a {@code PeriodFields} based on this period with the remainder, never null
   * @throws CalendricalException if any field cannot be converted to the unit of the period
   */
  def remainder(period: PeriodField): PeriodFields = {
    checkNotNull(period, "PeriodField must not be null")
    val copy: TreeMap[PeriodUnit, PeriodField] = createMap
    for (loopField <- unitFieldMap.values) {
      if (loopField.getUnit.equals(period.getUnit)) {
        copy.updated(loopField.getUnit, loopField.remainder(period.getAmount))
      }
      else {
        for (equivalent <- period.getUnit.getEquivalentPeriods) {
          if (loopField.getUnit.equals(equivalent.getUnit)) {
            copy.updated(loopField.getUnit, loopField.remainder(equivalent.getAmount))
          }
        }
      }
    }
    if (copy.size < size) {
      throw new CalendricalException("Unable to calculate remainder as some fields cannot be converted")
    }
    return create(copy)
  }

  /**
   * Returns a copy of this period with only those units that can be converted to
   * the specified units.
   * <p>
   * This method will return a new period where every field can be converted to one
   * of the specified units. In the result, each of the retained periods will have the
   * same amount as they do in this period - no conversion or normalization occurs.
   * <p>
   * For example, if this period is '2 Days, 5 Hours, 7 Minutes' and the specified
   * unit array contains 'Seconds' then the output will be '5 Hours, 7 Minutes'.
   * The 'Days' unit is not retained as it cannot be converted to 'Seconds'.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param units the units to retain, not altered, not null, no nulls
   * @return a {@code PeriodFields} based on this period with the specified units retained, never null
   */
  def retainConvertible(units: Seq[PeriodUnit]): PeriodFields = {
    checkNotNull(units, "PeriodUnit array must not be null")
    throw new Exception("Not implemented!")
    //    val copy: TreeMap[PeriodUnit, PeriodField] = clonedMap
    //
    //    {
    //      val it: Iterator[PeriodUnit] = copy.keySet.iterator
    //      while (it.hasNext) {
    //        val loopUnit: PeriodUnit = it.next
    //        for (unit <- units) {
    //          checkNotNull(unit, "PeriodUnit array must not contain null")
    //          if (loopUnit.isConvertibleTo(unit)) {
    //            //continue //todo: continue is not supported
    //          }
    //        }
    //        it.remove
    //      }
    //    } //todo: labels is not supported
    //    return create(copy)
  }

  /**
   * Totals this period in terms of a single unit.
   * <p>
   * This will take each of the stored {@code PeriodField} instances and
   * convert them to the specified unit. The result will be the total of these
   * converted periods.
   * <p>
   * For example, '3 Hours, 34 Minutes' can be totalled to minutes resulting
   * in '214 Minutes'.
   *
   * @param unit the unit to total in, not null
   * @return a period equivalent to the total of this period in a single unit, never null
   * @throws CalendricalException if this period cannot be converted to the unit
   * @throws ArithmeticException if the calculation overflows
   */
  def toTotal(unit: PeriodUnit): PeriodField = {
    checkNotNull(unit, "PeriodUnit must not be null")
    var result: PeriodField = null
    for (period <- unitFieldMap.values) {
      val equivalentPeriod = period.toEquivalent(unit)
      result = (if (result != null) result + equivalentPeriod else equivalentPeriod)
    }
    result
  }

  /**
   * Returns a {@code Map} equivalent to this period.
   * <p>
   * The map will connect the unit to the single field period.
   * The sort order is from largest unit to smallest unit.
   *
   * @return the map equivalent to this period, unmodifiable, never null
   */
  def toMap: TreeMap[PeriodUnit, PeriodField] = unitFieldMap

  /**
   * Converts this period to one containing only the units specified.
   * <p>
   * This converts this period to one measured in the specified units.
   * It operates by looping through the individual parts of this period,
   * converting each in turn to one of the specified units.
   * These converted periods are then combined to form the result.
   * <p>
   * No normalization is performed on the result.
   * This means that an amount in a smaller unit cannot be converted to an amount in a larger unit.
   * If you need to do this, call {@link #normalized()} before calling this method.
   * <p>
   * This method uses {@link PeriodField#toEquivalent ( PeriodUnit...)} and as such,
   * it is recommended to specify the units from largest to smallest.
   * <p>
   * For example, '3 Hours' can normally be converted to both minutes and seconds.
   * If the units array contains both 'Minutes' and 'Seconds', then the result will
   * be measured in whichever is first in the array.
   *
   * @param units the required unit array, not altered, not null, no nulls
   * @return a period equivalent to this period, never null
   * @throws CalendricalException if this period cannot be converted to any of the units
   * @throws ArithmeticException if the calculation overflows
   */
  def toEquivalent(units: PeriodUnit*): PeriodFields = {
    checkNotNull(units, "PeriodUnit array must not be null")
    val map: TreeMap[PeriodUnit, PeriodField] = createMap
    for (period <- unitFieldMap.values) {
      throw new Exception("Not implemented!")
      //      period = period.toEquivalent(units)
      //      val old: PeriodField = map(period.getUnit)
      //      period = (if (old != null) old.plus(period) else period)
      //      map(period.getUnit, period)
    }
    return (if (map.equals(unitFieldMap)) this else create(map))
  }

  /**
   * Returns a copy of this period with the specified units retained.
   * <p>
   * This method will return a new period that only has the specified units.
   * All units not present in the input will not be present in the result.
   * In most cases, the result will not be equivalent to this period.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param units the units to retain, not altered, not null, no nulls
   * @return a {@code PeriodFields} based on this period with the specified units retained, never null
   */
  def retain(units: Array[PeriodUnit]): PeriodFields = {
    checkNotNull(units, "PeriodUnit array must not be null")
    if (units.contains(null)) throw new NullPointerException("PeriodUnit array must not contain null")
    else create(unitFieldMap.filter({
      case (k, v) => units.contains(k)
    }))
    //    val copy: TreeMap[PeriodUnit, PeriodField] = clonedMap
    //    val unitList: List[PeriodUnit] = Arrays.asList(units: _*)
    //    if (unitList.contains(null)) {
    //      throw new NullPointerException("PeriodUnit array must not contain null")
    //    }
    //    copy.keySet.retainAll(unitList)
    //    return create(copy)
  }

  /**
   * Returns a copy of this period with the amounts normalized to the specified units.
   * <p>
   * This will normalize the period around the specified units.
   * The calculation examines each pair of units that have a fixed conversion factor.
   * Each pair is adjusted so that the amount in the smaller unit does not exceed
   * the amount of the fixed conversion factor.
   * At least one unit must be specified for this method to have any effect.
   * <p>
   * For example, a period of '2 Decades, 2 Years, 17 Months' normalized using
   * 'Years' and 'Months' will return '23 Years, 5 Months'.
   * <p>
   * Any part of this period that cannot be converted to one of the specified units
   * will be unaffected in the result.
   * <p>
   * The result will always contain all the specified units, even if they are zero.
   * The result will be equivalent to this period.
   *
   * @param units the unit array to normalize to, not altered, not null, no nulls
   * @return a period equivalent to this period with the amounts normalized, never null
   * @throws ArithmeticException if the calculation overflows
   */
  def normalizedTo(units: Array[PeriodUnit]): PeriodFields = {
    checkNotNull(units, "PeriodUnit array must not be null")
    var result: PeriodFields = this
    var targetUnits: TreeSet[PeriodUnit] = new TreeSet[PeriodUnit]()(implicitly[Ordering[PeriodUnit]].reverse)
    targetUnits ++= (units)
    for (loopUnit <- unitFieldMap.keySet) {
      breakable {
        for (targetUnit <- targetUnits) {
          if (targetUnits.contains(loopUnit) == false) {
            val conversion: PeriodField = loopUnit.getEquivalentPeriod(targetUnit)
            if (conversion != null) {
              val amount: Long = result.getAmount(loopUnit)
              result = (result + (conversion * amount)).without(loopUnit)
              break
            }
          }
        }
      }
    }
    {
      var process: Boolean = true
      while (process) {
        process = false
        for (targetUnit <- targetUnits) {
          for (loopUnit <- result.unitFieldMap.keySet) {
            if (targetUnit.equals(loopUnit) == false) {
              val conversion: PeriodField = targetUnit.getEquivalentPeriod(loopUnit)
              if (conversion != null) {
                val convertAmount: Long = conversion.getAmount
                val amount: Long = result.getAmount(loopUnit)
                if (amount >= convertAmount || amount <= -convertAmount) {
                  result = result.`with`(amount % convertAmount, loopUnit).plus(amount / convertAmount, targetUnit)
                  process = (units.length > 2)
                }
              }
            }
          }
          result = result.plus(0, targetUnit)
        }
      }
    }
    return result
  }

  /**
   * Checks if this period is fully positive, excluding zero.
   * <p>
   * This checks whether all the amounts in this period are positive,
   * defined as greater than zero.
   *
   * @return true if this period is fully positive excluding zero
   */
  def isPositive: Boolean = {
    for (field <- unitFieldMap.values) {
      if (field.getAmount <= 0) {
        return false
      }
    }
    return true
  }

  /**
   * Returns a copy of this period with the amounts normalized.
   * <p>
   * The calculation examines each pair of units in this period that have a fixed conversion factor.
   * Each pair is adjusted so that the amount in the smaller unit does not exceed
   * the amount of the fixed conversion factor.
   * <p>
   * For example, a period of '2 Decades, 2 Years, 17 Months' normalized using
   * 'Years' and 'Months' will return '23 Years, 5 Months'.
   * <p>
   * The result will always contain all the units present in this period, even if they are zero.
   * The result will be equivalent to this period.
   *
   * @return a period equivalent to this period with the amounts normalized, never null
   * @throws ArithmeticException if the calculation overflows
   */
  def normalized: PeriodFields = normalizedTo(unitFieldMap.keySet.toArray)

  /**
   * Returns a copy of this period with the specified amount for the unit.
   * <p>
   * If this period already contains an amount for the unit then the amount
   * is replaced. Otherwise, the unit-amount pair is added.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param amount the amount to store in terms of the unit, positive or negative
   * @param unit the unit to store not null
   * @return a {@code PeriodFields} based on this period with the specified period overlaid, never null
   */
  def `with`(amount: Long, unit: PeriodUnit): PeriodFields = {
    val existing: PeriodField = get(unit)
    if (existing != null && existing.getAmount == amount) this
    else create(unitFieldMap.updated(unit, PeriodField.of(amount, unit)))
  }

  /**
   * Returns a copy of this period with the specified period subtracted.
   * <p>
   * The returned period will take each unit in the provider and subtract the
   * value from the amount already stored in this period, returning a new one.
   * If this period does not contain an amount for the unit then the unit and
   * amount are simply returned directly in the result. The result will have
   * the union of the units in this instance and the units in the specified instance.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param periodProvider the period to subtract, not null
   * @return a {@code PeriodFields} based on this period with the specified period subtracted, never null
   * @throws ArithmeticException if the calculation overflows
   */
  def minus(periodProvider: PeriodProvider): PeriodFields = {
    val periods: PeriodFields = of(periodProvider)
    if (this == Zero) periods
    else throw new Exception("Not implemented!")
    //    val copy: TreeMap[PeriodUnit, PeriodField] = clonedMap
    //    for (period <- periods.unitFieldMap.values) {
    //      val old: PeriodField = copy.get(period.getUnit).orNull
    //      val newPeriod = (if (old != null) old - period else -period)
    //      copy.updated(newPeriod.getUnit, newPeriod)
    //    }
    //    return create(copy)
  }

  def -(periodProvider: PeriodProvider): PeriodFields = minus(periodProvider)

  /**
   * Returns a copy of this period with each amount in this period divided
   * by the specified value.
   *
   * @param divisor the value to divide by, not null, not zero
   * @return a {@code PeriodFields} based on this period with the amounts divided by the divisor, never null
   * @throws ArithmeticException if dividing by zero
   */
  def dividedBy(divisor: Long): PeriodFields = {
    if (divisor == 0) {
      throw new ArithmeticException("Cannot divide by zero")
    }
    if (divisor == 1 || isZero) {
      return this
    }
    //    val copy: TreeMap[PeriodUnit, PeriodField] = createMap
    //    for (field <- this) {
    //      copy.put(field.getUnit, field / divisor)
    //    }
    else throw new Exception("Not implemented!")
//    return create(this.map(e => (e.getUnit, e / divisor)))
  }

  def /(divisor: Long): PeriodFields = dividedBy(divisor)
}