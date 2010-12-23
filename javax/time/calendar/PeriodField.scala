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

import java.util.Arrays
import javax.time.CalendricalException
import javax.time.Duration
import javax.time.MathUtils

/**
 * A period of time measured using a single unit, such as '3 Days' or '65 Seconds'.
 * <p>
 * {@code PeriodField} is an immutable period that stores an amount of human-scale
 * time for a single unit. For example, humans typically measure periods of time
 * in units of years, months, days, hours, minutes and seconds. These concepts are
 * defined by instances of {@link PeriodUnit} in the chronology classes. This class
 * allows an amount to be specified for one of the units, such as '3 Days' or '65 Seconds'.
 * <p>
 * Basic mathematical operations are provided - plus(), minus(), multipliedBy(),
 * dividedBy(), negated() and abs(), all of which return a new instance.
 * <p>
 * {@code PeriodField} can store rules of any kind which makes it usable with
 * any calendar system.
 * <p>
 * PeriodField is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object PeriodField {
  /**
   * Obtains a {@code PeriodField} from an amount and unit.
   * <p>
   * The parameters represent the two parts of a phrase like '6 Days'.
   *
   * @param amount the amount of the period, measured in terms of the unit, positive or negative
   * @param unit the unit that the period is measured in, not null
   * @return the {@code PeriodField} instance, never null
   */
  def of(amount: Long, unit: PeriodUnit): PeriodField = {
    PeriodFields.checkNotNull(unit, "PeriodUnit must not be null")
    new PeriodField(amount, unit)
  }
}


/**
 * Constructor.
 *
 * @param amount the amount of the period, measured in terms of the unit, positive or negative
 * @param unit the unit that the period is measured in, validated not null
 */
@SerialVersionUID(1L)
final class PeriodField private(val amount: Long, val unit: PeriodUnit) extends PeriodProvider with Comparable[PeriodField] with Serializable {
  /**
   * Gets the amount of this period, converted to an {@code int}.
   * <p>
   * For example, in the period '5 Days', the amount is '5'.
   *
   * @return the amount of time of this period, positive or negative
   * @throws ArithmeticException if the amount exceeds the capacity of an {@code int }
   */
  def getAmountInt: Int = MathUtils.safeToInt(amount)

  /**
   * Gets the unit of this period.
   * <p>
   * For example, in the period '5 Days', the unit is 'Days'.
   *
   * @return the period unit, never null
   */
  def getUnit: PeriodUnit = unit

  /**
   * Returns a copy of this period with the amount multiplied by the specified scalar.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param scalar the value to multiply by, positive or negative
   * @return a {@code PeriodField} based on this period multiplied by the specified scalar, never null
   * @throws ArithmeticException if the calculation overflows
   */
  def multipliedBy(scalar: Long): PeriodField = withAmount(MathUtils.safeMultiply(amount, scalar))

  def *(scalar: Long): PeriodField = multipliedBy(scalar)

  /**
   * Returns a copy of this period with a different unit.
   * <p>
   * Calling this method returns a new period with the same amount but different unit.
   * For example, it could be used to change '3 Days' to '3 Months'.
   *
   * @param unit the unit to set in the returned period, positive or negative
   * @return a {@code PeriodField} based on this period with the specified unit, never null
   */
  def withUnit(unit: PeriodUnit): PeriodField = {
    PeriodFields.checkNotNull(unit, "PeriodUnit must not be null")
    if (unit.equals(this.unit)) this
    else new PeriodField(amount, unit)
  }

  /**
   * Returns a copy of this period with the amount negated.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @return a {@code PeriodField} based on this period with the amount negated, never null
   * @throws ArithmeticException if the amount is {@code Long.MIN_VALUE }
   */
  def negated: PeriodField = withAmount(MathUtils.safeNegate(amount))

  /**
   * Returns a copy of this period with the amount as the remainder following
   * division by the specified divisor.
   * <p>
   * This uses the {@code %} operator to provide the result, which may be negative.
   * For example, the remainder of '11 Days' divided by 4 is '3 Days'.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param divisor the value to divide by, positive or negative
   * @return a {@code PeriodField} based on this period divided by the specified divisor, never null
   * @throws ArithmeticException if the divisor is zero
   */
  def remainder(divisor: Long): PeriodField = withAmount(amount % divisor)

  /**
   * Checks if this period is zero length.
   * <p>
   * A {@code PeriodField} can be positive, zero or negative.
   * This method checks whether the length is zero.
   *
   * @return true if this period is zero length
   */
  def isZero: Boolean = amount == 0

  /**
   * Gets the amount of this period.
   * <p>
   * For example, in the period '5 Days', the amount is '5'.
   *
   * @return the amount of time of this period, positive or negative
   */
  def getAmount: Long = amount

  /**
   * Returns a copy of this period with a positive amount.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @return a {@code PeriodField} based on this period with an absolute amount, never null
   * @throws ArithmeticException if the amount is {@code Long.MIN_VALUE }
   */
  def abs: PeriodField = if (amount < 0) negated else this

  /**
   * Compares this period to the specified period.
   * <p>
   * The comparison orders first by the unit, then by the amount.
   *
   * @param otherPeriod the other period to compare to, not null
   * @return the comparator value, negative if less, positive if greater
   */
  def compareTo(otherPeriod: PeriodField): Int = {
    val cmp: Int = unit.compareTo(otherPeriod.unit)
    if (cmp != 0) cmp
    else MathUtils.safeCompare(amount, otherPeriod.amount)
  }

  /**
   * Converts this period to an equivalent in the specified unit.
   * <p>
   * This converts this period to one measured in the specified unit.
   * This uses {@link PeriodUnit#getEquivalentPeriod ( PeriodUnit )} to lookup
   * the equivalent period for the unit.
   * <p>
   * For example, '3 Hours' could be converted to '180 Minutes'.
   * <p>
   * This method is equivalent to {@link #toEquivalent ( PeriodUnit...)} with a single parameter.
   *
   * @param requiredUnit the unit to convert to, not null
   * @return a period equivalent to this period, never null
   * @throws CalendricalException if this period cannot be converted to the specified unit
   * @throws ArithmeticException if the calculation overflows
   */
  def toEquivalent(requiredUnit: PeriodUnit): PeriodField = {
    val equivalent: PeriodField = unit.getEquivalentPeriod(requiredUnit)
    if (equivalent != null) {
      return equivalent.multipliedBy(amount)
    }
    throw new CalendricalException("Unable to convert " + getUnit + " to " + requiredUnit)
  }

  /**
   * Returns a copy of this period with the specified period subtracted.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param amount the period to subtract, measured in the unit of the period, positive or negative
   * @return a {@code PeriodField} based on this period with the specified amount subtracted, never null
   * @throws ArithmeticException if the calculation overflows
   */
  def minus(amount: Long): PeriodField = withAmount(MathUtils.safeSubtract(this.amount, amount))

  def -(amount: Long): PeriodField = minus(amount)

  /**
   * Converts this period to a {@code PeriodFields}.
   * <p>
   * The returned {@code PeriodFields} will always contain the unit even
   * if the amount is zero.
   *
   * @return the equivalent period, never null
   */
  override def toPeriodFields: PeriodFields = PeriodFields.of(this)

  /**
   * Calculates the accurate duration of this period.
   * <p>
   * The conversion is based on the {@code ISOChronology} definition of the seconds and
   * nanoseconds units. If the unit of this period can be converted to either seconds
   * or nanoseconds then the conversion will succeed, subject to calculation overflow.
   * If the unit cannot be converted then an exception is thrown.
   *
   * @return the duration of this period based on {@code ISOChronology} fields, never null
   * @throws ArithmeticException if the calculation overflows
   */
  def toDuration: Duration = {
    var equivalent: PeriodField = unit.getEquivalentPeriod(ISOChronology.periodSeconds)
    if (equivalent != null) {
      return equivalent.multipliedBy(amount).toEstimatedDuration
    }
    equivalent = unit.getEquivalentPeriod(ISOChronology.periodNanos)
    if (equivalent != null) {
      return equivalent.multipliedBy(amount).toEstimatedDuration
    }
    throw new CalendricalException("Unable to convert " + getUnit + " to a Duration")
  }

  /**
   * Checks if this period is equal to the specified period.
   * <p>
   * The comparison is based on the unit and amount.
   *
   * @param obj the object to check, null returns false
   * @return true if this period is the same as that specified
   */
  override def equals(obj: AnyRef): Boolean = {
    if (this eq obj) true
    else if (obj.isInstanceOf[PeriodField]) {
      val other: PeriodField = obj.asInstanceOf[PeriodField]
      this.amount == other.amount && this.unit.equals(other.unit)
    }
    else false
  }

  /**
   * Returns a copy of this period with the amount divided by the specified divisor.
   * <p>
   * This uses the {@code /} operator and integer division to provide the result.
   * For example, the result of '11 Days' divided by 4 is '2 Days'.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param divisor the value to divide by, positive or negative
   * @return a {@code PeriodField} based on this period divided by the specified divisor, never null
   * @throws ArithmeticException if the divisor is zero
   */
  def dividedBy(divisor: Long): PeriodField = withAmount(amount / divisor)

  def /(divisor: Long): PeriodField = dividedBy(divisor)

  /**
   * Converts this period to an equivalent in <i>one</i> of the units specified.
   * <p>
   * This converts this period to one measured in one of the specified units.
   * It operates by trying to convert to each unit in turn until one succeeds.
   * As such, it is recommended to specify the units from largest to smallest.
   * <p>
   * For example, '3 Hours' can normally be converted to both minutes and seconds.
   * If the units array contains both 'Minutes' and 'Seconds', then the result will
   * be measured in whichever is first in the array, either '180 Minutes' or '10800 Seconds'.
   *
   * @param requiredUnits the required unit array, not altered, not null, no nulls
   * @return a period equivalent to this period, never null
   * @throws CalendricalException if this period cannot be converted to any of the units
   * @throws ArithmeticException if the calculation overflows
   */
  def toEquivalent(requiredUnits: Array[PeriodUnit]): PeriodField = {
    for (requiredUnit <- requiredUnits) {
      val equivalent: PeriodField = unit.getEquivalentPeriod(requiredUnit)
      if (equivalent != null) {
        return equivalent.multipliedBy(amount)
      }
    }
    throw new CalendricalException( //FIXME
      "Unable to convert " + getUnit + " to any requested unit: " + Arrays.toString(requiredUnits.asInstanceOf[Array[AnyRef]]))
  }

  /**
   * Returns a copy of this period with the specified period added.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param amount the period to add, measured in the unit of the period, positive or negative
   * @return a {@code PeriodField} based on this period with the specified amount added, never null
   * @throws ArithmeticException if the calculation overflows
   */
  def plus(amount: Long): PeriodField = withAmount(MathUtils.safeAdd(this.amount, amount))

  def +(amount: Long): PeriodField = plus(amount)

  /**
   * Returns a string representation of this period, such as '6 Days'.
   * <p>
   * The format consists of the amount, followed by a space, followed by the unit name.
   *
   * @return a descriptive representation of this period, not null
   */
  override def toString: String = amount + " " + unit.getName

  /**
   * Returns a copy of this period with a different amount of time.
   * <p>
   * Calling this method returns a new period with the same unit but different amount.
   * For example, it could be used to change '3 Days' to '5 Days'.
   *
   * @param amount the amount of time to set in the returned period, positive or negative
   * @return a {@code PeriodField} based on this period with the specified amount, never null
   */
  def withAmount(amount: Long): PeriodField = {
    if (amount == this.amount) this
    else new PeriodField(amount, unit)
  }

  /**
   * Returns a copy of this period with the specified period added.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param period the period to add, positive or negative
   * @return a {@code PeriodField} based on this period with the specified period added, never null
   * @throws IllegalArgumetException if the specified period has a different unit
   * @throws ArithmeticException if the calculation overflows
   */
  def plus(period: PeriodField): PeriodField = {
    PeriodFields.checkNotNull(period, "PeriodField must not be null")
    if (period.getUnit.equals(unit) == false) {
      throw new IllegalArgumentException("Cannot add '" + period + "' to '" + this + "' as the units differ")
    }
    return plus(period.getAmount)
  }

  def +(period: PeriodField): PeriodField = plus(period)

  /**
   * Returns a copy of this period with the specified period subtracted.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param period the period to subtract, positive or negative
   * @return a {@code PeriodField} based on this period with the specified period subtracted, never null
   * @throws IllegalArgumetException if the specified has a different unit
   * @throws ArithmeticException if the calculation overflows
   */
  def minus(period: PeriodField): PeriodField = {
    PeriodFields.checkNotNull(period, "PeriodField must not be null")
    if (period.getUnit.equals(unit) == false) {
      throw new IllegalArgumentException("Cannot subtract '" + period + "' from '" + this + "' as the units differ")
    }
    return minus(period.getAmount)
  }

  def -(period: PeriodField): PeriodField = minus(period)

  /**
   * Estimates the duration of this period.
   * <p>
   * The {@link PeriodUnit} contains an estimated duration for that unit.
   * The value allows an estimate to be calculated for this period irrespective
   * of whether the unit is of fixed or variable duration. The estimate will equal the
   * {@link #toDuration accurate} calculation if the unit is based on the second.
   *
   * @return the estimated duration of this period, positive or negative
   * @throws ArithmeticException if the calculation overflows
   */
  def toEstimatedDuration: Duration = unit.getEstimatedDuration.multipliedBy(amount)

  /**
   * Returns the hash code for this period.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = unit.hashCode ^ (amount ^ (amount >>> 32)).toInt
}