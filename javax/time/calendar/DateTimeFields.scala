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

import java.io.Serializable
import java.util.Collections
import java.util.Iterator
import java.util.Map
import java.util.SortedMap
import java.util.TreeMap

/**
 * A set of date-time fields.
 * <p>
 * Instances of this class store a map of field-value pairs.
 * Together these specify constraints on the dates and times that match.
 * For example, if an instance stores 'DayOfMonth=13' and 'DayOfWeek=Friday'
 * then it represents and matches only dates of Friday the Thirteenth.
 * <p>
 * All the values will be within the valid range for the field.
 * However, there is no cross validation between fields.
 * Thus, it is possible for the date-time represented to never exist.
 * For example, if an instance stores 'DayOfMonth=31' and 'MonthOfYear=February'
 * then there will never be a matching date.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object DateTimeFields {
  /**
   * Obtains an instance of {@code DateTimeFields} from two field-value pairs.
   * <p>
   * This factory allows the creation of a fields object with two field-value pairs.
   * Each value must be within the valid range for that field.
   * <p>
   * The two fields are not cross-validated. Thus, you can specify MonthOfYear of June
   * and DayOfMonth of 31, which is a date that can never occur.
   *
   * @param fieldRule1 the first rule, not null
   * @param value1 the first field value
   * @param fieldRule2 the second rule, not null
   * @param value2 the second field value
   * @return the fields instance, never null
   * @throws NullPointerException if either field rule is null
   * @throws IllegalCalendarFieldValueException if either value is invalid
   */
  def of(fieldRule1: DateTimeFieldRule[_], value1: Int, fieldRule2: DateTimeFieldRule[_], value2: Int): DateTimeFields = {
    ISOChronology.checkNotNull(fieldRule1, "First DateTimeFieldRule must not be null")
    ISOChronology.checkNotNull(fieldRule2, "Second DateTimeFieldRule must not be null")
    fieldRule1.checkValue(value1)
    fieldRule2.checkValue(value2)
    val map: TreeMap[DateTimeFieldRule[_], Int] = createMap
    map.put(fieldRule1, value1)
    map.put(fieldRule2, value2)
    return new DateTimeFields(map)
  }

  /**
   * Obtains an instance of {@code DateTimeFields} from a map of field-value pairs.
   * <p>
   * This factory allows the creation of a fields object from a map of field-value pairs.
   * Each value must be within the valid range for that field.
   * <p>
   * The fields are not cross-validated. Thus, you can specify MonthOfYear of June
   * and DayOfMonth of 31, which is a date that can never occur.
   *
   * @param fieldValueMap a map of fields that will be used to create a field set,
   *  not updated by this factory, not null, contains no nulls
   * @return the fields instance, never null
   * @throws NullPointerException if the map contains null keys or values
   * @throws IllegalCalendarFieldValueException if any value is invalid
   */
  def of(fieldValueMap: Map[DateTimeFieldRule[_], Int]): DateTimeFields = {
    ISOChronology.checkNotNull(fieldValueMap, "Field-value map must not be null")
    if (fieldValueMap.isEmpty) {
      return Empty
    }
    val map: TreeMap[DateTimeFieldRule[_], Int] = createMap
    for (entry <- fieldValueMap.entrySet) {
      val fieldRule: DateTimeFieldRule[_] = entry.getKey
      val value: Int = entry.getValue
      ISOChronology.checkNotNull(fieldRule, "Null keys are not permitted in field-value map")
      ISOChronology.checkNotNull(value, "Null values are not permitted in field-value map")
      fieldRule.checkValue(value)
      map.put(fieldRule, value)
    }
    return new DateTimeFields(map)
  }

  /**
   * Obtains an instance of {@code DateTimeFields} from a field-value pair.
   * <p>
   * This factory allows the creation of a fields object with a single field-value pair.
   * The value must be within the valid range for the field.
   *
   * @param fieldRule the rule, not null
   * @param value the field value, may be invalid
   * @return the fields instance, never null
   * @throws NullPointerException if the field rule is null
   * @throws IllegalCalendarFieldValueException if the value is invalid
   */
  def of(fieldRule: DateTimeFieldRule[_], value: Int): DateTimeFields = {
    ISOChronology.checkNotNull(fieldRule, "DateTimeFieldRule must not be null")
    fieldRule.checkValue(value)
    val map: TreeMap[DateTimeFieldRule[_], Int] = createMap
    map.put(fieldRule, value)
    return new DateTimeFields(map)
  }

  /**
   * Creates a new empty map.
   *
   * @return ordered representation of internal map
   */
  private def createMap: TreeMap[DateTimeFieldRule[_], Int] = {
    return new TreeMap[DateTimeFieldRule[_], Int](Collections.reverseOrder)
  }

  /**
   * A singleton empty {@code DateTimeFields }, placing no restrictions on the date-time.
   */
  val Empty: DateTimeFields = new DateTimeFields(createMap)
}

/**
 * Constructor.
 *
 * @param fieldValueMap the map of fields, which is assigned, not null
 */

@SerialVersionUID(1L)
final class DateTimeFields private(val fieldValueMap: TreeMap[DateTimeFieldRule[_], Int])
  extends Calendrical with CalendricalMatcher with Iterable[DateTimeFieldRule[_]] with Serializable {

  import DateTimeFields._

  /**
   * Checks if this object contains a mapping for the specified field.
   * <p>
   * This method returns true if a value can be obtained for the specified field.
   *
   * @param fieldRule the field to query, null returns false
   * @return true if the field is supported, false otherwise
   */
  def contains(fieldRule: DateTimeFieldRule[_]): Boolean = fieldRule != null && fieldValueMap.containsKey(fieldRule)

  /**
   * Returns a copy of this DateTimeFields with the specified field value.
   * <p>
   * If this instance already has a value for the field then the value is replaced.
   * Otherwise the value is added to the map.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param fieldRule the field to set in the returned object, not null
   * @param value the value to set in the returned set of fields
   * @return a new, updated DateTimeFields, never null
   * @throws NullPointerException if DateTimeFieldRule is null
   * @throws IllegalCalendarFieldValueException if the value is invalid
   */
  def `with`(fieldRule: DateTimeFieldRule[_], value: Int): DateTimeFields = {
    ISOChronology.checkNotNull(fieldRule, "DateTimeFieldRule must not be null")
    fieldRule.checkValue(value)
    val clonedMap: TreeMap[DateTimeFieldRule[_], Int]= clonedMap
    clonedMap.put(fieldRule, value)
    return new DateTimeFields(clonedMap)
  }

  /**
   * Converts this object to a map of fields to values.
   * <p>
   * The returned map will never be null, however it may be empty.
   * It is independent of this object - changes will not be reflected back.
   *
   * @return an independent, modifiable copy of the field-value map, never null
   */
  def toFieldValueMap: SortedMap[DateTimeFieldRule[_], Int] = new TreeMap[DateTimeFieldRule[_], Int](fieldValueMap)

  /**
   * Gets the value for the specified field throwing an exception if the
   * field is not in the field-value map.
   * <p>
   * The value will be within the valid range for the field.
   * <p>
   * No attempt is made to derive values. The result is simply based on
   * the contents of the stored field-value map. If you want to derive a
   * value then use {@link #get} or a {@link CalendricalMerger}.
   *
   * @param rule the rule to query from the map, not null
   * @return the value mapped to the specified field
   * @throws UnsupportedRuleException if the field is not in the map
   */
  def getInt(rule: DateTimeFieldRule[_]): Int = {
    ISOChronology.checkNotNull(rule, "DateTimeFieldRule must not be null")
    var value: Int = fieldValueMap.get(rule)
    if (value == null) throw new UnsupportedRuleException(rule)
    else value
  }

  /**
   * Checks if the fields in this object match those in the specified calendrical.
   * <p>
   * This implementation checks that all calendrical fields in this object match.
   *
   * @param calendrical the calendrical to match, not null
   * @return true if the calendrical fields match, false otherwise
   */
  override def matchesCalendrical(calendrical: Calendrical): Boolean = {
    ISOChronology.checkNotNull(calendrical, "Calendrical must not be null")
    for (entry <- fieldValueMap.entrySet) {
      val dateValue: Int = entry.getKey.getInteger(calendrical)
      if (dateValue != null && dateValue.equals(entry.getValue) == false) {
        return false
      }
    }
    return true
  }

  /**
   * A hash code for these fields.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = fieldValueMap.hashCode

  /**
   * Returns a copy of this object with the specified field removed.
   * <p>
   * If this instance does not contain the field then the returned instance
   * is the same as this one.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param fieldRule the field to remove from the returned object, not null
   * @return a new, updated DateTimeFields, never null
   */
  def withFieldRemoved(fieldRule: DateTimeFieldRule[_]): DateTimeFields = {
    ISOChronology.checkNotNull(fieldRule, "DateTimeFieldRule must not be null")
    val clonedMap: TreeMap[DateTimeFieldRule[_], Int] = clonedMap
    if (clonedMap.remove(fieldRule) == null) {
      return this
    }
    return if (clonedMap.isEmpty) Empty else new DateTimeFields(clonedMap)
  }

  /**
   * Clones the field-value map.
   *
   * @return a clone of the field-value map, never null
   */
  private def clonedMap: TreeMap[DateTimeFieldRule[_], Int] = {
    val cloned: TreeMap[DateTimeFieldRule[_], Int] = createMap
    cloned.putAll(fieldValueMap)
    cloned
  }

  /**
   * Returns a copy of this DateTimeFields with the specified fields added.
   * <p>
   * If this instance already has a value for the field then the value is replaced.
   * Otherwise the value is added.
   * <p>
   * This instance is immutable and unaffected by this method call.
   *
   * @param fields the fields to add to the returned object, not null
   * @return a new, updated DateTimeFields, never null
   */
  def `with`(fields: DateTimeFields): DateTimeFields = {
    ISOChronology.checkNotNull(fields, "DateTimeFields must not be null")
    if (fields.size == 0 || fields == this) {
      return this
    }
    val clonedMap: TreeMap[DateTimeFieldRule[_], Int] = clonedMap
    clonedMap.putAll(fields.fieldValueMap)
    return new DateTimeFields(clonedMap)
  }

  /**
   * Gets the value of the specified calendrical rule.
   * <p>
   * This method queries the value of the specified calendrical rule.
   * If the value cannot be returned for the rule from this instance then
   * an attempt is made to derive the value.
   * If that fails, {@code null} will be returned.
   *
   * @param rule the rule to use, not null
   * @return the value for the rule, null if the value cannot be returned
   */
  def get[T](rule: CalendricalRule[T]): Option[T] = {
    ISOChronology.checkNotNull(rule, "CalendricalRule must not be null")
    if (rule.isInstanceOf[DateTimeFieldRule[_]]) {
      val value: Int = fieldValueMap.get(rule)
      if (value != null) {
        val r: DateTimeFieldRule[T] = rule.asInstanceOf[DateTimeFieldRule[T]]
        return Some(r.convertIntToValue(value))
      }
    }
    return rule.deriveValueFrom(this)
  }

  /**
   * Returns the size of the map of fields to values.
   * <p>
   * This method returns the number of field-value pairs stored.
   *
   * @return number of field-value pairs, zero or greater
   */
  def size: Int = fieldValueMap.size

  /**
   * Ensure EMPTY singleton.
   *
   * @return the resolved instance
   * @throws ObjectStreamException if an error occurs
   */
  private def readResolve: AnyRef = if (fieldValueMap.isEmpty) DateTimeFields.Empty else this

  /**
   * Iterates through all the field rules.
   * <p>
   * This method fulfills the {@link Iterable} interface and allows looping
   * around the fields using the for-each loop. The values can be obtained using
   * { @link # get} or {@link # getInt }.
   *
   * @return an iterator over the fields in this object, never null
   */
  def iterator: Iterator[DateTimeFieldRule[_]] = fieldValueMap.keySet.iterator

  /**
   * Gets the value for the specified field quietly returning null
   * if the field is not in the field-value map.
   * <p>
   * The value will be within the valid range for the field.
   *
   * @param fieldRule the rule to query from the map, null returns null
   * @return the value mapped to the specified field, null if not present
   */
  def getQuiet(fieldRule: DateTimeFieldRule[_]): Int = if (fieldRule == null) null else fieldValueMap.get(fieldRule)

  /**
   * Is this object equal to the specified object.
   * <p>
   * This compares the map of field-value pairs.
   *
   * @param obj the other fields to compare to, null returns false
   * @return true if this instance is equal to the specified field set
   */
  override def equals(obj: AnyRef): Boolean = {
    if (obj == this) {
      return true
    }
    if (obj.isInstanceOf[DateTimeFields]) {
      val other: DateTimeFields = obj.asInstanceOf[DateTimeFields]
      return fieldValueMap.equals(other.fieldValueMap)
    }
    return false
  }

  /**
   * Outputs the fields as a {@code String }.
   * <p>
   * The output will consist of the field-value map in standard map format.
   *
   * @return the formatted date-time string, never null
   */
  override def toString: String = fieldValueMap.toString
}