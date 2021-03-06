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
package scalax.time.calendar

//import java.util.Iterator

import java.util.concurrent.ConcurrentHashMap
import scalax.time.CalendricalException
import collection.mutable.{ConcurrentMap, HashMap}

/**
 * Stateful class used to merge calendrical information.
 * <p>
 * This class is a tool for merging any set of calendrical information into the
 * most meaningful set of information. For example, separate year, month and day
 * fields will be merged into a date. And if both date and time are present, then
 * they will be merged into a date-time.
 * <p>
 * CalendricalMerger is mutable and not thread-safe.
 * It must only be used from a single thread and must not be passed between threads.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */

/**
 * Constructor.
 *
 * @param context the context to use, not null
 * @param inputMap the map of data to merge, not null
 */
final class CalendricalMerger(private var context: CalendricalContext) extends Calendrical {
  ISOChronology.checkNotNull(context, null)

  /**
   * Constructor.
   *
   * @param context the context to use, not null
   */
  def this(context: CalendricalContext, inputMap: HashMap[CalendricalRule[Any], Any]) {
    this (context)
    ISOChronology.checkNotNull(inputMap, null)
    this.inputMap ++= inputMap
  }

  /**
   * Removes any field from the processing map that can be derived from another field.
   */
  private def removeDerivable: Unit = {
    val derivableValues = processingMap.filter({
      case (k, v) => k.derive(this) != None
    })
    //  processingMap = processingMap -- derivableValues        //FIXME
  }

  /**
   * Gets the calendrical context in use for the merge.
   *
   * @return the calendrical context, never null
   */
  def getContext: CalendricalContext = context


  /**
   * Removes a rule and its value from the map being processed.
   * <p>
   * This method is called to remove a rule-value pair that can now be derived
   * from another item in the map following a merge.
   *
   * @param rule the rule to remove, not null
   */
  def removeProcessed(rule: CalendricalRule[_]): Unit = {
    ISOChronology.checkNotNull(rule, "CalendricalRule must not be null")
    processingMap.remove(rule)
  }

  /**
   * The map of potentially invalid data to being merged, never null.
   * This is a concurrent hash map mainly to gain the no-nulls implementation.
   */
  private val inputMap = collection.JavaConversions.JConcurrentMapWrapper(new ConcurrentHashMap[CalendricalRule[_], Any])

  /**
   * The overflow period to be added to the resultant date/time.
   */
  private var overflow: Period = Period.Zero
  /**
   * Current iterator, updated when the state of the map is changed.
   */
  private var iterator: Iterator[CalendricalRule[_]] = null
  /**
   * Stores a rule-value pair into this map ensuring that it does not clash
   * with any previous value defined for that rule.
   * <p>
   * This method adds the specified rule-value pair to the map.
   * If this instance already has a value for the rule then the value is checked
   * to see if it is the same with an exception being thrown if it is not.
   * If this instance does not hold the rule already, then the value is simply added.
   * <p>
   * The merged value should be within the valid range for the rule.
   *
   * @param rule the field to store, not null
   * @param value the value to store, not null
   * @throws CalendricalException if the input field does not match a previously stored field
   */
  def storeMerged[T](rule: CalendricalRule[T], value: T): Unit = {
    ISOChronology.checkNotNull(rule, "CalendricalRule must not be null")
    ISOChronology.checkNotNull(value, "Value must not be null")

    val oldValue: Option[T] = getValue(rule)
    if (oldValue.isDefined) {
      if (oldValue.get.equals(value) == false) {
        throw new InvalidCalendarFieldException("Merge resulted in two different values, " + value + " and " + oldValue.get + ", for " + rule.getID + " given input " + inputMap, rule);
      } else {
        return // no change
      }
    }
    processingMap.put(rule, value);
    iterator = processingMap.keySet.iterator // restart the iterator
  }

  /**
   * Interprets each value in the input map, converting to standard types.
   *
   * @throws CalendricalException if the value for any rule is invalid
   */
  private def interpret: Unit = {
    inputMap.foreach({
      case (k, v) => processingMap.put(k, k.interpretValue(this, v))
    })


    //    for (entry <- inputMap.entrySet) {
    //      val rule: CalendricalRule[_] = entry.getKey
    //      val value: AnyRef = rule.interpretValue(this, entry.getValue)
    //      processingMap.put(rule, value)
    //    }
  }

  /**{@inheritDoc}*/
  override def toString: String = {
    var str: String =
      if (processingMap.isEmpty && inputMap.size > 0) inputMap.toString
      else processingMap.toString
    if (overflow.isZero == false)
      str += "+" + overflow
    str
  }

  /**
   * Sets the calendrical context to use for the merge.
   * <p>
   * The context must only be updated before merging starts.
   * If the context is updated after merging starts, the result is undefined.
   *
   * @param context the calendrical context, not null
   */
  def setContext(context: CalendricalContext): Unit = {
    ISOChronology.checkNotNull(context, "CalendricalContext must not be null")
    this.context = context
  }

  /**
   * Gets the value of the specified calendrical rule from the merged result.
   * <p>
   * This method queries the value of the specified calendrical rule using
   * the merged rule-value map.
   * If the value cannot be returned for the rule from this date then
   * {@code null} will be returned.
   *
   * @param rule the rule to use, not null
   * @return the value for the rule, null if the value cannot be returned
   */
  def getValue[T](rule: CalendricalRule[T]): Option[T] = rule.reify(processingMap.get(rule))

  /**
   * Gets the underlying rule-value map that is being merged.
   * <p>
   * The map returned is the live data from this instance.
   * Updating the map will update the data held by the merger.
   * <p>
   * Values in this map may be invalid, for example the day-of-month may be
   * an invalid negative value, or the hour represented as a currency.
   * Some of these, like a negative day-of-month, may be capable of being
   * interpreted by a lenient merger. Others, like a currency, cannot.
   * <p>
   * The map must only be updated before merging starts.
   * If the map is updated after merging starts, the result is undefined.
   *
   * @return the rule-value map being merged, doesn't accept nulls, never null
   */
  def getInputMap: ConcurrentMap[CalendricalRule[_], Any] = inputMap

  /**
   * Gets the overflow that results from the merge.
   * <p>
   * When some sets of fields are merged, the result may include an overflow.
   * This is a period that should be added to a date-time to make the result whole.
   *
   * @param additionalOverflow the additional overflow to store, not null
   */
  def addToOverflow(additionalOverflow: Period): Unit = {
    if ((overflow.years != 0 && additionalOverflow.years != 0) ||
      (overflow.months != 0 && additionalOverflow.months != 0) ||
      (overflow.days != 0 && additionalOverflow.days != 0) ||
      (overflow.hours != 0 && additionalOverflow.hours != 0) ||
      (overflow.minutes != 0 && additionalOverflow.minutes != 0) ||
      (overflow.seconds != 0 && additionalOverflow.seconds != 0) ||
      (overflow.nanos != 0 && additionalOverflow.nanos != 0)) {
      throw new CalendricalException("Unable to complete merge as input contains two conflicting out of range values")
    }
    overflow = overflow.plus(additionalOverflow)
  }

  /**
   * Gets the overflow that results from the merge.
   * <p>
   * When some sets of fields are merged, the result may include an overflow.
   * This is a period that should be added to a date-time to make the result whole.
   *
   * @return the overflow resulting from the merge, never null
   */
  def getOverflow: Period = overflow

  /**
   * The map of in range data to be merged, never null.
   * This is a concurrent hash map mainly to gain the no-nulls implementation.
   */
  private var processingMap = collection.JavaConversions.JConcurrentMapWrapper(new ConcurrentHashMap[CalendricalRule[_], Any])

  /**
   * Gets the value of the specified calendrical rule from the merged result.
   * <p>
   * This method queries the value of the specified calendrical rule using
   * the merged rule-value map.
   * If the value cannot be returned for the rule from this date then
   * {@code null} will be returned.
   *
   * @param rule the rule to use, not null
   * @return the value for the rule, null if the value cannot be returned
   */
  def get[T](rule: CalendricalRule[T]): Option[T] = {
    ISOChronology.checkNotNull(rule, "CalendricalRule must not be null")
    getValue(rule) match {
      case None => rule.deriveValueFrom(this)
      case Some(value) => Some(value)
    }
  }

  /**
   * Merges the fields to extract the maximum possible date, time and offset information.
   * <p>
   * The merge process aims to extract the maximum amount of information
   * possible from this set of fields. Ideally the outcome will be a date, time
   * or both, however there may be insufficient information to achieve this.
   * <p>
   * The process repeatedly calls the field rule {@link CalendricalRule#merge merge }
   * method to perform the merge on each individual field. Sometimes two or
   * more fields will combine to form a more significant field. Sometimes they
   * will combine to form a date or time. The process stops when there no more
   * merges can occur.
   * <p>
   * The process is based around hierarchies that can be combined.
   * For example, QuarterOfYear and MonthOfQuarter can be combined to form MonthOfYear.
   * Then, MonthOfYear can be combined with DayOfMonth and Year to form a date.
   * Any fields which take part in a merge will be removed from the result as their
   * values can be derived from the merged field.
   * <p>
   * The exact definition of which fields combine with which is chronology dependent.
   * For example, see {@link ISOChronology}.
   * <p>
   * The details of the process are controlled by the merge context.
   * This includes strict/lenient behavior.
   * <p>
   * The merge must result in consistent values for each field, date and time.
   * If two different values are produced an exception is thrown.
   * For example, both Year/MonthOfYear/DayOfMonth and Year/DayOfYear will merge to form a date.
   * If both sets of fields do not produce the same date then an exception will be thrown.
   *
   * @throws CalendricalException if the merge cannot be completed successfully
   */
  def merge: Calendrical = {
    processingMap.clear
    if (inputMap.size > 0) {
      interpret
      mergeLoop
      ISOChronology.merge(this)
      if (processingMap.size > 1) {
        removeDerivable
      }
    }
    return this
  }

  /**
   * Performs the merge based on the rules.
   *
   * @throws CalendricalException if the merge cannot be completed successfully
   */
  private def mergeLoop: Unit = {
    iterator = inputMap.keySet.iterator
    var protect: Int = 0
    while (iterator.hasNext && protect < 100) {
      iterator.next.merge(this)
      protect += 1;
    }
    if (iterator.hasNext) {
      throw new CalendricalException("Merge fields failed, infinite loop blocked, " + "probably caused by an incorrectly implemented field rule")
    }
  }
}