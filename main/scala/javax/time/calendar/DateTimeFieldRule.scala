/*
 * Copyright (c) 2007-2010 Stephen Colebourne & Michael Nascimento Santos
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

import format.DateTimeFormatterBuilder
import java.lang.ref.SoftReference
import java.math.BigDecimal
import java.math.MathContext
import java.math.RoundingMode
import java.util.Arrays
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap

import collection.JavaConversions.JConcurrentMapWrapper
import collection.mutable.{HashSet, HashMap}

/**
 * The rule defining how a measurable field of time operates.
 * <p>
 * Rule implementations define how a field like day-of-month operates.
 * This includes the field name and minimum/maximum values.
 * <p>
 * DateTimeFieldRule is an abstract class and must be implemented with care to
 * ensure other classes in the framework operate correctly.
 * All instantiable subclasses must be final, immutable and thread-safe and must
 * ensure serialization works correctly.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object DateTimeFieldRule {

  /**
   * The mapping between integer values and textual representations.
   * <p>
   * Some fields have a textual representation, such as day-of-week or month-of-year.
   * These textual representations can be captured in this class for printing
   * and parsing.
   * <p>
   * TextStore is immutable and thread-safe.
   *
   * @author Stephen Colebourne
   */

  /**
   * Constructor.
   *
   * @param locale the locale, not null
   * @param valueTextMap the map of values to text to store, not null
   */
  final class TextStore(locale: Locale, var valueTextMap: HashMap[Int, String]) {
    ISOChronology.checkNotNull(locale, "Locale must not be null")
    ISOChronology.checkNotNull(valueTextMap, "Map must not be null")
    if (valueTextMap.contains(null) || valueTextMap.valuesIterator.contains(null) || valueTextMap.valuesIterator.contains("")) {
      throw new IllegalArgumentException("The map must not contain null or empty text")
    }
    val copy: HashMap[Int,String] = new HashMap[Int, String]() ++ valueTextMap
    var reverse = new HashMap[String, Int]
    var insensitive = new HashMap[String, Int]
    val lengthSet = new HashSet[Int]
    for (entry <- copy) {
      val text: String = entry._2
      val value: Int = entry._1
      reverse.put(text, value)
      lengthSet.add(text.length)
      val lower: String = text.toLowerCase(locale)
      insensitive.put(lower, value)
      lengthSet.add(lower.length)
      val upper: String = text.toUpperCase(locale)
      insensitive.put(upper, value)
      lengthSet.add(upper.length)
    }
    if (reverse.size < copy.size) {
      this.textValueMap = Map[String, Int]()
      this.insensitiveTextValueMap = Map[String, Int]()
      this.lengths = null
    }
    else {
      textValueMap = reverse.toMap
      insensitiveTextValueMap = insensitive.toMap
      this.lengths = new Array[Int](lengthSet.size)
      var i: Int = 0
      val it: Iterator[Int] = lengthSet.iterator
      while (it.hasNext) {
        //lengths[i++] = it.next();
        lengths(i) = it.next
        i += 1
      }

      Arrays.sort(lengths)
    }
    this.valueTextMap = copy

    /**
     * Gets the locale that the text relates to.
     *
     * @return the locale for the text, never null
     */
    def getLocale: Locale = locale

    /**
     * Map of case sensitive text to value.
     */
    private var textValueMap: Map[String, Int] = null

    /**
     * Matches the specified text against the text-value map returning the
     * matched length and value.
     * <p>
     * This method is intended for use during parsing, and matches the search text
     * against the text-value map, optionally ignoring case.
     *
     * @param ignoreCase true to ignore case during the matching
     * @param parseText the text to match against
     * @return a long packed result of two int values (for performance in parsing).
     *  The value is <code>(parseLength << 32 + matchedValue)</code>.
     *  Zero is returned if there is no match.
     *  Minus one is returned if the text store cannot parse.
     *  The parse length can be obtained via (result >>> 32).
     *  The value can be obtained via ((int) result).
     */
    def matchText(ignoreCase: Boolean, _parseText: String): Long = {
      var parseText = _parseText
      ISOChronology.checkNotNull(parseText, "Search text must not be null")
      if (lengths == null) {
        return -1
      }
      var lengthsStart: Int = Arrays.binarySearch(lengths, parseText.length)
      lengthsStart = (if (lengthsStart < 0) -lengthsStart - 2 else lengthsStart)
      if (ignoreCase) {
        parseText = parseText.toUpperCase(locale)

        {
          var i: Int = lengthsStart
          while (i >= 0) {
            insensitiveTextValueMap.get(parseText.substring(0, lengths(i))) match {
              case Some(value) => return ((lengths(i).toLong) << 32) + value
              case None =>
            }
            i -= 1;
          }
        }
        parseText = parseText.toLowerCase(locale)

        {
          var i: Int = lengthsStart
          while (i >= 0) {
            insensitiveTextValueMap.get(parseText.substring(0, lengths(i))) match {
              case Some(value) => return ((lengths(i).toLong) << 32) + value
              case None =>
            }
            i -= 1;
          }
        }
      }
      else {
        {
          var i: Int = lengthsStart
          while (i >= 0) {
            textValueMap.get(parseText.substring(0, lengths(i))) match {
              case Some(value) => return ((lengths(i).toLong) << 32) + value
              case None =>
            }
            i -= 1;
          }
        }
      }
      return 0
    }

    /**
     * Gets the text for the specified integer value.
     * <p>
     * The text associated with the value is returned, or null if none found.
     *
     * @param value the value to get text for
     * @return the text for the field value, null if no text found
     */
    def getValueText(value: Int): Option[String] = valueTextMap.get(value)

    /**
     * Map of case insensitive text to value.
     */
    private var insensitiveTextValueMap: Map[String, Int] = null

    /**
     * The lengths of the text items.
     */
    private var lengths: Array[Int] = null
    /**
     * Gets the map of text for each integer value.
     *
     * @return the unmodifiable map of value to text, never null
     */
    def getValueTextMap: HashMap[Int, String] = valueTextMap

    /**
     * Gets the derived map expressing the value for each text.
     * <p>
     * If the value-text map contains duplicate text elements then this map
     * will be empty.
     *
     * @return the unmodifiable map of text to value for the field rule and style, never null
     */
    def getTextValueMap: Map[String, Int] = textValueMap
  }

  /**A Math context for calculating values from fractions. */
  private val ValueContext: MathContext = new MathContext(0, RoundingMode.FLOOR)
  /**A Math context for calculating fractions from values. */
  private val FractionContext: MathContext = new MathContext(9, RoundingMode.FLOOR)
}


/**
 * Constructor.
 *
 * @param reifiedClass the reified class, not null
 * @param chronology the chronology, not null
 * @param name the name of the type, not null
 * @param periodUnit the period unit, not null
 * @param periodRange the period range, not null
 * @param minimumValue the minimum value
 * @param maximumValue the minimum value
 * @param hasText true if this field has a text representation
 */
abstract class DateTimeFieldRule[T] protected(reifiedClass: Class[T],
                                              chronology: Chronology,
                                              name: String,
                                              periodUnit: PeriodUnit,
                                              periodRange: PeriodUnit,
                                              minimumValue: Int,
                                              maximumValue: Int,
                                              hasText: Boolean = false)
  extends CalendricalRule[T](reifiedClass, chronology, name, periodUnit, periodRange) {

  import DateTimeFieldRule._

  /**
   * Converts the typed value of the rule to the {@code int} equivalent.
   * <p>
   * This default implementation handles {@code Integer} and {@code Enum}.
   * When the reified type is another type, this method must be overridden.
   *
   * @param value the value to convert, not null
   * @return the int value of the field
   * @throws ClassCastException if the value cannot be converted
   */
  def convertValueToInt(value: T): Int = {
    //FIXME
    if (value.isInstanceOf[Enum[_]]) (value.asInstanceOf[Enum[_]]).ordinal + getMinimumValue
    else value.asInstanceOf[Int]
  }

  /**
   * Checks if the value is invalid and throws an exception if it is.
   * <p>
   * This method has no knowledge of other calendrical fields, thus only the
   * outer minimum and maximum range for the field is validated.
   * <p>
   * This method performs the same check as {@link #checkValue ( int )}.
   * The implementation uses {@link #isValidValue ( long )}.
   *
   * @param value the value to check
   * @return the value cast to an int
   * @throws IllegalCalendarFieldValueException if the value is invalid
   */
  def checkValue(value: Long): Int = {
    if (isValidValue(value) == false) {
      throw new IllegalCalendarFieldValueException(this, value, getMinimumValue, getMaximumValue)
    }
    return value.toInt
  }

  /**
   * Gets the text for this field.
   * <p>
   * Some fields have a textual representation, such as day-of-week or
   * month-of-year. This method provides a convenient way to convert a value
   * to such a textual representation.
   * More control is available using {@link #getTextStore}.
   * <p>
   * If there is no textual mapping, then the value is returned as per
   * {@link Integer#toString()}. Note that this is different to what occurs
   * in printing /parsing, where a more advanced localized conversion from
   * int to String is used.
   *
   * @param value the value to convert to text, not null
   * @param locale the locale to use, not null
   * @param textStyle the text style, not null
   * @return the text of the field, never null
   */
  def getText(value: Int, locale: Locale, textStyle: DateTimeFormatterBuilder.TextStyle): String = {
    val textStore: DateTimeFieldRule.TextStore = getTextStore(locale, textStyle)
    val text: String = (if (textStore != null) textStore.getValueText(value).getOrElse(null) else null)
    if (text == null) value.toString else text
  }

  /**
   * Converts a fraction from 0 to 1 for this field to a value.
   * <p>
   * The fractional value must be between 0 (inclusive) and 1 (exclusive).
   * It can only be returned if {@link #isFixedValueSet()} returns true and the
   * {@link #getMinimumValue()} returns zero.
   * The value is obtained by calculation from the field range and a rounding
   * mode of {@link RoundingMode#FLOOR FLOOR}.
   * <p>
   * For example, the fractional second-of-minute of 0.25 would be converted to 15,
   * assuming the standard definition of 60 seconds in a minute.
   *
   * @param fraction the fraction to convert, not null
   * @return the value of the field, checked for validity
   * @throws UnsupportedRuleException if the value cannot be converted
   * @throws IllegalCalendarFieldValueException if the value is invalid
   */
  def convertFractionToInt(fraction: BigDecimal): Int = {
    if (isFixedValueSet == false) {
      throw new UnsupportedRuleException("The fractional value of " + getName + " cannot be converted as the range is not fixed", this)
    }
    if (getMinimumValue != 0) {
      throw new UnsupportedRuleException("The fractional value of " + getName + " cannot be converted as the minimum field value is not zero", this)
    }
    var range: Long = getMaximumValue
    range += 1;
    val decimal: BigDecimal = fraction.multiply(new BigDecimal(range), ValueContext)
    try {
      val value: Int = decimal.intValueExact
      checkValue(value)
      return value
    }
    catch {
      case ex: ArithmeticException => {
        throw new IllegalCalendarFieldValueException("The fractional value " + fraction + " of " + getName + " cannot be converted as it is not in the range 0 (inclusive) to 1 (exclusive)", this)
      }
    }
  }

  /**
   * Gets the largest possible minimum value that the field can take.
   * <p>
   * The default implementation returns {@link #getMinimumValue()}.
   * Subclasses must override this as necessary.
   *
   * @return the largest possible minimum value for this field
   */
  def getLargestMinimumValue: Int = getMinimumValue

  /**
   * Gets the minimum value that the field can take using the specified
   * calendrical information to refine the accuracy of the response.
   * <p>
   * The result of this method will still be inaccurate if there is insufficient
   * information in the calendrical.
   * <p>
   * For example, if this field is the ISO day-of-month field, then the number
   * of days in the month varies depending on the month and year. If both the
   * month and year can be derived from the calendrical, then the maximum value
   * returned will be accurate. Otherwise the 'best guess' value from
   * {@link #getMaximumValue()} will be returned.
   * <p>
   * The default implementation returns {@link #getMaximumValue()}.
   * Subclasses must override this as necessary.
   *
   * @param calendrical context calendrical, not null
   * @return the minimum value of the field given the context
   */
  def getMaximumValue(calendrical: Calendrical): Int = getMaximumValue

  /**
   * Gets the minimum value that the field can take using the specified
   * calendrical information to refine the accuracy of the response.
   * <p>
   * The result of this method may still be inaccurate, if there is insufficient
   * information in the calendrical.
   * <p>
   * The default implementation returns {@link #getMinimumValue()}.
   * Subclasses must override this as necessary.
   *
   * @param calendrical context calendrical, not null
   * @return the minimum value of the field given the context
   */
  def getMinimumValue(calendrical: Calendrical): Int = getMinimumValue

  /**
   * Gets the maximum value that the field can take.
   *
   * @return the maximum value for this field
   */
  def getMaximumValue: Int = maximumValue

  /**
   * Checks if the value is invalid and throws an exception if it is.
   * <p>
   * This method has no knowledge of other calendrical fields, thus only the
   * outer minimum and maximum range for the field is validated.
   * <p>
   * This method performs the same check as {@link #checkValue ( long )}.
   * The implementation uses {@link #isValidValue ( int )}.
   *
   * @param value the value to check
   * @return the value
   * @throws IllegalCalendarFieldValueException if the value is invalid
   */
  def checkValue(value: Int): Int = {
    if (isValidValue(value) == false) throw new IllegalCalendarFieldValueException(this, value, getMinimumValue, getMaximumValue)
    else value
  }

  /**
   * Gets the minimum value that the field can take.
   *
   * @return the minimum value for this field
   */
  def getMinimumValue: Int = minimumValue

  /**
   * Gets the text map for this field with the specified locale and style.
   * <p>
   * Some fields have a textual representation, such as day-of-week or
   * month-of-year. The text store provides details of those textual representations.
   * <p>
   * To supply text, subclasses should pass true in the constructor and
   * override {@link #createTextStores}. This method is not normally overridden.
   *
   * @param locale the locale to use, not null
   * @param textStyle the text style, not null
   * @return the text cache, null if no text available
   */
//  def getTextStore(locale: Locale, textStyle: DateTimeFormatterBuilder.TextStyle): DateTimeFieldRule.TextStore = {
//    if (textStores == null) {
//      return null
//    }
//    val ref: SoftReference[HashMap[DateTimeFormatterBuilder.TextStyle, DateTimeFieldRule.TextStore]] = textStores.getOrElse(locale, null)
//    if (ref != null) {
//      val textMapByStyle: HashMap[DateTimeFormatterBuilder.TextStyle, DateTimeFieldRule.TextStore] = ref.get
//      if (textMapByStyle != null) {
//        return textMapByStyle.getOrElse(textStyle, null)
//      }
//    }
//    var textStoreByStyle = throw new Exception("Not implemented!") //FIXME
////      HashMap[DateTimeFormatterBuilder.TextStyle, DateTimeFieldRule.TextStore](classOf[DateTimeFormatterBuilder.TextStyle])
//    createTextStores(textStoreByStyle, locale)
//    textStoreByStyle = throw new Exception("Not implemented!") //FIXME
////      HashMap[DateTimeFormatterBuilder.TextStyle, DateTimeFieldRule.TextStore](textStoreByStyle)
////    textStores.put(locale, new SoftReference[HashMap[DateTimeFormatterBuilder.TextStyle, DateTimeFieldRule.TextStore]](textStoreByStyle))  //FIXME
////    return textStoreByStyle.getOrElse(textStyle, null)
//
//  }
  def getTextStore(locale: Locale, textStyle: DateTimeFormatterBuilder.TextStyle): DateTimeFieldRule.TextStore = throw new Exception("Not implemented!") //FIXME

  /**
   * Checks if the value is valid or invalid for this field.
   * <p>
   * This method has no knowledge of other calendrical fields, thus only the
   * outer minimum and maximum range for the field is validated.
   * <p>
   * This method performs the same check as {@link #isValidValue ( int )}.
   *
   * @param value the value to check
   * @return true if the value is valid, false if invalid
   */
  def isValidValue(value: Long): Boolean = (value >= getMinimumValue && value <= getMaximumValue)

  /**
   * Gets the smallest possible maximum value that the field can take.
   * <p>
   * The default implementation returns {@link #getMaximumValue()}.
   * Subclasses must override this as necessary.
   *
   * @return the smallest possible maximum value for this field
   */
  def getSmallestMaximumValue: Int = getMaximumValue

  /**
   * Gets the {@code int} value of this field from the specified calendrical
   * throwing an exception if the value cannot be returned.
   * <p>
   * This uses {@link #getValue ( Calendrical )} to find the value and then
   * converts it to an {@code int} ensuring it isn't {@code null}.
   *
   * @param calendrical the calendrical to get the field value from, not null
   * @return the value of the field, never null
   * @throws UnsupportedRuleException if the field cannot be extracted
   */
  final def getInt(calendrical: Calendrical): Int = {
    getValue(calendrical) match {
      case Some(value) => convertValueToInt(value)
      case None => throw new UnsupportedRuleException(this)
    }
  }

  /**
   * Converts a value for this field to a fraction between 0 and 1.
   * <p>
   * The fractional value is between 0 (inclusive) and 1 (exclusive).
   * It can only be returned if {@link #isFixedValueSet()} returns true and the
   * {@link #getMinimumValue()} returns zero.
   * The fraction is obtained by calculation from the field range using 9 decimal
   * places and a rounding mode of {@link RoundingMode#FLOOR FLOOR}.
   * <p>
   * For example, the second-of-minute value of 15 would be returned as 0.25,
   * assuming the standard definition of 60 seconds in a minute.
   *
   * @param value the value to convert, valid for this field
   * @return the fractional value of the field
   * @throws UnsupportedRuleException if the value cannot be converted
   * @throws IllegalCalendarFieldValueException if the value is invalid
   */
  def convertIntToFraction(value: Int): BigDecimal = {
    if (isFixedValueSet == false) {
      throw new UnsupportedRuleException("The fractional value of " + getName + " cannot be obtained as the range is not fixed", this)
    }
    if (getMinimumValue != 0) {
      throw new UnsupportedRuleException("The fractional value of " + getName + " cannot be obtained as the minimum field value is not zero", this)
    }
    checkValue(value)
    var range: Long = getMaximumValue
    range += 1
    val decimal: BigDecimal = new BigDecimal(value)
    return decimal.divide(new BigDecimal(range), FractionContext)
  }

  /**
   * Creates the text store for each style for the specified locale.
   * <p>
   * It is intended that a new copy of the text store should be created in
   * response to calling this method as the result is cached by {@link #getTextStore}.
   *
   * @param textStores the map to populate with TextStore instances, not null
   * @param locale the locale to use, not null
   */
  protected def createTextStores(textStores: HashMap[DateTimeFormatterBuilder.TextStyle, DateTimeFieldRule.TextStore], locale: Locale): Unit = {}

  /**The cached text for this rule. */
  @transient
  private lazy val textStores = (if (hasText) JConcurrentMapWrapper(new ConcurrentHashMap[Locale, SoftReference[HashMap[Any, Any]]]) else null)

  /**
   * Converts the {@code int} to a typed value of the rule.
   * <p>
   * The {@code int} will be checked to ensure that it is within the
   * valid range of values for the field.
   * <p>
   * This default implementation handles {@code Integer} and {@code Enum}.
   * When the reified type is another type, this method must be overridden.
   *
   * @param value the value to convert, not null
   * @return the int value of the field
   * @throws IllegalCalendarFieldValueException if the value is invalid
   * @throws ClassCastException if the value cannot be converted
   */
  def convertIntToValue(value: Int): T = {
    //FIXME
    checkValue(value)
    return reify(value).getOrElse(throw new IllegalCalendarFieldValueException(null, null))
  }

  /**
   * Is the set of values, from the minimum value to the maximum, a fixed
   * set, or does it vary according to other fields.
   *
   * @return true if the set of values is fixed
   */
  def isFixedValueSet: Boolean = getMaximumValue == getSmallestMaximumValue && getMinimumValue == getLargestMinimumValue

  /**
   * Converts the typed value of the rule to the {@code Integer} equivalent.
   * <p>
   * This method avoids boxing and unboxing when the value is an Integer.
   *
   * @param value the value to convert, not null
   * @return the int value of the field
   */
  private def convertValueToInteger(value: T): Int = {
    if (getReifiedType == classOf[Int]) value.asInstanceOf[Int]
    else convertValueToInt(value)
  }

  /**
   * Checks if the value is valid or invalid for this field.
   * <p>
   * This method has no knowledge of other calendrical fields, thus only the
   * outer minimum and maximum range for the field is validated.
   * <p>
   * This method performs the same check as {@link #isValidValue ( long )}.
   *
   * @param value the value to check
   * @return true if the value is valid, false if invalid
   */
  def isValidValue(value: Int): Boolean = value >= getMinimumValue && value <= getMaximumValue

  /**
   * Gets the {@code Integer} value of this field from the specified calendrical
   * returning {@code null} if the value cannot be returned.
   * <p>
   * This uses {@link #getValue ( Calendrical )} to find the value and then
   * converts it to an {@code Integer}.
   *
   * @param calendrical the calendrical to get the field value from, not null
   * @return the value of the field, null if unable to extract the field
   */
  final def getInteger(calendrical: Calendrical): Option[Int] = getValue(calendrical).map(convertValueToInteger(_))
}