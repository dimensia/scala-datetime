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
package scalax.time.calendar.format

import java.util.Locale

import collection.mutable.{HashMap, ArrayBuffer}
import collection.immutable.TreeMap

import scalax.time.calendar.CalendricalContext
import scalax.time.calendar.CalendricalMerger
import scalax.time.calendar.CalendricalRule
import scalax.time.calendar.DateTimeFieldRule

/**
 * Context object used during date and time parsing.
 * <p>
 * This class represents the current state of the parse.
 * It has the ability to store and retrieve the parsed values and manage optional segments.
 * It also provides key information to the parsing methods.
 * <p>
 * Once parsing is complete, the {@link #toCalendricalMerger()} is typically used
 * to obtain a merger that will merge the separate parsed fields into meaningful values.
 * <p>
 * This class is mutable and thus not thread-safe.
 * Usage of the class is thread-safe within the Time Framework for Java as the
 * framework creates a new instance of the class for each parse.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object DateTimeParseContext {

  private[format] class Parsed {
    override def toString: String =  throw new Exception("Not implemented!")
    /*(new TreeMap[CalendricalRule[Any], Any]() ++ values).toString*/

    protected[format] override def clone: DateTimeParseContext.Parsed = {
      val cloned: DateTimeParseContext.Parsed = new DateTimeParseContext.Parsed
      cloned.values ++= this.values
      cloned
    }

    private[format] val values = new HashMap[CalendricalRule[Any], Any]
  }

}

/**
 * Constructor.
 *
 * @param symbols the symbols to use during parsing, not null
 */
final class DateTimeParseContext(val symbols: DateTimeFormatSymbols) {
  DateTimeFormatter.checkNotNull(symbols, "DateTimeFormatSymbols must not be null")
  calendricals += new DateTimeParseContext.Parsed

  /**
   * Whether to parse using strict rules.
   */
  private var strict: Boolean = true

  /**
   * Checks if parsing is strict.
   * <p>
   * Strict parsing requires exact matching of the text and sign styles.
   *
   * @return true if parsing is strict, false if lenient
   */
  def isStrict: Boolean = strict

  /**
   * Sets whether parsing is strict or lenient.
   *
   * @param strict  changes the parsing to be strict or lenient from now on
   */
  def setStrict(implicit strict: Boolean = true): Unit = this.strict = strict;  //TODO implicit: is this what we want?

  /**
   * Gets the set of parsed rules.
   * <p>
   * The set can be read and have elements removed, but nothing can be added.
   *
   * @return the set of rules previously parsed, never null
   */
  def getParsedRules = currentCalendrical.values.keySet

  /**
   * Whether to parse using case sensitively.
   */
  var caseSensitive: Boolean = true

  /**
   * Checks if parsing is case sensitive.
   *
   * @return true if parsing is case sensitive, false if case insensitive
   */
  def isCaseSensitive: Boolean = caseSensitive

  /**
   * Sets whether the parsing is case sensitive or not.
   *
   * @param caseSensitive  changes the parsing to be case sensitive or not from now on
   */
  def setCaseSensitive(caseSensitive: Boolean) = this.caseSensitive = caseSensitive

  /**
   * Returns a {@code CalendricalMerger} that can be used to interpret
   * the results of the parse.
   * <p>
   * This method is typically used once parsing is complete to obtain the parsed data.
   * Parsing will typically result in separate fields, such as year, month and day.
   * The returned merger can be used to combine the parsed data into meaningful
   * objects such as {@code Date}, potentially applying complex processing
   * to handle invalid parsed data.
   *
   * @return a new independent merger with the parsed rule-value map, never null
   */
  def toCalendricalMerger: CalendricalMerger = new CalendricalMerger(new CalendricalContext(true, true), currentCalendrical.values)

  /**
   * Ends the parsing of an optional segment of the input.
   *
   * @param successful whether the optional segment was successfully parsed
   */
  def endOptional(successful: Boolean): Unit = {
    if (successful) calendricals.remove(calendricals.size - 2)
    else calendricals.remove(calendricals.size - 1)
  }

  /**
   * Gets the formatting symbols.
   *
   * @return the formatting symbols, never null
   */
  def getSymbols: DateTimeFormatSymbols = symbols

  /**
   * Returns a string version of the context for debugging.
   *
   * @return a string representation of the context date, never null
   */
  override def toString: String = currentCalendrical.toString

  /**
   * Sets the parsed value associated with the specified rule.
   * <p>
   * The value stored may be out of range for the rule and of any type -
   * no checks are performed.
   *
   * @param rule the rule to set in the rule-value map, not null
   * @param value the value to set in the rule-value map, not null
   */
  def setParsed(rule: CalendricalRule[Any], value: AnyRef): Unit = {
    DateTimeFormatter.checkNotNull(rule, "CalendricalRule must not be null")
    DateTimeFormatter.checkNotNull(value, "Value must not be null")
    currentCalendrical.values.put(rule, value)
  }

  /**
   * Gets the currently active calendrical.
   *
   * @return the current calendrical, never null
   */
  private def currentCalendrical: DateTimeParseContext.Parsed = calendricals(calendricals.size - 1)

  /**
   * The list of parsed data.
   */
  private val calendricals: ArrayBuffer[DateTimeParseContext.Parsed] = new ArrayBuffer[DateTimeParseContext.Parsed]

  /**
   * Starts the parsing of an optional segment of the input.
   */
  def startOptional: Unit = calendricals += currentCalendrical.clone

  /**
   * Sets the parsed value associated with the specified rule.
   * <p>
   * The value stored may be out of range for the rule - no checks are performed.
   *
   * @param rule the rule to set in the rule-value map, not null
   * @param value the value to set in the rule-value map
   */
  def setParsed(rule: DateTimeFieldRule[Any], value: Int): Unit = {
    DateTimeFormatter.checkNotNull(rule, "DateTimeFieldRule must not be null")
    currentCalendrical.values.put(rule, value)
  }

  /**
   * Gets the locale to use for printing and parsing text.
   *
   * @return the locale, never null
   */
  def getLocale: Locale = symbols.getLocale

  /**
   * Gets the parsed value for the specified rule.
   * <p>
   * The value returned is directly obtained from the stored map of values.
   * It may be of any type and any value.
   * For example, the day-of-month might be set to 50, or the hour to 1000.
   *
   * @param rule the rule to query from the map, not null
   * @return the value mapped to the specified rule, null if rule not in the map
   */
  def getParsed(rule: CalendricalRule[Any]): Any = {
    DateTimeFormatter.checkNotNull(rule, "CalendricalRule must not be null")
    currentCalendrical.values.get(rule)
  }
}