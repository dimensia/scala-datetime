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
package javax.time.calendar.format

import java.text.DateFormatSymbols
import java.util.Locale

/**
 * Symbols used for date and time formatting.
 *
 * @author Stephen Colebourne
 */
object DateTimeFormatSymbols {
  /**
   * Gets the {@code DateFormatSymbols} instance for the default
   * locale. This method provides access to {@code DateFormatSymbols }
   * instances for locales supported by the Java runtime itself as well as for
   * those supported by installed
   * {@link java.text.spi.DateFormatSymbolsProvider DateFormatSymbolsProvider }
   * implementations.
   *
   * @return a {@code DateTimeFormatSymbols} instance.
   */
  def getInstance: DateTimeFormatSymbols = getInstance(Locale.getDefault)

  /**
   * Returns an array of all locales for which the {@code getInstance }
   * methods of this class can return localized instances. The returned array
   * represents the union of locales supported by the Java runtime and by
   * installed
   * {@link java.text.spi.DateTimeFormatSymbolsProvider DateTimeFormatSymbolsProvider }
   * implementations. It must contain at least a {@code Locale }
   * instance equal to {@link java.util.Locale#US Locale.US}.
   *
   * @return an array of locales for which localized
   * {@code DateTimeFormatSymbols} instances are available
   */
  def getAvailableLocales: Array[Locale] = Locale.getAvailableLocales

  /**
   * Gets the {@code DateFormatSymbols} instance for the specified
   * locale. This method provides access to {@code DateFormatSymbols }
   * instances for locales supported by the Java runtime itself as well as for
   * those supported by installed
   * {@link java.text.spi.DateFormatSymbolsProvider DateFormatSymbolsProvider }
   * implementations.
   *
   * @param locale the given locale, not null
   * @return a {@code DateTimeFormatSymbols} instance
   * @throws NullPointerException if {@code locale} is null
   */
  def getInstance(locale: Locale): DateTimeFormatSymbols = {
    DateTimeFormatter.checkNotNull(locale, "Locale must not be null")
    val symbols: DateFormatSymbols = new DateFormatSymbols(locale)
    return new DateTimeFormatSymbols(locale, symbols)
  }
}

/**
 * Constructor.
 *
 * @param locale the locale for printing and parsing text, not null
 */
final class DateTimeFormatSymbols private(locale: Locale) {
  DateTimeFormatter.checkNotNull(locale, "Locale must not be null")

  /**
   * Constructor.
   *
   * @param locale the locale for printing and parsing text, not null
   * @param oldSymbols the old symbols, not null
   */
  private def this(locale: Locale, oldSymbols: DateFormatSymbols) {
    this (locale)
    DateTimeFormatter.checkNotNull(oldSymbols, "Symbols to convert must not be null")
  }

  /**
   * Gets the character that represents the decimal point.
   * <p>
   * This character can vary by locale.
   *
   * @return the character for the decimal point
   */
  def getDecimalPointChar: Char = decimalPointChar

  /**
   * The negative sign character.
   */
  private val negativeSignChar: Char = '-'
  /**
   * The zero character.
   */
  private val zeroChar: Char = '0'
  /**
   * Checks whether the character is a digit, based on the currently set zero character.
   *
   * @param ch the character to check
   * @return the value, 0 to 9, of the character, or -1 if not a digit
   */
  def convertToDigit(ch: Char): Int = {
    val value: Int = ch - zeroChar
    return if ((value >= 0 && value <= 9)) value else -1
  }

  /**
   * Converts the input numeric text to the internationalized form using the
   * zero character.
   *
   * @param numericText the text, consisting of digits 0 to 9, to convert, not null
   * @return the internationalized text, never null
   */
  private[format] def convertNumberToI18N(numericText: String): String = {
    if (getZeroChar == '0') {
      return numericText
    }
    val diff: Int = getZeroChar - '0'
    val array: Array[Char] = numericText.toCharArray
      var i: Int = 0
      while (i < array.length) {
          array(i) = (array(i) + diff).toChar
          i += 1;
      }
    return new String(array)
  }

  /**
   * Gets the character that represents zero.
   * <p>
   * This character can vary by locale.
   *
   * @return the character for zero
   */
  def getZeroChar: Char = zeroChar

  /**
   * Gets the locale to use for printing and parsing text.
   * <p>
   * The locale information for printing and parsing numbers is defined in
   * the zero, negative and positive characters.
   *
   * @return the character for zero
   */
  def getLocale: Locale = locale

  /**
   * Gets the character that represents the specified digit.
   * <p>
   * This character can vary by locale.
   * The characters for one to nine are based on the stored zero character.
   *
   * @param digit the single digit to convert to a character, from 0 to 9
   * @return the character for the digit
   */
  def getDigitChar(digit: Int): Char = (digit + zeroChar).toChar

  /**
   * The decimal point character.
   */
  private val decimalPointChar: Char = '.'

  /**
   * The positive sign character.
   */
  private val positiveSignChar: Char = '+'

  /**
   * Gets the character that represents the negative sign.
   * <p>
   * This character can vary by locale.
   *
   * @return the character for the negative sign
   */
  def getNegativeSignChar: Char = negativeSignChar

  /**
   * Gets the character that represents the positive sign.
   * <p>
   * This character can vary by locale.
   *
   * @return the character for the positive sign
   */
  def getPositiveSignChar: Char = positiveSignChar
}