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
import scalax.time.calendar.ISOChronology
import scalax.time.calendar.format.DateTimeFormatterBuilder.FormatStyle
import scalax.time.calendar.format.DateTimeFormatterBuilder.SignStyle
import scalax.time.calendar.format.DateTimeFormatterBuilder.TextStyle

/**
 * Provides common implementations of {@code DateTimeFormatter}.
 * <p>
 * DateTimeFormatters is a utility class.
 * All formatters returned are immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object DateTimeFormatters {
  /**
   * Returns the ISO date formatter that prints/parses a date without an offset.
   * <p>
   * This is the ISO-8601 extended format:<br />
   * {@code yyyyMMdd }
   * <p>
   * The year is limited to printing and parsing 4 digits, as the lack of
   * separators makes it impossible to parse more than 4 digits.
   *
   * @return the ISO date formatter, never null
   */
  def basicIsoDate: DateTimeFormatter = BasicISODate

  /**Singleton date formatter. */
  private val RFC1123DateTime: DateTimeFormatter = {
    (new DateTimeFormatterBuilder) //      .appendText(ISOChronology.dayOfWeekRule, TextStyle.Short)
      .appendLiteral(", ")
      .appendValue(ISOChronology.dayOfMonthRule, 2)
      .appendLiteral(' ') //      .appendText(ISOChronology.monthOfYearRule, TextStyle.Short)
      .appendLiteral(' ')
      .appendValue(ISOChronology.yearRule, 4, 4, SignStyle.NotNegative)
      .appendLiteral(' ')
      .appendValue(ISOChronology.hourOfDayRule, 2)
      .appendLiteral(':')
      .appendValue(ISOChronology.minuteOfHourRule, 2)
      .appendLiteral(':')
      .appendValue(ISOChronology.secondOfMinuteRule, 2)
      .appendLiteral(' ')
      .appendOffset("Z", false, false)
      .toFormatter
      .withLocale(Locale.ENGLISH)
  }

  /**Singleton date formatter. */
  private val ISOLocalTime: DateTimeFormatter = {
    (new DateTimeFormatterBuilder)
      .appendValue(ISOChronology.hourOfDayRule, 2)
      .appendLiteral(':')
      .appendValue(ISOChronology.minuteOfHourRule, 2)
      .optionalStart
      .appendLiteral(':')
      .appendValue(ISOChronology.secondOfMinuteRule, 2)
      .optionalStart // .appendFraction(ISOChronology.nanoOfSecondRule, 0, 9)
      .toFormatter
  }

  /**
   * Returns the ISO date formatter that prints/parses a date, with the
   * offset and zone if available, such as '2007-12-03T10:15:30',
   * '2007-12-03T10:15:30+01:00' or '2007-12-03T10:15:30+01:00[Europe/Paris]'.
   * <p>
   * This is the ISO-8601 extended format:<br />
   * {@code yyyy -MM-dd'T'HH:mm[:ss[.S]][ZZ['['                { ZoneId} ']']] }
   * <p>
   * The year will print 4 digits, unless this is insufficient, in which
   * case the full year will be printed together with a positive/negative sign.
   * <p>
   * The seconds will be printed if present in the Calendrical, thus a ZonedDateTime
   * will always print the seconds.
   * The nanoseconds will be printed if non-zero.
   * If non-zero, the minimum number of fractional second digits will printed.
   * <p>
   * The offset will print and parse an offset with seconds even though that
   * is not part of the ISO-8601 standard.
   *
   * @return the ISO date formatter, never null
   */
  def isoDateTime: DateTimeFormatter = ISODateTime

  /**
   * Returns the RFC-1123 date-time formatter.
   * <p>
   * This is the RFC-1123 format: EEE, dd MMM yyyy HH:mm:ss Z.
   * This is the updated replacement for RFC-822 which had a two digit year.
   * <p>
   * The year will print 4 digits, and only the range 0000 to 9999 is supported.
   *
   * @return the ISO date formatter, never null
   */
  def rfc1123: DateTimeFormatter = RFC1123DateTime

  /**Singleton date formatter. */
  private val ISOZonedDateTime: DateTimeFormatter = {
    (new DateTimeFormatterBuilder)
      .append(ISOLocalDateTime)
      .appendOffsetId
      .appendLiteral('[')
      .appendZoneId
      .appendLiteral(']')
      .toFormatter
  }

  /**
   * Returns a locale specific date format, which is typically of full length.
   * <p>
   * This returns a formatter that will print/parse a full length date format.
   * The exact format pattern used varies by locale, which is determined from the
   * locale on the formatter. That locale is initialized by method.
   * If a new formatter is obtained using {@link DateTimeFormatter#withLocale ( Locale ) }
   * then it will typically change the pattern in use to the appropriate full
   * length date format for that new locale.
   *
   * @param locale the locale to use, not null
   * @return the full date formatter, never null
   */
  def fullDate(locale: Locale): DateTimeFormatter = date(FormatStyle.Full, locale)

  /**
   * Returns the ISO date formatter that prints/parses an offset date with an offset,
   * such as '2007-12-03T10:15:30+01:00'.
   * <p>
   * This is the ISO-8601 extended format:<br />
   * {@code yyyy -MM-dd'T'HH:mm[:ss[.S]]ZZ }
   * <p>
   * The year will print 4 digits, unless this is insufficient, in which
   * case the full year will be printed together with a positive/negative sign.
   * <p>
   * The seconds will be printed if present in the Calendrical, thus an OffsetDateTime
   * will always print the seconds.
   * The nanoseconds will be printed if non-zero.
   * If non-zero, the minimum number of fractional second digits will printed.
   * <p>
   * The offset will print and parse an offset with seconds even though that
   * is not part of the ISO-8601 standard.
   *
   * @return the ISO date formatter, never null
   */
  def isoOffsetDateTime: DateTimeFormatter = ISOOffsetDateTime

  /**
   * Returns a locale specific date format, which is typically of long length.
   * <p>
   * This returns a formatter that will print/parse a long length date format.
   * The exact format pattern used varies by locale, which is determined from the
   * locale on the formatter. That locale is initialized by method.
   * If a new formatter is obtained using {@link DateTimeFormatter#withLocale ( Locale ) }
   * then it will typically change the pattern in use to the appropriate long
   * length date format for that new locale.
   *
   * @param locale the locale to use, not null
   * @return the long date formatter, never null
   */
  def longDate(locale: Locale): DateTimeFormatter = date(FormatStyle.Long, locale)

  /**
   * Creates a formatter using the specified pattern.
   * <p>
   * This method will create a formatter based on a simple pattern of letters and symbols.
   * For example, {@code d MMM yyyy} will format 2008-12-03 as '3 Dec 2008'.
   * <p>
   * The returned formatter will use the default locale, but this can be changed
   * using {@link DateTimeFormatter#withLocale ( Locale )}.
   * <p>
   * All letters 'A' to 'Z' and 'a' to 'z' are reserved as pattern letters.
   * The following pattern letters are defined:
   * <pre>
   *  Symbol  Meaning                     Presentation      Examples
   *  ------  -------                     ------------      -------
   *   y       year                        year              2004; 04
   *   D       day-of-year                 number            189
   *   M       month-of-year               month             July; Jul; 07
   *   d       day-of-month                number            10
   *
   *   Q       quarter-of-year             number            3
   *   q       month-of-quarter            number            2
   *
   *   x       week-based-year             year              1996
   *   w       week-of-week-based-year     number            27
   *   e       day-of-week                 number            2
   *   E       day-of-week                 text              Tuesday; Tue
   *   F       week-of-month               number            3
   *
   *   a       am-pm-of-day                text              PM
   *   h       clock-hour-of-am-pm (1-12)  number            12
   *   K       hour-of-am-pm (0-11)        number/fraction   0
   *
   *   H       hour-of-day (0-23)          number/fraction   0
   *   m       minute-of-hour              number/fraction   30
   *   s       second-of-minute            number/fraction   55
   *   S       milli-of-second             number/fraction   978
   *   n       nano-of-second              number/fraction   987654321
   *
   *   I       time-zone ID                zoneID            America/Los_Angeles
   *   z       time-zone name              text              Pacific Standard Time; PST
   *   Z       zone-offset                 offset            -0800; -08:00;
   *
   *   f       make next a fraction        fraction modifier .123
   *   p       pad next                    pad modifier      1
   *
   *   '       escape for text             delimiter
   *   ''      single quote                literal           '
   *   [       optional section start
   *   ]       optional section end
   * </pre>
   * <p>
   * The count of pattern letters determine the format.
   * <p>
   * <b>Text</b>: If the number of pattern letters is 4 or more, the full textual form is used
   * as per {@link TextStyle#Full}. Otherwise a short form is used, as per {@link TextStyle#Short}.
   * <p>
   * <b>Number</b>: If the count of letters is one, then the value is printed using the minimum number
   * of digits and without padding as per {@link #appendValue ( DateTimeFieldRule )}. Otherwise, the
   * count of digits is used as the width of the output field as per {@link #appendValue ( DateTimeFieldRule, int )}.
   * <p>
   * <b>Fraction modifier</b>: Modifies the pattern that immediately follows to be a fraction.
   * All fractional values must use the 'f' prefix to ensure correct parsing.
   * The fraction also outputs the decimal point.
   * If the count of 'f' is one, then the fractional value has the exact number of digits defined by
   * the count of the value being output.
   * If the count of 'f' is two or more, then the fractional value has the a minimum number of digits
   * defined by the count of the value being output and a maximum output of nine digits.
   * <p>
   * For example, 'ssffnnn' outputs the second followed by 3-9 digits of the nanosecond, while
   * 'mmfss' outputs the minute followed by exactly 2 digits representing the second.
   * <p>
   * <b>Year</b>: The count of letters determines the minimum field width below which padding is used.
   * If the count of letters is two, then a {@link DateTimeFormatterBuilder#appendValueReduced reduced} two digit form is used.
   * For printing, this outputs the rightmost two digits. For parsing, this will parse using the
   * base value of 2000, resulting in a year within the range 2000 to 2099 inclusive.
   * If the count of letters is less than four (but not two), then the sign is only output for negative
   * years as per {@link SignStyle#Normal}.
   * Otherwise, the sign is output if the pad width is exceeded, as per {@link SignStyle#ExceedsPad }
   * <p>
   * <b>Month</b>: If the count of letters is 3 or greater, use the Text rules above.
   * Otherwise use the Number rules above.
   * <p>
   * <b>ZoneID</b>: 'Z' outputs offset without a colon, 'ZZ' outputs the offset with a colon, 'ZZZ' or more outputs the zone id.
   * <p>
   * <b>Offset</b>: 'Z' outputs offset without a colon, without seconds and '+0000' as the text for UTC.
   * 'ZZ' outputs the offset with a colon, without seconds and '+00:00' as the text for UTC.
   * 'ZZZ' outputs offset without a colon, with seconds and 'Z' as the text for UTC (ISO-8601 style).
   * 'ZZZZ' outputs the offset with a colon, with seconds and 'Z' as the text for UTC (ISO-8601 style).
   * <p>
   * <b>Zone names</b>: Time zone names ('z') cannot be parsed.
   * <p>
   * <b>Optional section</b>: The optional section markers work exactly like calling {@link #optionalStart()}
   * and {@link #optionalEnd()}.
   * <p>
   * <b>Pad modifier</b>: Modifies the pattern that immediately follows to be padded with spaces.
   * The pad width is determined by the number of pattern letters.
   * This is the same as calling {@link #padNext ( int )}.
   * <p>
   * For example, 'ppH' outputs the hour-of-day padded on the left with spaces to a width of 2.
   * <p>
   * Any unrecognized letter will be output directly.
   * However, since these are reserved, that may change in future versions.
   * Any non-letter character, other than '[', ']' and the single quote will be output directly.
   * Despite this, it is recommended to use single quotes around all characters that you want to
   * output directly to ensure that future changes do not break your application.
   * <p>
   * The pattern string is similar, but not identical, to {@link SimpleDateFormat}.
   * SimpleDateFormat pattern letters 'G', 'W' and 'k' are not available.
   * Pattern letters 'x', 'Q', 'q', 'e', 'n', 'I', 'f' and 'p' are added.
   * Letters 'y', 'z' and 'Z' have some differences.
   *
   * @param pattern the pattern to use, not null
   * @return the formatter based on the pattern, never null
   * @throws IllegalArgumentException if the pattern is invalid
   * @see DateTimeFormatterBuilder#appendPattern ( String )
   */
  def pattern(pattern: String): DateTimeFormatter = (new DateTimeFormatterBuilder).appendPattern(pattern).toFormatter

  /**
   * Returns the ISO time formatter that prints/parses a time, with the
   * offset and zone if available, such as '10:15:30', '10:15:30+01:00'
   * or '10:15:30+01:00[Europe/Paris]'.
   * <p>
   * This is the ISO-8601 extended format:<br />
   * {@code HH :mm[:ss[.S]][ZZ['['                { ZoneId} ']']] }
   * <p>
   * The seconds will be printed if present in the Calendrical, thus a Time
   * will always print the seconds.
   * The nanoseconds will be printed if non-zero.
   * If non-zero, the minimum number of fractional second digits will printed.
   * <p>
   * The offset will print and parse an offset with seconds even though that
   * is not part of the ISO-8601 standard.
   *
   * @return the ISO date formatter, never null
   */
  def isoTime: DateTimeFormatter = ISOTime

  /**
   * Returns a locale specific date-time format of medium length.
   * <p>
   * This returns a formatter that will print/parse a medium length date-time format.
   * The exact format pattern used varies by locale, which is determined from the
   * locale on the formatter. That locale is initialized by method.
   * If a new formatter is obtained using {@link DateTimeFormatter#withLocale ( Locale ) }
   * then it will typically change the pattern in use to the appropriate medium
   * length date-time format for that new locale.
   *
   * @param locale the locale to use, not null
   * @return the medium date-time formatter, never null
   */
  def mediumDateTime(locale: Locale): DateTimeFormatter = dateTime(FormatStyle.Medium, locale)

  /**
   * Returns a locale specific date, time or date-time format.
   * <p>
   * This returns a formatter that will print/parse a date, time or date-time.
   * The exact format pattern used varies by locale, which is determined from the
   * locale on the formatter. That locale is initialized by method.
   * If a new formatter is obtained using {@link DateTimeFormatter#withLocale ( Locale ) }
   * then it will typically change the pattern in use to the appropriate
   * format for that new locale.
   *
   * @param dateStyle the date formatter style to obtain, not null
   * @param timeStyle the time formatter style to obtain, not null
   * @param locale the locale to use, not null
   * @return the date, time or date-time formatter, never null
   */
  def dateTime(dateStyle: FormatStyle, timeStyle: FormatStyle, locale: Locale): DateTimeFormatter = {
    DateTimeFormatter.checkNotNull(dateStyle, "Date style must not be null")
    DateTimeFormatter.checkNotNull(timeStyle, "Time style must not be null")
    (new DateTimeFormatterBuilder).appendLocalized(dateStyle, timeStyle).toFormatter(locale)
  }

  /**Singleton date formatter. */
  private val ISOOffsetTime: DateTimeFormatter = {
    (new DateTimeFormatterBuilder)
      .parseCaseInsensitive
      .append(ISOLocalTime)
      .appendOffsetId
      .toFormatter
  }

  /**Singleton date formatter. */
  private val ISODate: DateTimeFormatter = {
    (new DateTimeFormatterBuilder)
      .parseCaseInsensitive
      .append(ISOLocalDate)
      .optionalStart
      .appendOffsetId
      .optionalStart
      .appendLiteral('[')
      .appendZoneId
      .appendLiteral(']')
      .toFormatter
  }

  /**
   * Returns a locale specific date format of short length.
   * <p>
   * This returns a formatter that will print/parse a short length date format.
   * The exact format pattern used varies by locale, which is determined from the
   * locale on the formatter. That locale is initialized by method.
   * If a new formatter is obtained using {@link DateTimeFormatter#withLocale ( Locale ) }
   * then it will typically change the pattern in use to the appropriate short
   * length date format for that new locale.
   *
   * @param locale the locale to use, not null
   * @return the short date formatter, never null
   */
  def shortDate(locale: Locale): DateTimeFormatter = date(FormatStyle.Short, locale)

  /**
   * Returns the ISO time formatter that prints/parses a local time, with an offset
   * such as '10:15:30+01:00'.
   * <p>
   * This is the ISO-8601 extended format:<br />
   * {@code HH :mm[:ss[.S]]ZZ }
   * <p>
   * The seconds will be printed if present in the Calendrical, thus an OffsetTime
   * will always print the seconds.
   * The nanoseconds will be printed if non-zero.
   * If non-zero, the minimum number of fractional second digits will printed.
   * <p>
   * The offset will print and parse an offset with seconds even though that
   * is not part of the ISO-8601 standard.
   *
   * @return the ISO time formatter, never null
   */
  def isoOffsetTime: DateTimeFormatter = ISOOffsetTime

  /**Singleton date formatter. */
  private val ISOOffsetDate: DateTimeFormatter = {
    (new DateTimeFormatterBuilder)
      .parseCaseInsensitive
      .append(ISOLocalDate)
      .appendOffsetId
      .toFormatter
  }

  /**Singleton date formatter. */
  private val ISOLocalDate: DateTimeFormatter = {
    (new DateTimeFormatterBuilder)
      .appendValue(ISOChronology.yearRule, 4, 10, SignStyle.ExceedsPad)
      .appendLiteral('-')
      .appendValue(ISOChronology.monthOfYearRule, 2)
      .appendLiteral('-')
      .appendValue(ISOChronology.dayOfMonthRule, 2)
      .toFormatter
  }

  /**
   * Returns the ISO date formatter that prints/parses a local date without an offset,
   * such as '2007-12-03'.
   * <p>
   * This is the ISO-8601 extended format:<br />
   * {@code yyyy -MM-dd }
   * <p>
   * The year will print 4 digits, unless this is insufficient, in which
   * case the full year will be printed together with a positive/negative sign.
   *
   * @return the ISO date formatter, never null
   */
  def isoLocalDate: DateTimeFormatter = ISOLocalDate

  /**
   * Returns a locale specific time format.
   * <p>
   * This returns a formatter that will print/parse a time.
   * The exact format pattern used varies by locale, which is determined from the
   * locale on the formatter. That locale is initialized by method.
   * If a new formatter is obtained using {@link DateTimeFormatter#withLocale ( Locale ) }
   * then it will typically change the pattern in use to the appropriate
   * time format for that new locale.
   *
   * @param timeStyle the formatter style to obtain, not null
   * @param locale the locale to use, not null
   * @return the time formatter, never null
   */
  def time(timeStyle: FormatStyle, locale: Locale): DateTimeFormatter = {
    DateTimeFormatter.checkNotNull(timeStyle, "Time style must not be null")
    (new DateTimeFormatterBuilder).appendLocalized(null, timeStyle).toFormatter(locale)
  }

  /**
   * Returns a locale specific date-time format of short length.
   * <p>
   * This returns a formatter that will print/parse a short length date-time format.
   * The exact format pattern used varies by locale, which is determined from the
   * locale on the formatter. That locale is initialized by method.
   * If a new formatter is obtained using {@link DateTimeFormatter#withLocale ( Locale ) }
   * then it will typically change the pattern in use to the appropriate short
   * length date-time format for that new locale.
   *
   * @param locale the locale to use, not null
   * @return the short date-time formatter, never null
   */
  def shortDateTime(locale: Locale): DateTimeFormatter = dateTime(FormatStyle.Short, locale)

  /**
   * Returns a locale specific date-time format, which is typically of short length.
   * <p>
   * This returns a formatter that will print/parse a date-time.
   * The exact format pattern used varies by locale, which is determined from the
   * locale on the formatter. That locale is initialized by method.
   * If a new formatter is obtained using {@link DateTimeFormatter#withLocale ( Locale ) }
   * then it will typically change the pattern in use to the appropriate
   * date-time format for that new locale.
   *
   * @param dateTimeStyle the formatter style to obtain, not null
   * @param locale the locale to use, not null
   * @return the date-time formatter, never null
   */
  def dateTime(dateTimeStyle: FormatStyle, locale: Locale): DateTimeFormatter = {
    DateTimeFormatter.checkNotNull(dateTimeStyle, "Date-time style must not be null")
    (new DateTimeFormatterBuilder).appendLocalized(dateTimeStyle, dateTimeStyle).toFormatter(locale)
  }

  /**
   * Returns a locale specific time format of short length.
   * <p>
   * This returns a formatter that will print/parse a short length time format.
   * The exact format pattern used varies by locale, which is determined from the
   * locale on the formatter. That locale is initialized by method.
   * If a new formatter is obtained using {@link DateTimeFormatter#withLocale ( Locale ) }
   * then it will typically change the pattern in use to the appropriate short
   * length time format for that new locale.
   *
   * @param locale the locale to use, not null
   * @return the short time formatter, never null
   */
  def shortTime(locale: Locale): DateTimeFormatter = time(FormatStyle.Short, locale)

  /**Singleton date formatter. */
  private val ISODateTime: DateTimeFormatter = {
    (new DateTimeFormatterBuilder)
      .append(ISOLocalDateTime)
      .optionalStart
      .appendOffsetId
      .optionalStart
      .appendLiteral('[')
      .appendZoneId
      .appendLiteral(']')
      .toFormatter
  }

  /**
   * Returns the ISO date formatter that prints/parses a date, with the
   * offset and zone if available, such as '2007-12-03', '2007-12-03+01:00'
   * or '2007-12-03+01:00[Europe/Paris]'.
   * <p>
   * This is the ISO-8601 extended format:<br />
   * {@code yyyy -MM-dd[ZZ['['                { ZoneId} ']']] }
   * <p>
   * The year will print 4 digits, unless this is insufficient, in which
   * case the full year will be printed together with a positive/negative sign.
   * <p>
   * The offset will print and parse an offset with seconds even though that
   * is not part of the ISO-8601 standard.
   *
   * @return the ISO date formatter, never null
   */
  def isoDate: DateTimeFormatter = ISODate

  /**
   * Returns a locale specific date-time format, which is typically of long length.
   * <p>
   * This returns a formatter that will print/parse a long length date-time format.
   * The exact format pattern used varies by locale, which is determined from the
   * locale on the formatter. That locale is initialized by method.
   * If a new formatter is obtained using {@link DateTimeFormatter#withLocale ( Locale ) }
   * then it will typically change the pattern in use to the appropriate long
   * length date-time format for that new locale.
   *
   * @param locale the locale to use, not null
   * @return the long date-time formatter, never null
   */
  def longDateTime(locale: Locale): DateTimeFormatter = dateTime(FormatStyle.Long, locale)

  /**Singleton date formatter. */
  private val ISOLocalDateTime: DateTimeFormatter = {
    (new DateTimeFormatterBuilder)
      .parseCaseInsensitive
      .append(ISOLocalDate)
      .appendLiteral('T')
      .append(ISOLocalTime)
      .toFormatter
  }
  /**Singleton date formatter. */
  private val ISOOffsetDateTime: DateTimeFormatter = {
    (new DateTimeFormatterBuilder)
      .append(ISOLocalDateTime)
      .appendOffsetId
      .toFormatter
  }
  /**
   * Returns the ISO time formatter that prints/parses a local time, without an offset
   * such as '10:15:30'.
   * <p>
   * This is the ISO-8601 extended format:<br />
   * {@code HH :mm[:ss[.S]] }
   * <p>
   * The seconds will be printed if present in the Calendrical, thus a Time
   * will always print the seconds.
   * The nanoseconds will be printed if non-zero.
   * If non-zero, the minimum number of fractional second digits will printed.
   *
   * @return the ISO time formatter, never null
   */
  def isoLocalTime: DateTimeFormatter = ISOLocalTime

  /**
   * Returns a locale specific time format, which is typically of long length.
   * <p>
   * This returns a formatter that will print/parse a long length time format.
   * The exact format pattern used varies by locale, which is determined from the
   * locale on the formatter. That locale is initialized by method.
   * If a new formatter is obtained using {@link DateTimeFormatter#withLocale ( Locale ) }
   * then it will typically change the pattern in use to the appropriate long
   * length time format for that new locale.
   *
   * @param locale the locale to use, not null
   * @return the long time formatter, never null
   */
  def longTime(locale: Locale): DateTimeFormatter = time(FormatStyle.Long, locale)

  /**
   * Returns the ISO date formatter that prints/parses an offset date with an offset,
   * such as '2007-12-03+01:00'.
   * <p>
   * This is the ISO-8601 extended format:<br />
   * {@code yyyy -MM-ddZZ }
   * <p>
   * The year will print 4 digits, unless this is insufficient, in which
   * case the full year will be printed together with a positive/negative sign.
   * <p>
   * The offset will print and parse an offset with seconds even though that
   * is not part of the ISO-8601 standard.
   *
   * @return the ISO date formatter, never null
   */
  def isoOffsetDate: DateTimeFormatter = ISOOffsetDate

  /**
   * Returns a locale specific date format of medium length.
   * <p>
   * This returns a formatter that will print/parse a medium length date format.
   * The exact format pattern used varies by locale, which is determined from the
   * locale on the formatter. That locale is initialized by method.
   * If a new formatter is obtained using {@link DateTimeFormatter#withLocale ( Locale ) }
   * then it will typically change the pattern in use to the appropriate medium
   * length date format for that new locale.
   *
   * @param locale the locale to use, not null
   * @return the medium date formatter, never null
   */
  def mediumDate(locale: Locale): DateTimeFormatter = date(FormatStyle.Medium, locale)

  /**
   * Returns a locale specific time format, which is typically of full length.
   * <p>
   * This returns a formatter that will print/parse a full length time format.
   * The exact format pattern used varies by locale, which is determined from the
   * locale on the formatter. That locale is initialized by method.
   * If a new formatter is obtained using {@link DateTimeFormatter#withLocale ( Locale ) }
   * then it will typically change the pattern in use to the appropriate full
   * length time format for that new locale.
   *
   * @param locale the locale to use, not null
   * @return the full time formatter, never null
   */
  def fullTime(locale: Locale): DateTimeFormatter = time(FormatStyle.Full, locale)

  /**
   * Returns the ISO date formatter that prints/parses a date without an offset.
   * <p>
   * This is the ISO-8601 extended format:<br />
   * {@code yyyy -DDD }
   * <p>
   * The year will print 4 digits, unless this is insufficient, in which
   * case the full year will be printed together with a positive/negative sign.
   *
   * @return the ISO ordinal date formatter, never null
   */
  def isoOrdinalDate: DateTimeFormatter = ISOOrdinalDate

  /**
   * Returns a locale specific date format.
   * <p>
   * This returns a formatter that will print/parse a date.
   * The exact format pattern used varies by locale, which is determined from the
   * locale on the formatter. That locale is initialized by method.
   * If a new formatter is obtained using {@link DateTimeFormatter#withLocale ( Locale ) }
   * then it will typically change the pattern in use to the appropriate
   * date format for that new locale.
   *
   * @param dateStyle the formatter style to obtain, not null
   * @param locale the locale to use, not null
   * @return the date formatter, never null
   */
  def date(dateStyle: FormatStyle, locale: Locale): DateTimeFormatter = {
    DateTimeFormatter.checkNotNull(dateStyle, "Date style must not be null")
    (new DateTimeFormatterBuilder).appendLocalized(dateStyle, null).toFormatter(locale)
  }

  /**Singleton date formatter. */
  private val ISOOrdinalDate: DateTimeFormatter = null
  /**Singleton date formatter. */
  private val BasicISODate: DateTimeFormatter = {
    (new DateTimeFormatterBuilder)
      .parseCaseInsensitive
      .appendValue(ISOChronology.yearRule, 4)
      .appendValue(ISOChronology.monthOfYearRule, 2)
      .appendValue(ISOChronology.dayOfMonthRule, 2)
      .optionalStart
      .appendOffset("Z", false, false)
      .optionalStart
      .appendLiteral('[')
      .appendZoneId
      .appendLiteral(']')
      .toFormatter
  }
  /**
   * Creates a formatter using the specified pattern.
   * <p>
   * This method will create a formatter based on a simple pattern of letters and symbols.
   * For example, {@code d MMM yyyy} will format 2008-12-03 as '3 Dec 2008'.
   * <p>
   * See {@link #pattern ( String )} for details of the pattern.
   * <p>
   * The returned formatter will use the specified locale, but this can be changed
   * using {@link DateTimeFormatter#withLocale ( Locale )}.
   *
   * @param pattern the pattern to use, not null
   * @param locale the locale to use, not null
   * @return the formatter based on the pattern, never null
   * @throws IllegalArgumentException if the pattern is invalid
   * @see DateTimeFormatterBuilder#appendPattern ( String )
   */
  def pattern(pattern: String, locale: Locale): DateTimeFormatter =
    (new DateTimeFormatterBuilder).appendPattern(pattern).toFormatter(locale)

  /**
   * Returns the ISO date formatter that prints/parses a date without an offset.
   * <p>
   * This is the ISO-8601 extended format:<br />
   * {@code yyyy -Www-D }
   * <p>
   * The year will print 4 digits, unless this is insufficient, in which
   * case the full year will be printed together with a positive/negative sign.
   *
   * @return the ISO week date formatter, never null
   */
  def isoWeekDate: DateTimeFormatter = ISOWeekDate

  /**
   * Returns a locale specific time format of medium length.
   * <p>
   * This returns a formatter that will print/parse a medium length time format.
   * The exact format pattern used varies by locale, which is determined from the
   * locale on the formatter. That locale is initialized by method.
   * If a new formatter is obtained using {@link DateTimeFormatter#withLocale ( Locale ) }
   * then it will typically change the pattern in use to the appropriate medium
   * length time format for that new locale.
   *
   * @param locale the locale to use, not null
   * @return the medium time formatter, never null
   */
  def mediumTime(locale: Locale): DateTimeFormatter = time(FormatStyle.Medium, locale)

  /**Singleton date formatter. */
  private val ISOTime: DateTimeFormatter = {
    (new DateTimeFormatterBuilder)
      .parseCaseInsensitive
      .append(ISOLocalTime)
      .optionalStart
      .appendOffsetId
      .optionalStart
      .appendLiteral('[')
      .appendZoneId
      .appendLiteral(']')
      .toFormatter
  }

  /**
   * Returns a locale specific date-time format, which is typically of full length.
   * <p>
   * This returns a formatter that will print/parse a full length date-time format.
   * The exact format pattern used varies by locale, which is determined from the
   * locale on the formatter. That locale is initialized by method.
   * If a new formatter is obtained using {@link DateTimeFormatter#withLocale ( Locale ) }
   * then it will typically change the pattern in use to the appropriate full
   * length date-time format for that new locale.
   *
   * @param locale the locale to use, not null
   * @return the full date-time formatter, never null
   */
  def fullDateTime(locale: Locale): DateTimeFormatter = dateTime(FormatStyle.Full, locale)

  /**
   * Returns the ISO date formatter that prints/parses a local date without an offset,
   * such as '2007-12-03T10:15:30'.
   * <p>
   * This is the ISO-8601 extended format:<br />
   * {@code yyyy -MM-dd'T'HH:mm[:ss[.S]] }
   * <p>
   * The year will print 4 digits, unless this is insufficient, in which
   * case the full year will be printed together with a positive/negative sign.
   * <p>
   * The seconds will be printed if present in the Calendrical, thus a DateTime
   * will always print the seconds.
   * The nanoseconds will be printed if non-zero.
   * If non-zero, the minimum number of fractional second digits will printed.
   *
   * @return the ISO date formatter, never null
   */
  def isoLocalDateTime: DateTimeFormatter = ISOLocalDateTime

  /**
   * Returns the ISO date formatter that prints/parses an offset date with a zone,
   * such as '2007-12-03T10:15:30+01:00[Europe/Paris]'.
   * <p>
   * This is the ISO-8601 extended format:<br />
   * {@code yyyy -MM-dd'T'HH:mm[:ss[.S]]ZZ[        { ZoneId} ] }
   * <p>
   * The year will print 4 digits, unless this is insufficient, in which
   * case the full year will be printed together with a positive/negative sign.
   * <p>
   * The seconds will be printed if present in the Calendrical, thus an OffsetDateTime
   * will always print the seconds.
   * The nanoseconds will be printed if non-zero.
   * If non-zero, the minimum number of fractional second digits will printed.
   * <p>
   * The offset will print and parse an offset with seconds even though that
   * is not part of the ISO-8601 standard.
   *
   * @return the ISO date formatter, never null
   */
  def isoZonedDateTime: DateTimeFormatter = ISOZonedDateTime

  /**Singleton date formatter. */
  private val ISOWeekDate: DateTimeFormatter = {
    (new DateTimeFormatterBuilder)
      .parseCaseInsensitive
      .appendValue(ISOChronology.weekBasedYearRule, 4, 10, SignStyle.ExceedsPad)
      .appendLiteral("-W")
      .appendValue(ISOChronology
      .weekOfWeekBasedYearRule, 2)
      .appendLiteral('-')
      .appendValue(ISOChronology.dayOfWeekRule, 1)
      .optionalStart
      .appendOffsetId
      .optionalStart
      .appendLiteral('[')
      .appendZoneId
      .appendLiteral(']')
      .toFormatter
  }
}