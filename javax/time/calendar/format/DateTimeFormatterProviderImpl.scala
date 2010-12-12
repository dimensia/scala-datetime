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
package javax.time.calendar.format

import java.text.DateFormat
import java.text.SimpleDateFormat
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentMap
import javax.time.calendar.Chronology

/**
 * The Service Provider Implementation to obtain date-time formatters.
 * <p>
 * DateTimeFormatterProviderImpl is thread-safe.
 *
 * @author Stephen Colebourne
 */
object DateTimeFormatterProviderImpl {
  /**Cache of formatters. */
  private val FormatterCache: ConcurrentMap[String, AnyRef] = new ConcurrentHashMap[String, AnyRef](16, 0.75f, 2)
}

class DateTimeFormatterProviderImpl extends DateTimeFormatterProvider {

  import DateTimeFormatterProviderImpl._

  /**
   * Converts the enum style to the old format style.
   * @param style the enum style, not null
   * @return the int style
   */
  private def convertStyle(style: DateTimeFormatterBuilder.FormatStyle): Int = style.ordinal

  /** { @inheritDoc }*/
  def getFormatter(dateStyle: DateTimeFormatterBuilder.FormatStyle, timeStyle: DateTimeFormatterBuilder.FormatStyle, locale: Locale, chronology: Chronology): DateTimeFormatter = {
    if (dateStyle == null && timeStyle == null) {
      throw new IllegalArgumentException("Date and Time style must not both be null")
    }
    var key: String = chronology.getName + '|' + locale.toString + '|' + dateStyle + timeStyle
    var cached: AnyRef = FormatterCache.get(key)
    if (cached != null) {
      if (cached.equals("")) {
        throw new IllegalArgumentException("Unable to convert DateFormat to DateTimeFormatter")
      }
      return cached.asInstanceOf[DateTimeFormatter]
    }
    var dateFormat: DateFormat = null
    if (dateStyle != null) {
      if (timeStyle != null) {
        dateFormat = DateFormat.getDateTimeInstance(convertStyle(dateStyle), convertStyle(timeStyle), locale)
      }
      else {
        dateFormat = DateFormat.getDateInstance(convertStyle(dateStyle), locale)
      }
    }
    else {
      dateFormat = DateFormat.getTimeInstance(convertStyle(timeStyle), locale)
    }
    if (dateFormat.isInstanceOf[SimpleDateFormat]) {
      var pattern: String = (dateFormat.asInstanceOf[SimpleDateFormat]).toPattern
      var formatter: DateTimeFormatter = (new DateTimeFormatterBuilder).appendPattern(pattern).toFormatter(locale)
      FormatterCache.putIfAbsent(key, formatter)
      return formatter
    }
    FormatterCache.putIfAbsent(key, "")
    throw new IllegalArgumentException("Unable to convert DateFormat to DateTimeFormatter")
  }

  /** { @inheritDoc }*/
  def getAvailableLocales: Array[Locale] = DateFormat.getAvailableLocales
}