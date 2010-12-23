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

/**
 * Provides common implementations of {@code DateResolver}.
 * <p>
 * DateResolvers is a utility class.
 * All resolvers returned are immutable and thread-safe.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object DateResolvers {

  /**
   * Returns the strict resolver which does not manipulate the state
   * in any way, resulting in an exception for all invalid values.
   *
   * @return the strict resolver, never null
   */
  def strict: DateResolver = Strict

  private object Strict extends Strict

    /**
   * Class implementing strict resolver.
   */
  @SerialVersionUID(1L)
  private class Strict extends DateResolver with Serializable {
    private def readResolve: AnyRef = Strict

    /** {@inheritDoc} */
    override def resolveDate(year: Int, monthOfYear: MonthOfYear, dayOfMonth: Int): LocalDate = {
      LocalDate.of(year, monthOfYear, dayOfMonth)
    }
  }

  /**
   * Returns the previous valid day resolver, which adjusts the date to be
   * valid by moving to the last valid day of the month.
   *
   * @return the previous valid day resolver, never null
   */
  def previousValid: DateResolver = PreviousValid

  private object PreviousValid extends PreviousValid

  /**
   * Class implementing previousValid resolver.
   */
  @SerialVersionUID(1L)
  private class PreviousValid extends DateResolver with Serializable {
    /** {@inheritDoc} */
    override def resolveDate(year: Int, monthOfYear: MonthOfYear, dayOfMonth: Int): LocalDate = {
      var lastDay: Int = monthOfYear.getLastDayOfMonth(ISOChronology.isLeapYear(year))
      if (dayOfMonth > lastDay) {
        return LocalDate.of(year, monthOfYear, lastDay)
      }
      return LocalDate.of(year, monthOfYear, dayOfMonth)
    }

    private def readResolve: AnyRef =  PreviousValid
  }

  /**
   * Returns the next valid day resolver, which adjusts the date to be
   * valid by moving to the first of the next month.
   *
   * @return the next valid day resolver, never null
   */
  def nextValid: DateResolver = NextValid

  private object NextValid extends NextValid

    /**
   * Class implementing nextValid resolver.
   */
  @SerialVersionUID(1L)
  private class NextValid extends DateResolver with Serializable {
    private def readResolve: AnyRef = NextValid

    /** {@inheritDoc} */
    override def resolveDate(year: Int, monthOfYear: MonthOfYear, dayOfMonth: Int): LocalDate = {
      var len: Int = monthOfYear.lengthInDays(ISOChronology.isLeapYear(year))
      if (dayOfMonth > len) {
        return LocalDate.of(year, monthOfYear.next, 1)
      }
      return LocalDate.of(year, monthOfYear, dayOfMonth)
    }
  }

  /**
   * Returns the part lenient resolver, which adjusts the date to be
   * valid by moving it to the next month by the number of days that
   * are invalid up to the 31st of the month.
   *
   * @return the part lenient resolver, never null
   */
  def partLenient: DateResolver = PartLenient

  private object PartLenient extends PartLenient

  /**
   * Class implementing partLenient resolver.
   */
  @SerialVersionUID(1L)
  private class PartLenient extends DateResolver with Serializable {
    private def readResolve: AnyRef = PartLenient

    /** {@inheritDoc} */
    override def resolveDate(year: Int, monthOfYear: MonthOfYear, dayOfMonth: Int): LocalDate = {
      var len: Int = monthOfYear.lengthInDays(ISOChronology.isLeapYear(year))
      if (dayOfMonth > len) {
        return LocalDate.of(year, monthOfYear.next, dayOfMonth - len)
      }
      return LocalDate.of(year, monthOfYear, dayOfMonth)
    }
  }
}