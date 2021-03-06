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

import java.io.IOException
import scalax.time.CalendricalException

/**
 * An exception thrown when an error occurs during printing.
 *
 * @author Stephen Colebourne
 */

/**
 * Constructs a new exception with the specified message and cause.
 *
 * @param message the message to use for this exception, may be null
 * @param throwable the throwable to store as the cause, may be null
 */
@SerialVersionUID(1L)
class CalendricalPrintException(message: String, throwable: Throwable = null)
  extends CalendricalException(message, throwable) {

  /**
   * Checks if the cause of this exception was an IOException, and if so
   * re-throws it
   * <p>
   * This method is useful if you call a printer with an open stream or
   * writer and want to ensure that IOExceptions are not lost.
   * <pre>
   * try   {
   *   printer.print(writer, dateTime);
   * } catch (CalendricalFormatException ex)   {
   *   ex.rethrowIOException();
   *   // if code reaches here exception was caused by date-time issues
   * }
   * </pre>
   * Note that calling this method will re-throw the original IOException,
   * causing this CalendricalFormatException to be lost.
   *
   * @throws IOException if the cause of this exception is an IOException
   */
  def rethrowIOException: Unit = if (getCause.isInstanceOf[IOException]) throw getCause.asInstanceOf[IOException]
}