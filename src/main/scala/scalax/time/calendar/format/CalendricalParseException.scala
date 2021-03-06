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

import scalax.time.CalendricalException

/**
 * An exception thrown when an error occurs during parsing.
 *
 * @author Stephen Colebourne
 */

/**
 * Constructs a new exception with the specified message.
 *
 * @param message the message to use for this exception, may be null
 * @param parsedString the parsed text, should not be null
 * @param errorIndex the index in the parsed string that was invalid, should be a valid index
 * @param cause the cause exception, may be null
 */
@SerialVersionUID(1L)
class CalendricalParseException(message: String, parsedString: String, errorIndex: Int, cause: Throwable = null)
  extends CalendricalException(message, cause) {

  /**
   * Returns the string that was being parsed.
   *
   * @return the string that was being parsed, should not be null
   */
  def getParsedString: String = parsedString

  /**
   * Returns the index where the error was found.
   *
   * @return the index in the parsed string that was invalid, should be a valid index
   */
  def getErrorIndex: Int = errorIndex
}