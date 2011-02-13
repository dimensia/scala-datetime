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
package scalax.time

/**
 * These methods are proposed for {@code java.lang.Math}
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object MathUtils {
  /**
   * Safely increments an int.
   *
   * @param value  the value to increment
   * @return the result
   * @throws ArithmeticException if the result overflows a long
   */
  def safeIncrement(value: Int): Int = {
    if (value == Int.MaxValue) {
      throw new ArithmeticException("Int.MaxValue cannot be incremented")
    }
    return value + 1
  }

  /**
   * Safely decrements an int.
   *
   * @param value  the value to decrement
   * @return the result
   * @throws ArithmeticException if the result overflows a long
   */
  def safeDecrement(value: Int): Int = {
    if (value == Int.MinValue) {
      throw new ArithmeticException("Int.MinValue cannot be decremented")
    }
    return value - 1
  }

  /**
   * Safely increments a long.
   *
   * @param value  the value to increment
   * @return the result
   * @throws ArithmeticException if the result overflows a long
   */
  def safeIncrement(value: Long): Long = {
    if (value == Long.MaxValue) {
      throw new ArithmeticException("Long.MaxValue cannot be incremented")
    }
    return value + 1
  }

  /**
   * Safely adds two int values.
   *
   * @param a  the first value
   * @param b  the second value
   * @return the result
   * @throws ArithmeticException if the result overflows an int
   */
  def safeAdd(a: Int, b: Int): Int = {
    val sum: Int = a + b
    if ((a ^ sum) < 0 && (a ^ b) >= 0) {
      throw new ArithmeticException("Addition overflows an int: " + a + " + " + b)
    }
    return sum
  }

  /**
   * Multiply two values throwing an exception if overflow occurs.
   *
   * @param a  the first value
   * @param b  the second value
   * @return the new total
   * @throws ArithmeticException if the result overflows a long
   */
  def safeMultiply(a: Long, b: Long): Long = {
    if (b == 1) {
      return a
    }
    if (a == 1) {
      return b
    }
    if (a == 0 || b == 0) {
      return 0
    }
    val total: Long = a * b
    if (total / b != a || a == Long.MinValue && b == -1 || b == Long.MinValue && a == -1) {
      throw new ArithmeticException("Multiplication overflows a long: " + a + " * " + b)
    }
    return total
  }

  /**
   * Safely compare one long with another.
   *
   * @param a  the first value
   * @param b  the second value
   * @return negative if a is less than b, positive if a is greater than b, zero if equal
   */
  def safeCompare(a: Long, b: Long): Int = {
    if (a < b) {
      return -1
    }
    if (a > b) {
      return 1
    }
    return 0
  }

  def ==!(a: Long, b: Long) = safeCompare(a, b)

  /**
   * Returns the floor division.
   * <p>
   * This returns {@code 0} for {@code floorDiv(0, 4)}.<br />
   * This returns {@code -1} for {@code floorDiv(-1, 4)}.<br />
   * This returns {@code -1} for {@code floorDiv(-2, 4)}.<br />
   * This returns {@code -1} for {@code floorDiv(-3, 4)}.<br />
   * This returns {@code -1} for {@code floorDiv(-4, 4)}.<br />
   * This returns {@code -2} for {@code floorDiv(-5, 4)}.<br />
   *
   * @param a  the dividend
   * @param b  the divisor
   * @return the floor division
   */
  def floorDiv(a: Long, b: Long): Long = (if (a >= 0) a / b else ((a + 1) / b) - 1)

  /**
   * Returns the floor modulus.
   * <p>
   * This returns {@code 0} for {@code floorMod(0, 4)}.<br />
   * This returns {@code 1} for {@code floorMod(-1, 4)}.<br />
   * This returns {@code 2} for {@code floorMod(-2, 4)}.<br />
   * This returns {@code 3} for {@code floorMod(-3, 4)}.<br />
   * This returns {@code 0} for {@code floorMod(-4, 4)}.<br />
   *
   * @param a  the dividend
   * @param b  the divisor
   * @return the floor modulus (positive)
   */
  def floorMod(a: Int, b: Int): Int = ((a % b) + b) % b

  /**
   * Safely adds two long values.
   *
   * @param a  the first value
   * @param b  the second value
   * @return the result
   * @throws ArithmeticException if the result overflows a long
   */
  def safeAdd(a: Long, b: Long): Long = {
    val sum: Long = a + b
    if ((a ^ sum) < 0 && (a ^ b) >= 0) {
      throw new ArithmeticException("Addition overflows a long: " + a + " + " + b)
    }
    return sum
  }

  def +!(a: Long, b: Long) = safeAdd(a, b)

  /**
   * Safely subtracts one long from another.
   *
   * @param a  the first value
   * @param b  the second value to subtract from the first
   * @return the result
   * @throws ArithmeticException if the result overflows a long
   */
  def safeSubtract(a: Long, b: Long): Long = {
    var result: Long = a - b
    if ((a ^ result) < 0 && (a ^ b) < 0) {
      throw new ArithmeticException("Subtraction overflows a long: " + a + " - " + b)
    }
    return result
  }

  def -!(a: Long, b: Long) = safeSubtract(a, b)

  /**
   * Negates the input value, throwing an exception if an overflow occurs.
   *
   * @param value  the value to negate
   * @return the negated value
   * @throws ArithmeticException if the value is MinValue and cannot be negated
   */
  def safeNegate(value: Int): Int = {
    if (value == Int.MinValue) {
      throw new ArithmeticException("Int.MinValue cannot be negated")
    }
    return -value
  }

  def -!(value: Int) = safeNegate(value)

  /**
   * Returns the floor modulus.
   * <p>
   * This returns {@code 0} for {@code floorMod(0, 4)}.<br />
   * This returns {@code 3} for {@code floorMod(-1, 4)}.<br />
   * This returns {@code 2} for {@code floorMod(-2, 4)}.<br />
   * This returns {@code 1} for {@code floorMod(-3, 4)}.<br />
   * This returns {@code 0} for {@code floorMod(-4, 4)}.<br />
   * This returns {@code 3} for {@code floorMod(-5, 4)}.<br />
   *
   * @param a  the dividend
   * @param b  the divisor
   * @return the floor modulus (positive)
   */
  def floorMod(a: Long, b: Int): Int = (((a % b) + b) % b).toInt

  /**
   * Safely compare one int with another.
   *
   * @param a  the first value
   * @param b  the second value
   * @return negative if a is less than b, positive if a is greater than b, zero if equal
   */
  def safeCompare(a: Int, b: Int): Int = {
    if (a < b) return -1
    if (a > b) return 1
    return 0
  }

  /**
   * Returns the floor division.
   * <p>
   * This returns {@code 0} for {@code floorDiv(0, 4)}.<br />
   * This returns {@code -1} for {@code floorDiv(-1, 4)}.<br />
   * This returns {@code -1} for {@code floorDiv(-2, 4)}.<br />
   * This returns {@code -1} for {@code floorDiv(-3, 4)}.<br />
   * This returns {@code -1} for {@code floorDiv(-4, 4)}.<br />
   * This returns {@code -2} for {@code floorDiv(-5, 4)}.<br />
   *
   * @param a  the dividend
   * @param b  the divisor
   * @return the floor division
   */
  def floorDiv(a: Int, b: Int): Int = (if (a >= 0) a / b else ((a + 1) / b) - 1)

  /**
   * Safely multiply a long by an int.
   *
   * @param a  the first value
   * @param b  the second value
   * @return the new total
   * @throws ArithmeticException if the result overflows a long
   */
  def safeMultiply(a: Long, b: Int): Long = {
    b match {
      case -1 =>
        if (a == Long.MinValue) {
          throw new ArithmeticException("Multiplication overflows a long: " + a + " * " + b)
        }
        return -a
      case 0 =>
        return 0L
      case 1 =>
        return a
      case _ => {
        var total: Long = a * b
        if (total / b != a) {
          throw new ArithmeticException("Multiplication overflows a long: " + a + " * " + b)
        }
        return total
      }
    }
  }

  /**
   * Safely subtracts one int from another.
   *
   * @param a  the first value
   * @param b  the second value to subtract from the first
   * @return the result
   * @throws ArithmeticException if the result overflows an int
   */
  def safeSubtract(a: Int, b: Int): Int = {
    var result: Int = a - b
    if ((a ^ result) < 0 && (a ^ b) < 0) {
      throw new ArithmeticException("Subtraction overflows an int: " + a + " - " + b)
    }
    return result
  }

  /**
   * Safely convert a long to an int.
   *
   * @param value  the value to convert
   * @return the int value
   * @throws ArithmeticException if the result overflows an int
   */
  def safeToInt(value: Long): Int = {
    if (value > Int.MaxValue || value < Int.MinValue) {
      throw new ArithmeticException("Calculation overflows an int: " + value)
    }
    return value.toInt
  }

  /**
   * Returns the floor modulus.
   * <p>
   * This returns {@code 0} for {@code floorMod(0, 4)}.<br />
   * This returns {@code 1} for {@code floorMod(-1, 4)}.<br />
   * This returns {@code 2} for {@code floorMod(-2, 4)}.<br />
   * This returns {@code 3} for {@code floorMod(-3, 4)}.<br />
   * This returns {@code 0} for {@code floorMod(-4, 4)}.<br />
   *
   * @param a  the dividend
   * @param b  the divisor
   * @return the floor modulus (positive)
   */
  def floorMod(a: Long, b: Long): Long = ((a % b) + b) % b

  /**
   * Negates the input value, throwing an exception if an overflow occurs.
   *
   * @param value  the value to negate
   * @return the negated value
   * @throws ArithmeticException if the value is MinValue and cannot be negated
   */
  def safeNegate(value: Long): Long = {
    if (value == Long.MinValue) {
      throw new ArithmeticException("Long.MinValue cannot be negated")
    }
    return -value
  }

  /**
   * Safely decrements a long.
   *
   * @param value  the value to decrement
   * @return the result
   * @throws ArithmeticException if the result overflows a long
   */
  def safeDecrement(value: Long): Long = {
    if (value == Long.MinValue) {
      throw new ArithmeticException("Long.MinValue cannot be decremented")
    }
    return value - 1
  }

  /**
   * Safely multiply one int by another.
   *
   * @param a  the first value
   * @param b  the second value
   * @return the result
   * @throws ArithmeticException if the result overflows an int
   */
  def safeMultiply(a: Int, b: Int): Int = {
    var total: Long = a.toLong * b.toLong
    if (total < Int.MinValue || total > Int.MaxValue) {
      throw new ArithmeticException("Multiplication overflows an int: " + a + " * " + b)
    }
    return total.toInt
  }
}