package time

object SafeLong {
  implicit def long2SafeLong(value: Long) = new SafeLong(value)
}

class SafeLong(value: Long) {

  import SafeLong._

  /**
   * Safely adds two long values.
   *
   * @param a  the first value
   * @param b  the second value
   * @return the result
   * @throws ArithmeticException if the result overflows a long
   */
  def +!(other: Long): Long = {
    val sum: Long = value + other
    if ((other ^ sum) < 0 && (value ^ other) >= 0) {
      throw new ArithmeticException("Addition overflows a long: " + value + " + " + other)
    }
    return sum
  }

  def -!(other: Long): Long = {
    val result: Long = value - other
    if ((value ^ result) < 0 && (value ^ other) < 0) {
      throw new ArithmeticException("Subtraction overflows a long: " + value + " - " + other)
    }
    return result
  }

  /**
   * Multiply two values throwing an exception if overflow occurs.
   *
   * @param a  the first value
   * @param b  the second value
   * @return the new total
   * @throws ArithmeticException if the result overflows a long
   */
  def *!(other: Long): Long = {
    if (other == 1) {
      return value
    }
    if (value == 1) {
      return other
    }
    if (value == 0 || other == 0) {
      return 0
    }
    val total: Long = value * other
    if (total / other != value || value == Long.MinValue && other == -1 || other == Long.MinValue && value == -1) {
      throw new ArithmeticException("Multiplication overflows a long: " + value + " * " + other)
    }
    return total
  }

  /**
   * Safely multiply a long by an int.
   *
   * @param a  the first value
   * @param b  the second value
   * @return the new total
   * @throws ArithmeticException if the result overflows a long
   */
  def *!(other: Int): Long = {
    other match {
      case -1 =>
        if (value == Long.MinValue) {
          throw new ArithmeticException("Multiplication overflows a long: " + value + " * " + other)
        }
        return -value
      case 0 =>
        return 0L
      case 1 =>
        return value
      case _ => {
        val total: Long = value * other
        if (total / other != value) {
          throw new ArithmeticException("Multiplication overflows a long: " + value + " * " + other)
        }
        return total
      }
    }
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
  def floorDiv(other: Long): Long = (if (value >= 0) value / other else ((value + 1) / other) - 1)

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
  def floorMod(other: Int): Int = (((value % other) + other) % other).toInt

  /**
   * Safely increments a long.
   *
   * @param value  the value to increment
   * @return the result
   * @throws ArithmeticException if the result overflows a long
   */
  def ++! : Long = {
    if (value == Long.MaxValue) {
      throw new ArithmeticException("Long.MaxValue cannot be incremented")
    }
    return value + 1
  }

  /**
   * Safely decrements a long.
   *
   * @param value  the value to decrement
   * @return the result
   * @throws ArithmeticException if the result overflows a long
   */
  def --! : Long = {
    if (value == Long.MinValue) throw new ArithmeticException("Long.MinValue cannot be decremented")
    else value - 1
  }

  /**
   * Negates the input value, throwing an exception if an overflow occurs.
   *
   * @param value  the value to negate
   * @return the negated value
   * @throws ArithmeticException if the value is MinValue and cannot be negated
   */
  def safeNegate: Long = {
    if (value == Long.MinValue) throw new ArithmeticException("Long.MinValue cannot be negated")
    else -value
  }

  /**
   * Safely compare one long with another.
   *
   * @param a  the first value
   * @param b  the second value
   * @return negative if a is less than b, positive if a is greater than b, zero if equal
   */
  def safeCompare(other: Long): Int = {
    if (value < other)
      -1
    else if (value > other)
      1
    else
      0
  }

  /**
   * Safely convert a long to an int.
   *
   * @param value  the value to convert
   * @return the int value
   * @throws ArithmeticException if the result overflows an int
   */
  def safeToInt: Int = {
    if (value > Int.MaxValue || value < Int.MinValue) throw new ArithmeticException("Calculation overflows an int: " + value)
    else value.toInt
  }
}

object SafeInt {
  implicit def int2SafeInt(value: Int) = new SafeInt(value)
}

class SafeInt(value: Int) {
  import SafeInt._

  /**
   * Safely adds two int values.
   *
   * @param a  the first value
   * @param b  the second value
   * @return the result
   * @throws ArithmeticException if the result overflows an int
   */
  def +!(other: Int): Int = {
    val sum: Int = value + other
    if ((value ^ sum) < 0 && (value ^ other) >= 0) {
      throw new ArithmeticException("Addition overflows an int: " + value + " + " + other)
    }
    return sum
  }

  /**
   * Safely subtracts one int from another.
   *
   * @param a  the first value
   * @param b  the second value to subtract from the first
   * @return the result
   * @throws ArithmeticException if the result overflows an int
   */
  def -!(other: Int): Int = {
    var result: Int = value - other
    if ((value ^ result) < 0 && (value ^ other) < 0) {
      throw new ArithmeticException("Subtraction overflows an int: " + value + " - " + other)
    }
    return result
  }

  /**
   * Safely multiply one int by another.
   *
   * @param a  the first value
   * @param b  the second value
   * @return the result
   * @throws ArithmeticException if the result overflows an int
   */
  def *!(other: Int): Int = {
    val total: Long = value.toLong * other.toLong
    if (total < Int.MinValue || total > Int.MaxValue) {
      throw new ArithmeticException("Multiplication overflows an int: " + value + " * " + other)
    }
    return total.toInt
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
  def floorDiv(other: Int): Int = (if (value >= 0) value / other else ((value + 1) / other) - 1)

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
  def floorMod(other: Int): Int = ((value % other) + other) % other

  /**
   * Safely increments an int.
   *
   * @param value  the value to increment
   * @return the result
   * @throws ArithmeticException if the result overflows a long
   */
  def ++! : Int = {
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
  def --! : Int = {
    if (value == Int.MinValue) {
      throw new ArithmeticException("Int.MinValue cannot be decremented")
    }
    return value - 1
  }

  /**
   * Negates the input value, throwing an exception if an overflow occurs.
   *
   * @param value  the value to negate
   * @return the negated value
   * @throws ArithmeticException if the value is MinValue and cannot be negated
   */
  def safeNegate: Int = {
    if (value == Int.MinValue) throw new ArithmeticException("Int.MinValue cannot be negated")
    else -value
  }

  /**
   * Safely compare one int with another.
   *
   * @param a  the first value
   * @param b  the second value
   * @return negative if a is less than b, positive if a is greater than b, zero if equal
   */
  def safeCompare(other: Int): Int = {
    if (value < other) -1
    else if (value > other) 1
    else 0
  }
}