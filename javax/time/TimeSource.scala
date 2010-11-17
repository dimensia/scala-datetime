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
package javax.time

import java.io.Serializable

/**
 * A source providing access to the current instant.
 * <p>
 * The Time Framework for Java abstracts the concept of the "current time" into two interfaces
 * -     { @code TimeSource } and     { @link javax.time.calendar.Clock Clock }.
 * This class, provides access to the current     { @code Instant } which is independent of
 * local factors such as time-zone and cannot be queried for human-scale fields.
 * By comparison,     { @code Clock } provides access to the current date and time, via
 * human-scale fields, but requires a time-zone.
 * <p>
 * The purpose of this abstraction is to allow alternate time-sources
 * to be plugged in as and when required. Applications use an object to obtain
 * the current time rather than a static method. This simplifies testing.
 *
 * <h4>Best practice</h4>
 * The recommended best practice for most applications is to <i>avoid using the static methods</i>.
 * Instead, the main application should obtain the current time from a     { @code TimeSource }
 * instance that is passed to the object or method.
 * This approach is typically implemented using a dependency injection framework.
 * <pre>
 * public class MyBean     {
 *   final TimeSource timeSource;
 *   &#064;Inject MyBean(TimeSource ts)     {
 *       this.timeSource = ts;
 * }
 *   public void process(Instant eventTime)     {
 *     if (eventTime.isBefore(timeSource.instant())     {
 *       ...
 * }
 * }
 * }
 * </pre>
 * This approach allows alternate time-source implementations, such as
 * { @link # fixed ( InstantProvider ) fixed } or     { @link # offsetSystem ( Duration ) offsetSystem }
 * to be used during testing.
 * <pre>
 * public void test_process()     {
 *   MyBean bean = new MyBean(TimeSource.fixed(Instant.EPOCH))     {
 *   assert ...
 * }
 * </pre>
 *
 * <h4>Accuracy</h4>
 * The main method used by applications is     { @link # instant ( ) }.
 * This returns the best value available for the current instant ignoring leap-seconds.
 * To achieve this, the instant may not be fully accurate around a leap-second.
 * <p>
 * Two alternative methods are available and may return a more accurate instant depending
 * on the implementation. The instant in UTC, taking into account leap seconds, can be
 * obtained using     { @link # utcInstant ( ) }. The instant in TAI, which is a simple incrementing number
 * ignoring all human time concepts, can be obtained using     { @link # taiInstant ( ) }.
 * See the relevant classes for more detail.
 *
 * <h4>Implementation notes</h4>
 * <p>
 * { @code TimeSource } is an abstract class and must be implemented with care
 * to ensure other classes in the framework operate correctly.
 * All instantiable implementations must be final, immutable and thread-safe.
 * <p>
 * Subclass implementations should implement     { @code Serializable } wherever possible.
 * They should also implement     { @code equals ( ) },     { @code hashCode ( ) } and
 * { @code toString ( ) } based on their state.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
object TimeSource {

  /**
   * Implementation of a time-source that always returns the latest time from
   * { @link System # currentTimeMillis ( ) }.
   */
  private[time] object SystemTimeSource

  /**Restricted constructor. */
  @SerialVersionUID(1L)
  private[time] final class SystemTimeSource private() extends TimeSource with Serializable {

    /** { @inheritDoc }*/
    def instant: Instant = Instant.ofEpochMillis(System.currentTimeMillis)

    /** { @inheritDoc }*/
    override def toString: String = "SystemTimeSource"

    /** { @inheritDoc }*/
    override def millis: Long = {
      return System.currentTimeMillis
    }

    /**Resolve singletons. */
    private def readResolve: AnyRef = {
      return INSTANCE
    }
  }

  /**
   * Gets a time-source that always returns the same     { @code Instant }.
   * <p>
   * This method converts the     { @code InstantProvider } to an     { @code Instant }
   * which it then returns from the     { @code TimeSource }.
   * <p>
   * The returned implementation is     { @code Serializable }
   *
   * @param fixedInstantProvider the instant to return from each call to the time-source
   * @return a { @code TimeSource } that always returns the same instant, never null
   */
  def fixed(fixedInstantProvider: InstantProvider): TimeSource = {
    Instant.checkNotNull(fixedInstantProvider, "InstantProvider must not be null")
    var instant: Instant = Instant.of(fixedInstantProvider)
    return new TimeSource.FixedTimeSource(instant)
  }

  /**
   * Gets a time-source that obtains the current instant using
   * the system millisecond clock.
   * <p>
   * The time-source wraps     { @link System # currentTimeMillis ( ) }, thus it has
   * at best millisecond resolution.
   * <p>
   * The returned implementation is     { @code Serializable }
   *
   * @return a { @code TimeSource } that uses the system millisecond clock, never null
   */
  def system: TimeSource = SystemTimeSource

  /**
   * Implementation of a time-source that always returns the same instant.
   * This is typically used for testing.
   * Restricted constructor. */
  @SerialVersionUID(1L)
  private[time] final class FixedTimeSource private(instant: Instant) extends TimeSource with Serializable {
    /** { @inheritDoc }*/
    override def equals(obj: AnyRef): Boolean = {
      if (obj.isInstanceOf[TimeSource.FixedTimeSource]) instant.equals((obj.asInstanceOf[TimeSource.FixedTimeSource]).instant)
      else false
    }

    /** { @inheritDoc }*/
    override def toString: String = "FixedTimeSource[" + instant + ']'

    /** { @inheritDoc }*/
    override def hashCode: Int = instant.hashCode
  }

  /**
   * Gets a time-source that obtains the current instant using the system
   * millisecond clock and adjusts by a fixed offset.
   * <p>
   * The time-source wraps     { @link System # currentTimeMillis ( ) }, thus it has
   * at best millisecond resolution.
   * <p>
   * The final instant is adjusted by adding the offset.
   * This is useful for simulating an application running at a later or earlier
   * point in time.
   * <p>
   * The returned implementation is     { @code Serializable }
   *
   * @param offset the duration by which this time-source is offset from the system millisecond clock
   * @return a { @code TimeSource } that is offset from the system millisecond clock, never null
   */
  def offsetSystem(offset: Duration): TimeSource = {
    Instant.checkNotNull(offset, "Duration must not be null")
    if (offset.equals(Duration.ZERO)) SystemTimeSource
    else new TimeSource.OffsetSystemTimeSource(offset)
  }

  /**
   * Implementation of a time-source that returns the latest time from
   * { @link System # currentTimeMillis ( ) } plus an offset.
   * Restricted constructor. */
  @SerialVersionUID(1L)
  private[time] final class OffsetSystemTimeSource private (offset: Duration) extends TimeSource with Serializable {

    /** { @inheritDoc }*/
    def instant: Instant = Instant.ofEpochMillis(System.currentTimeMillis).plus(offset)

    /** { @inheritDoc }*/
    override def hashCode: Int = offset.hashCode

    /** { @inheritDoc }*/
    override def toString: String = "OffsetSystemTimeSource[" + offset + ']'

    /** { @inheritDoc }*/
    override def equals(obj: AnyRef): Boolean = {
      if (obj.isInstanceOf[TimeSource.OffsetSystemTimeSource]) offset.equals((obj.asInstanceOf[TimeSource.OffsetSystemTimeSource]).offset)
      else false
    }
  }

}

/**
 * Constructor accessible by subclasses.
 */
abstract class TimeSource protected() {
  /**
   * Gets the current     { @code TAIInstant }.
   * <p>
   * The TAI time-scale is a simple incrementing number of seconds from the TAI epoch of 1958-01-01(TAI).
   * It ignores all human concepts of time such as days.
   * An accurate implementation of this abstract class will return a TAI value in
   * accordance with international standards.
   * <p>
   * The default implementation of this method converts the value from     { @code instant ( ) }
   * and thus is no more accurate than that method.
   * <p>
   * Normally, this method will not throw an exception.
   * However, one possible implementation would be to obtain the time from a
   * central time server across the network. Obviously, in this case the lookup
   * could fail, and so the method is permitted to throw an exception.
   */
  def taiInstant: TAIInstant = TAIInstant.of(instant)

  /**
   * Gets the current     { @code Instant }.
   * <p>
   * The instant returned by this method will vary according to the implementation.
   * For example, the time-source returned by     { @link # system ( ) } will return
   * an instant based on     { @link System # currentTimeMillis ( ) }.
   * <p>
   * Normally, this method will not throw an exception.
   * However, one possible implementation would be to obtain the time from a
   * central time server across the network. Obviously, in this case the lookup
   * could fail, and so the method is permitted to throw an exception.
   *
   * @return the current     { @code Instant } from this time-source, never null
   * @throws CalendricalException if the instant cannot be obtained, not thrown by most implementations
   */
  def instant: Instant

  /**
   * Gets the current     { @code UTCInstant }.
   * <p>
   * The UTC time-scale differs from that used by     { @code Instant } because it includes
   * leap-seconds. An accurate implementation of this abstract class will return a
   * UTC value that includes leap-second information.
   * <p>
   * The default implementation of this method converts the value from     { @code instant ( ) }
   * and thus is no more accurate than that method.
   * <p>
   * Normally, this method will not throw an exception.
   * However, one possible implementation would be to obtain the time from a
   * central time server across the network. Obviously, in this case the lookup
   * could fail, and so the method is permitted to throw an exception.
   *
   * @return the current     { @code UTCInstant } from this time-source, never null
   * @throws CalendricalException if the instant cannot be obtained, not thrown by most implementations
   */
  def utcInstant: UTCInstant = UTCInstant.of(instant)

  /**
   * Gets the current millisecond instant.
   * <p>
   * The instant returned by this method will vary according to the implementation.
   * For example, the time-source returned by     { @link # system ( ) } will return
   * { @link System # currentTimeMillis ( ) }.
   * <p>
   * This method is provided for backwards compatibility.
   * New code should use classes such as <code>Instant</code> to represent an instant
   * rather than a raw millisecond value.
   * <p>
   * Normally, this method will not throw an exception.
   * However, one possible implementation would be to obtain the time from a
   * central time server across the network. Obviously, in this case the lookup
   * could fail, and so the method is permitted to throw an exception.
   *
   * @return the current millisecond instant from this time-source, measured from
   *  the Java epoch of 1970-01-01T00:00 UTC, never null
   * @throws CalendricalException if the instant cannot be obtained, not thrown by most implementations
   */
  def millis: Long = instant.toEpochMillisLong
}