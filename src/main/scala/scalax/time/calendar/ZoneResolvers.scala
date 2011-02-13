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
package scalax.time.calendar

import scalax.time.CalendricalException
import scalax.time.Instant
import scalax.time.calendar.zone.ZoneOffsetTransition
import scalax.time.calendar.zone.ZoneRules

/**
 * Provides common implementations of {@code ZoneResolver}.
 * <p>
 * A {@link ZoneResolver} provides a strategy for handling the gaps and overlaps
 * on the time-line that occur due to changes in the offset from UTC, usually
 * caused by Daylight Savings Time.
 * <p>
 * ZoneResolvers is a utility class.
 * All resolvers returned are immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object ZoneResolvers {
  /**
   * Returns the strict zone resolver which rejects all gaps and overlaps
   * as invalid, resulting in an exception.
   *
   * @return the strict resolver, never null
   */
  def strict: ZoneResolver = Strict

  private object Strict extends Strict

  /**
   * Class implementing strict resolver.
   */
  private class Strict extends ZoneResolver {
    /**{@inheritDoc}*/
    protected[calendar] def handleGap(zone: ZoneId, rules: ZoneRules, discontinuity: ZoneOffsetTransition, newDateTime: DateTime, oldDateTime: OffsetDateTime): OffsetDateTime =
      throw new CalendricalException("Local time " + newDateTime + " does not exist in time-zone " + zone + " due to a gap in the local time-line")

    /**{@inheritDoc}*/
    protected[calendar] def handleOverlap(zone: ZoneId, rules: ZoneRules, discontinuity: ZoneOffsetTransition, newDateTime: DateTime, oldDateTime: OffsetDateTime): OffsetDateTime =
      throw new CalendricalException("Local time " + newDateTime + " has two matching offsets, " + discontinuity.getOffsetBefore + " and " + discontinuity.getOffsetAfter + ", in time-zone " + zone)
  }

  /**
   * Returns the pre-transition zone resolver, which returns the instant
   * one nanosecond before the transition for gaps, and the earlier offset
   * for overlaps.
   *
   * @return the pre-transition resolver, never null
   */
  def preTransition: ZoneResolver = PreTransition

  private object PreTransition extends PreTransition

  /**
   * Class implementing preTransition resolver.
   */
  private class PreTransition extends ZoneResolver {
    /**{@inheritDoc}*/
    protected[calendar] def handleGap(zone: ZoneId, rules: ZoneRules, discontinuity: ZoneOffsetTransition, newDateTime: DateTime, oldDateTime: OffsetDateTime): OffsetDateTime = {
      val instantBefore: Instant = discontinuity.getInstant.minusNanos(1)
      OffsetDateTime.ofInstant(instantBefore, discontinuity.getOffsetBefore)
    }

    /**{@inheritDoc}*/
    protected[calendar] def handleOverlap(zone: ZoneId, rules: ZoneRules, discontinuity: ZoneOffsetTransition, newDateTime: DateTime, oldDateTime: OffsetDateTime): OffsetDateTime =
      OffsetDateTime.of(newDateTime, discontinuity.getOffsetBefore)
  }

  /**
   * Returns the post-transition zone resolver, which returns the instant
   * after the transition for gaps, and the later offset for overlaps.
   *
   * @return the post-transition resolver, never null
   */
  def postTransition: ZoneResolver = PostTransition

  private object PostTransition extends PostTransition

  /**
   * Class implementing postTransition resolver.
   */
  private class PostTransition extends ZoneResolver {
    /**{@inheritDoc}*/
    protected[calendar] def handleGap(zone: ZoneId, rules: ZoneRules, discontinuity: ZoneOffsetTransition, newDateTime: DateTime, oldDateTime: OffsetDateTime): OffsetDateTime =
      discontinuity.getDateTimeAfter

    /**{@inheritDoc}*/
    protected[calendar] def handleOverlap(zone: ZoneId, rules: ZoneRules, discontinuity: ZoneOffsetTransition, newDateTime: DateTime, oldDateTime: OffsetDateTime): OffsetDateTime =
      OffsetDateTime.of(newDateTime, discontinuity.getOffsetAfter)
  }

  /**
   * Returns the post-gap-pre-overlap zone resolver, which returns the instant
   * after the transition for gaps, and the earlier offset for overlaps.
   *
   * @return the post-transition resolver, never null
   */
  def postGapPreOverlap: ZoneResolver = PostGapPreOverlap

  private object PostGapPreOverlap extends PostGapPreOverlap

  /**
   * Class implementing postGapPreOverlap resolver.
   */
  private class PostGapPreOverlap extends ZoneResolver {
    /**{@inheritDoc}*/
    protected[calendar] def handleGap(zone: ZoneId, rules: ZoneRules, discontinuity: ZoneOffsetTransition, newDateTime: DateTime, oldDateTime: OffsetDateTime): OffsetDateTime =
      discontinuity.getDateTimeAfter

    /**{@inheritDoc}*/
    protected[calendar] def handleOverlap(zone: ZoneId, rules: ZoneRules, discontinuity: ZoneOffsetTransition, newDateTime: DateTime, oldDateTime: OffsetDateTime): OffsetDateTime =
      OffsetDateTime.of(newDateTime, discontinuity.getOffsetBefore)
  }

  /**
   * Returns the retain offset resolver, which returns the instant after the
   * transition for gaps, and the same offset for overlaps.
   * <p>
   * This resolver is the same as the     { {@link #postTransition()} resolver with
   * one additional rule. When processing an overlap, this resolver attempts
   * to use the same offset as the offset specified in the old date-time.
   * If that offset is invalid then the later offset is chosen
   * <p>
   * This resolver is most commonly useful when adding or subtracting time
   * from a {@code ZonedDateTime}.
   *
   * @return the retain offset resolver, never null
   */
  def retainOffset: ZoneResolver = RetainOffset

  private object RetainOffset extends RetainOffset

  /**
   * Class implementing retain offset resolver.
   */
  private class RetainOffset extends ZoneResolver {
    /**{@inheritDoc}*/
    protected[calendar] def handleGap(zone: ZoneId, rules: ZoneRules, discontinuity: ZoneOffsetTransition, newDateTime: DateTime, oldDateTime: OffsetDateTime): OffsetDateTime =
      discontinuity.getDateTimeAfter

    /**{@inheritDoc}*/
    protected[calendar] def handleOverlap(zone: ZoneId, rules: ZoneRules, discontinuity: ZoneOffsetTransition, newDateTime: DateTime, oldDateTime: OffsetDateTime): OffsetDateTime = {
      if (oldDateTime != null && discontinuity.isValidOffset(oldDateTime.getOffset)) OffsetDateTime.of(newDateTime, oldDateTime.getOffset)
      else OffsetDateTime.of(newDateTime, discontinuity.getOffsetAfter)
    }
  }

  /**
   * Returns the push forward resolver, which changes the time of the result
   * in a gap by adding the lenth of the gap.
   * <p>
   * If the discontinuity is a gap, then the resolver will add the length of
   * the gap in seconds to the local time.
   * For example, given a gap from 01:00 to 02:00 and a time of 01:20, this
   * will add one hour to result in 02:20.
   * <p>
   * If the discontinuity is an overlap, then the resolver will choose the
   * later of the two offsets.
   *
   * @return the push forward resolver, never null
   */
  def pushForward: ZoneResolver = PushForward

  private object PushForward extends PushForward

  /**
   * Class implementing push forward resolver.
   */
  private class PushForward extends ZoneResolver {
    /**{@inheritDoc}*/
    protected[calendar] def handleGap(zone: ZoneId, rules: ZoneRules, discontinuity: ZoneOffsetTransition, newDateTime: DateTime, oldDateTime: OffsetDateTime): OffsetDateTime = {
      val result: DateTime = newDateTime.plus(discontinuity.getTransitionSize)
      OffsetDateTime.of(result, discontinuity.getOffsetAfter)
    }

    /**{@inheritDoc}*/
    protected[calendar] def handleOverlap(zone: ZoneId, rules: ZoneRules, discontinuity: ZoneOffsetTransition, newDateTime: DateTime, oldDateTime: OffsetDateTime): OffsetDateTime =
      OffsetDateTime.of(newDateTime, discontinuity.getOffsetAfter)
  }

  /**
   * Creates a combined resolver, using two different strategies for gap and overlap.
   * <p>
   * If either argument is {@code null} then the  {@link #strict()} resolver is used.
   *
   * @param gapResolver the resolver to use for a gap, null means strict
   * @param overlapResolver the resolver to use for an overlap, null means strict
   * @return the combination resolver, never null
   */
  def combination(_gapResolver: ZoneResolver, _overlapResolver: ZoneResolver): ZoneResolver = {
    var gapResolver = _gapResolver
    var overlapResolver = _overlapResolver
    gapResolver = (if (gapResolver == null) strict else gapResolver)
    overlapResolver = (if (overlapResolver == null) strict else overlapResolver)
    if (gapResolver == overlapResolver) gapResolver
    else new ZoneResolvers.Combination(gapResolver, overlapResolver)
  }

  /**
   * Class implementing combination resolver.
   *
   * Constructor.
   * @param gapResolver the resolver to use for a gap, not null
   * @param overlapResolver the resolver to use for an overlap, not null
   */
  private class Combination(gapResolver: ZoneResolver, overlapResolver: ZoneResolver) extends ZoneResolver {
    /**{@inheritDoc}*/
    protected[calendar] def handleGap(zone: ZoneId, rules: ZoneRules, discontinuity: ZoneOffsetTransition, newDateTime: DateTime, oldDateTime: OffsetDateTime): OffsetDateTime =
      gapResolver.handleGap(zone, rules, discontinuity, newDateTime, oldDateTime)

    /**{@inheritDoc}*/
    protected[calendar] def handleOverlap(zone: ZoneId, rules: ZoneRules, discontinuity: ZoneOffsetTransition, newDateTime: DateTime, oldDateTime: OffsetDateTime): OffsetDateTime =
      overlapResolver.handleOverlap(zone, rules, discontinuity, newDateTime, oldDateTime)
  }

}