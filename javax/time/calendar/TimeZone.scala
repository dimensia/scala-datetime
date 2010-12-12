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

import java.io.IOException
import java.io.ObjectInputStream
import java.io.ObjectStreamException
import java.io.Serializable
import java.io.StreamCorruptedException
import java.util.Collections
import java.util.HashMap
import java.util.Map
import java.util.regex.Matcher
import java.util.regex.Pattern
import javax.time.CalendricalException
import javax.time.Instant
import javax.time.calendar.zone.ZoneRules
import javax.time.calendar.zone.ZoneRulesGroup

/**
 * A time-zone representing the set of rules by which the zone offset
 * varies through the year and historically.
 * <p>
 * Time zones are geographical regions where the same rules for time apply.
 * The rules are defined by governments and change frequently.
 * <p>
 * There are a number of sources of time-zone information available,
 * each represented by an instance of     { @link ZoneRulesGroup }.
 * One group is provided as standard - 'TZDB' - and applications can add more as required.
 * <p>
 * Each group defines a naming scheme for the regions of the time-zone.
 * The format of the region is specific to the group.
 * For example, the 'TZDB' group typically use the format     { area } /    { city },
 * such as 'Europe/London'.
 * <p>
 * Each group typically produces multiple versions of their data.
 * The format of the version is specific to the group.
 * For example, the 'TZDB' group use the format     { year } { letter }, such as '2009b'.
 * <p>
 * In combination, a unique ID is created expressing the time-zone, formed from
 * { groupID } :    { regionID } #    { versionID }.
 * <p>
 * The version can be set to an empty string. This represents the "floating version".
 * The floating version will always choose the latest applicable set of rules.
 * Applications will probably choose to use the floating version, as it guarantees
 * usage of the latest rules.
 * <p>
 * In addition to the group:region#version combinations,     { @code TimeZone }
 * can represent a fixed offset. This has an empty group and version ID.
 * It is not possible to have an invalid instance of a fixed time-zone.
 * <p>
 * The purpose of capturing all this information is to handle issues when
 * manipulating and persisting time-zones. For example, consider what happens if the
 * government of a country changed the start or end of daylight savings time.
 * If you created and stored a date using one version of the rules, and then load it
 * up when a new version of the rules are in force, what should happen?
 * The date might now be invalid, for example due to a gap in the local time-line.
 * By storing the version of the time-zone rules data together with the date, it is
 * possible to tell that the rules have changed and to process accordingly.
 * <p>
 * { @code TimeZone } merely represents the identifier of the zone.
 * The actual rules are provided by     { @link ZoneRules }.
 * One difference is that serializing this class only stores the reference to the zone,
 * whereas serializing     { @code ZoneRules } stores the entire set of rules.
 * <p>
 * After deserialization, or by using the special factory     { @link # ofUnchecked ( String ) ofUnchecked },
 * it is possible for the time-zone to represent a group/region/version combination that is unavailable.
 * Since this class can still be loaded even when the rules cannot, the application can
 * continue. For example, a     { @link ZonedDateTime } instance could still be queried.
 * The application might also take appropriate corrective action.
 * For example, an application might choose to download missing rules from a central server.
 * <p>
 * TimeZone is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object TimeZone {
  /**
   * Obtains an instance of     { @code TimeZone } from an identifier without checking
   * if the time-zone has available rules.
   * <p>
   * The identifier is parsed in a similar manner to     { @link # of ( String ) }.
   * However, there is no check to ensure that the group, region and version resolve
   * to a set of rules that can be loaded.
   * This factory does however check that the identifier meets the acceptable format.
   * <p>
   * This method is intended for advanced use cases.
   * One example might be a system that always retrieves time-zone rules from a remote server.
   * Using this factory allows a     { @code TimeZone }, and thus a     { @code ZonedDateTime },
   * to be created without loading the rules from the remote server.
   *
   * @param zoneID the time-zone identifier, not null
   * @return the time-zone, never null
   * @throws CalendricalException if the time-zone cannot be found
   */
  def ofUnchecked(zoneID: String): TimeZone = ofID(zoneID, false)

  /**
   * Obtains an instance of     { @code TimeZone } using its ID using a map
   * of aliases to supplement the standard zone IDs.
   * <p>
   * Many users of time-zones use short abbreviations, such as PST for
   * 'Pacific Standard Time' and PDT for 'Pacific Daylight Time'.
   * These abbreviations are not unique, and so cannot be used as identifiers.
   * This method allows a map of string to time-zone to be setup and reused
   * within an application.
   *
   * @param timeZoneIdentifier the time-zone id, not null
   * @param aliasMap a map of time-zone IDs (typically abbreviations) to real time-zone IDs, not null
   * @return the time-zone, never null
   * @throws IllegalArgumentException if the time-zone cannot be found
   */
  def of(timeZoneIdentifier: String, aliasMap: Map[String, String]): TimeZone = {
    ISOChronology.checkNotNull(timeZoneIdentifier, "Time Zone ID must not be null")
    ISOChronology.checkNotNull(aliasMap, "Alias map must not be null")
    var zoneId: String = aliasMap.get(timeZoneIdentifier)
    zoneId = (if (zoneId != null) zoneId else timeZoneIdentifier)
    of(zoneId)
  }

  /**
   * Obtains an instance of     { @code TimeZone } representing a fixed time-zone.
   * <p>
   * The time-zone returned from this factory has a fixed offset for all time.
   * The region ID will return an identifier formed from 'UTC' and the offset.
   * The group and version IDs will both return an empty string.
   * <p>
   * Fixed time-zones are     { @link # isValid ( ) always valid }.
   *
   * @param offset the zone offset to create a fixed zone for, not null
   * @return the time-zone for the offset, never null
   */
  def of(offset: ZoneOffset): TimeZone = {
    ISOChronology.checkNotNull(offset, "ZoneOffset must not be null")
    if (offset == ZoneOffset.UTC) UTC
    else new TimeZone.Fixed(offset)
  }

  /**
   * The group:region#version ID pattern.
   */
  private val PATTERN: Pattern = Pattern.compile("(([A-Za-z0-9._-]+)[:])?([A-Za-z0-9%@~/+._-]+)([#]([A-Za-z0-9._-]+))?")

  /**
   * A map of zone overrides to enable the older US time-zone names to be used.
   * <p>
   * This maps as follows:
   * <ul>
   * <li>EST - America/Indianapolis</li>
   * <li>MST - America/Phoenix</li>
   * <li>HST - Pacific/Honolulu</li>
   * <li>ACT - Australia/Darwin</li>
   * <li>AET - Australia/Sydney</li>
   * <li>AGT - America/Argentina/Buenos_Aires</li>
   * <li>ART - Africa/Cairo</li>
   * <li>AST - America/Anchorage</li>
   * <li>BET - America/Sao_Paulo</li>
   * <li>BST - Asia/Dhaka</li>
   * <li>CAT - Africa/Harare</li>
   * <li>CNT - America/St_Johns</li>
   * <li>CST - America/Chicago</li>
   * <li>CTT - Asia/Shanghai</li>
   * <li>EAT - Africa/Addis_Ababa</li>
   * <li>ECT - Europe/Paris</li>
   * <li>IET - America/Indiana/Indianapolis</li>
   * <li>IST - Asia/Kolkata</li>
   * <li>JST - Asia/Tokyo</li>
   * <li>MIT - Pacific/Apia</li>
   * <li>NET - Asia/Yerevan</li>
   * <li>NST - Pacific/Auckland</li>
   * <li>PLT - Asia/Karachi</li>
   * <li>PNT - America/Phoenix</li>
   * <li>PRT - America/Puerto_Rico</li>
   * <li>PST - America/Los_Angeles</li>
   * <li>SST - Pacific/Guadalcanal</li>
   * <li>VST - Asia/Ho_Chi_Minh</li>
   * </ul>
   * The map is unmodifiable.
   */
  var OLD_IDS_PRE_2005: Map[String, String] = null //FIXME: Should be val

  /**
   * A map of zone overrides to enable the older US time-zone names to be used.
   * <p>
   * This maps as follows:
   * <ul>
   * <li>EST - UTC-05:00</li>
   * <li>HST - UTC-10:00</li>
   * <li>MST - UTC-07:00</li>
   * <li>ACT - Australia/Darwin</li>
   * <li>AET - Australia/Sydney</li>
   * <li>AGT - America/Argentina/Buenos_Aires</li>
   * <li>ART - Africa/Cairo</li>
   * <li>AST - America/Anchorage</li>
   * <li>BET - America/Sao_Paulo</li>
   * <li>BST - Asia/Dhaka</li>
   * <li>CAT - Africa/Harare</li>
   * <li>CNT - America/St_Johns</li>
   * <li>CST - America/Chicago</li>
   * <li>CTT - Asia/Shanghai</li>
   * <li>EAT - Africa/Addis_Ababa</li>
   * <li>ECT - Europe/Paris</li>
   * <li>IET - America/Indiana/Indianapolis</li>
   * <li>IST - Asia/Kolkata</li>
   * <li>JST - Asia/Tokyo</li>
   * <li>MIT - Pacific/Apia</li>
   * <li>NET - Asia/Yerevan</li>
   * <li>NST - Pacific/Auckland</li>
   * <li>PLT - Asia/Karachi</li>
   * <li>PNT - America/Phoenix</li>
   * <li>PRT - America/Puerto_Rico</li>
   * <li>PST - America/Los_Angeles</li>
   * <li>SST - Pacific/Guadalcanal</li>
   * <li>VST - Asia/Ho_Chi_Minh</li>
   * </ul>
   * The map is unmodifiable.
   */
  var OLD_IDS_POST_2005: Map[String, String] = null //FIXME: should be val

  //FIXME!
  var base: Map[String, String] = new HashMap[String, String]
  base.put("ACT", "Australia/Darwin")
  base.put("AET", "Australia/Sydney")
  base.put("AGT", "America/Argentina/Buenos_Aires")
  base.put("ART", "Africa/Cairo")
  base.put("AST", "America/Anchorage")
  base.put("BET", "America/Sao_Paulo")
  base.put("BST", "Asia/Dhaka")
  base.put("CAT", "Africa/Harare")
  base.put("CNT", "America/St_Johns")
  base.put("CST", "America/Chicago")
  base.put("CTT", "Asia/Shanghai")
  base.put("EAT", "Africa/Addis_Ababa")
  base.put("ECT", "Europe/Paris")
  base.put("IET", "America/Indiana/Indianapolis")
  base.put("IST", "Asia/Kolkata")
  base.put("JST", "Asia/Tokyo")
  base.put("MIT", "Pacific/Apia")
  base.put("NET", "Asia/Yerevan")
  base.put("NST", "Pacific/Auckland")
  base.put("PLT", "Asia/Karachi")
  base.put("PNT", "America/Phoenix")
  base.put("PRT", "America/Puerto_Rico")
  base.put("PST", "America/Los_Angeles")
  base.put("SST", "Pacific/Guadalcanal")
  base.put("VST", "Asia/Ho_Chi_Minh")
  var pre: Map[String, String] = new HashMap[String, String](base)
  pre.put("EST", "America/Indianapolis")
  pre.put("MST", "America/Phoenix")
  pre.put("HST", "Pacific/Honolulu")
  OLD_IDS_PRE_2005 = Collections.unmodifiableMap(pre)
  var post: Map[String, String] = new HashMap[String, String](base)
  post.put("EST", "UTC-05:00")
  post.put("MST", "UTC-07:00")
  post.put("HST", "UTC-10:00")
  OLD_IDS_POST_2005 = Collections.unmodifiableMap(post)

  /**
   * The time-zone offset for UTC, with an id of 'UTC'.
   */
  val UTC: TimeZone = new TimeZone.Fixed(ZoneOffset.UTC)

  /**
   * Fixed time-zone.
   *
   * Constructor.
   *
   * @param offset the offset, not null
   */
  @SerialVersionUID(1L)
  private[calendar] final class Fixed private[calendar](offset: ZoneOffset) extends TimeZone {

    def getRulesValidFor(dateTime: OffsetDateTime): ZoneRules = {
      ISOChronology.checkNotNull(dateTime, "OffsetDateTime must not be null")
      if (rules.getOffset(dateTime).equals(dateTime.getOffset) == false) {
        throw new CalendricalException("Fixed time-zone " + getID + " is invalid for date-time " + dateTime)
      }
      return rules
    }

    def getRegionID: String = return id

    def isValid: Boolean = true

    /**The zone id. */
    private lazy val id: String = rules.toString

    def getVersionID: String = return ""

    def withLatestVersionValidFor(dateTime: OffsetDateTime): TimeZone = {
      ISOChronology.checkNotNull(dateTime, "OffsetDateTime must not be null")
      if (getRules.getOffset(dateTime).equals(dateTime.getOffset) == false) {
        throw new CalendricalException("Fixed time-zone " + getID + " is invalid for date-time " + dateTime)
      }
      return this
    }

    def isLatestVersion: Boolean = true

    def isFixed: Boolean = true

    def withFloatingVersion: TimeZone = this

    def isFloatingVersion: Boolean = true

    def isValidFor(dateTime: OffsetDateTime): Boolean = {
      if (dateTime == null) false
      else rules.getOffset(dateTime).equals(dateTime.getOffset)
    }

    def getGroupID: String = return ""

    def withLatestVersion: TimeZone = this

    def getRules: ZoneRules = rules

    /**The zone rules. */
    @transient
    private lazy val rules: ZoneRules = ZoneRules.ofFixed(offset)

    def withVersion(versionID: String): TimeZone = {
      ISOChronology.checkNotNull(versionID, "Version ID must not be null")
      if (versionID.length > 0) {
        throw new CalendricalException("Fixed time-zone does not provide versions")
      }
      return this
    }

    def getID: String = id

    /**
     * Handle deserialization.
     *
     * @return the resolved instance, never null
     */
    private def readResolve: AnyRef = {
      if (id == null || id.startsWith("UTC") == false) throw new StreamCorruptedException
      else TimeZone.of(id)
    }

    def getGroup: ZoneRulesGroup = throw new CalendricalException("Fixed time-zone is not provided by a group")
  }

  /**
   * Gets the field rule for     { @code DateTimeZone }.
   *
   * @return the field rule for the time-zone, never null
   */
  def rule: CalendricalRule[TimeZone] = Rule

  /**
   * ID based time-zone.
   * This can refer to an id that does not have available rules.
   *
   * Constructor.
   *
   * @param groupID the time-zone rules group ID, not null
   * @param regionID the time-zone region ID, not null
   * @param versionID the time-zone rules version ID, not null
   */
  @SerialVersionUID(1L)
  private[calendar] final class ID private[calendar](groupID: String, regionID: String, versionID: String) extends TimeZone {

    def withFloatingVersion: TimeZone = {
      if (isFloatingVersion) this
      else new TimeZone.ID(groupID, regionID, "")
    }

    def getGroupID: String = groupID

    def getRegionID: String = regionID


    /**
     * Validate deserialization.
     *
     * @param in the input stream
     */
    private def readObject(in: ObjectInputStream): Unit = {
      in.defaultReadObject
      if (groupID == null || groupID.length == 0 || regionID == null || versionID == null) {
        throw new StreamCorruptedException
      }
    }

    def isLatestVersion: Boolean = isFloatingVersion || versionID.equals(getGroup.getLatestVersionID(regionID))

    def getGroup: ZoneRulesGroup = ZoneRulesGroup.getGroup(groupID)

    def isFixed: Boolean = false

    def isValid: Boolean = {
      if (isFloatingVersion) ZoneRulesGroup.isValidGroupID(groupID) && getGroup.isValidRegionID(regionID)
      else ZoneRulesGroup.isValidGroupID(groupID) && getGroup.isValidRules(regionID, versionID)
    }

    def isFloatingVersion: Boolean = versionID.length == 0

    def getRulesValidFor(dateTime: OffsetDateTime): ZoneRules = {
      ISOChronology.checkNotNull(dateTime, "OffsetDateTime must not be null")
      var group: ZoneRulesGroup = getGroup
      if (isFloatingVersion) group.getRules(regionID, group.getLatestVersionIDValidFor(regionID, dateTime))
      else group.getRulesValidFor(regionID, versionID, dateTime)
    }

    def getID: String = {
      if (groupID.equals("TZDB")) regionID + (if (versionID.length == 0) "" else '#' + versionID)
      else groupID + ':' + regionID + (if (versionID.length == 0) "" else '#' + versionID)
    }

    def getRules: ZoneRules = {
      var group: ZoneRulesGroup = getGroup
      if (isFloatingVersion) group.getRules(regionID, group.getLatestVersionID(regionID))
      else group.getRules(regionID, versionID)
    }

    def getVersionID: String = versionID

    def withLatestVersionValidFor(dateTime: OffsetDateTime): TimeZone = {
      ISOChronology.checkNotNull(dateTime, "OffsetDateTime must not be null")
      return withVersion(getGroup.getLatestVersionIDValidFor(regionID, dateTime))
    }

    def withVersion(versionID: String): TimeZone = {
      ISOChronology.checkNotNull(versionID, "Version ID must not be null")
      if (versionID.length == 0) {
        return withFloatingVersion
      }
      if (getGroup.isValidRules(regionID, versionID) == false) {
        throw new CalendricalException("Unknown version: " + groupID + ":" + regionID + '#' + versionID)
      }
      if (versionID.equals(this.versionID)) {
        return this
      }
      return new TimeZone.ID(groupID, regionID, versionID)
    }

    def isValidFor(dateTime: OffsetDateTime): Boolean = {
      if (dateTime == null) {
        return false
      }
      try {
        getRulesValidFor(dateTime)
        return true
      }
      catch {
        case ex: CalendricalException => {
          return false
        }
      }
    }

    def withLatestVersion: TimeZone = {
      val versionID: String = getGroup.getLatestVersionID(regionID)
      if (versionID.equals(this.versionID)) this
      else new TimeZone.ID(groupID, regionID, versionID)
    }
  }

  /**
   * Rule implementation.
   */
  private[calendar] object Rule extends Rule

  @SerialVersionUID(1L)
  private[calendar] sealed class Rule
    extends CalendricalRule[TimeZone](classOf[TimeZone], ISOChronology, "TimeZone", null, null)
    with Serializable {

    protected override def derive(calendrical: Calendrical): Option[TimeZone] = {
      val zdt: ZonedDateTime = calendrical.get(ZonedDateTime.rule)
      if (zdt != null) Some(zdt.getZone) else None
    }

    private def readResolve: AnyRef = Rule
  }

  /**
   * Obtains an instance of     { @code TimeZone } from an identifier.
   *
   * @param zoneID the time-zone identifier, not null
   * @param checkAvailable whether to check if the time-zone ID is available
   * @return the time-zone, never null
   * @throws CalendricalException if the time-zone cannot be found
   */
  private def ofID(zoneID: String, checkAvailable: Boolean): TimeZone = {
    ISOChronology.checkNotNull(zoneID, "Time zone ID must not be null")
    if (zoneID.equals("UTC") || zoneID.equals("GMT")) {
      return UTC
    }
    if ((zoneID.startsWith("UTC") || zoneID.startsWith("GMT")) && zoneID.indexOf('#') < 0) {
      try {
        return of(ZoneOffset.of(zoneID.substring(3)))
      }
      catch {
        case ex: IllegalArgumentException => {
        }
      }
    }
    var matcher: Matcher = PATTERN.matcher(zoneID)
    if (matcher.matches == false) {
      throw new CalendricalException("Invalid time-zone ID: " + zoneID)
    }
    var groupID: String = matcher.group(2)
    var regionID: String = matcher.group(3)
    var versionID: String = matcher.group(5)
    groupID = (if (groupID != null) groupID else "TZDB")
    versionID = (if (versionID != null) versionID else "")
    if (checkAvailable) {
      var group: ZoneRulesGroup = ZoneRulesGroup.getGroup(groupID)
      if (versionID.length == 0) {
        if (group.isValidRegionID(regionID) == false) {
          throw new CalendricalException("Unknown time-zone region: " + groupID + ':' + regionID)
        }
      }
      else {
        if (group.isValidRules(regionID, versionID) == false) {
          throw new CalendricalException("Unknown time-zone region or version: " + groupID + ':' + regionID + '#' + versionID)
        }
      }
    }
    return new TimeZone.ID(groupID, regionID, versionID)
  }

  /**
   * Obtains an instance of     { @code TimeZone } from an identifier ensuring that the
   * identifier is valid and available for use.
   * <p>
   * Six forms of identifier are recognized:
   * <ul>
   * <li>    { @code    { groupID } :    { regionID } #    { versionID } } - full
   * <li>    { @code    { groupID } :    { regionID } } - implies the floating version
   * <li>    { @code    { regionID } #    { versionID } } - implies 'TZDB' group and specific version
   * <li>    { @code    { regionID } } - implies 'TZDB' group and the floating version
   * <li>    { @code UTC    { offset } } - fixed time-zone
   * <li>    { @code GMT    { offset } } - fixed time-zone
   * </ul>
   * Group IDs must match regular expression     { @code[A -Za-z0-9._-]+ }.<br />
   * Region IDs must match regular expression     { @code[A -Za-z0-9%@~/+._-]+ }.<br />
   * Version IDs must match regular expression     { @code[A -Za-z0-9._-]+ }.
   * <p>
   * Most of the formats are based around the group, version and region IDs.
   * The version and region ID formats are specific to the group.
   * <p>
   * The default group is 'TZDB' which has versions of the form     { year } { letter }, such as '2009b'.
   * The region ID for the 'TZDB' group is generally of the form '    { area } /    { city } ', such as 'Europe/Paris'.
   * This is compatible with most IDs from     { @link java.util.TimeZone }.
   * <p>
   * For example, if a provider is loaded with the ID 'MyProvider' containing a zone ID of
   * 'France', then the unique key for version 2.1 would be 'MyProvider:France#2.1'.
   * A specific version of the TZDB provider is 'TZDB:Asia/Tokyo#2008g'.
   * <p>
   * Once parsed, this factory will ensure that the group, region and version combination is valid
   * and rules can be obtained.
   * <p>
   * The alternate format is for fixed time-zones, where the offset never changes over time.
   * A fixed time-zone is returned if the first three characters are 'UTC' or 'GMT' and
   * the remainder of the ID is a valid format for     { @link ZoneOffset # of ( String ) }.
   * The result will have a normalized time-zone ID of 'UTC    { offset } ', or just 'UTC' if the offset is zero.
   * <p>
   * Note that it is intended that fixed offset time-zones are rarely used. Applications should use
   * { @link ZoneOffset } and     { @link OffsetDateTime } in preference.
   *
   * @param zoneID the time-zone identifier, not null
   * @return the time-zone, never null
   * @throws CalendricalException if the time-zone cannot be found
   */
  def of(zoneID: String): TimeZone = ofID(zoneID, true)
}

/**
 * Constructor only accessible within the package.
 */
@SerialVersionUID(1L)
abstract class TimeZone private[calendar] extends Calendrical with Serializable {
  /**
   * Gets the unique time-zone ID.
   * <p>
   * The unique key is created from the group ID, version ID and region ID.
   * The format is     { groupID } :    { regionID } #    { versionID }.
   * If the group is 'TZDB' then the     { groupID } : is omitted.
   * If the version is floating, then the #    { versionID } is omitted.
   * Fixed time-zones will only output the region ID.
   *
   * @return the time-zone unique ID, never null
   */
  def getID: String

  /**
   * Is this instance equal to that specified by comparing the ID.
   *
   * @param otherZone the other zone, null returns false
   * @return true if this zone is the same as that specified
   */
  override def equals(otherZone: AnyRef): Boolean = {
    if (this == otherZone) true
    else if (otherZone.isInstanceOf[TimeZone]) {
      val zone: TimeZone = otherZone.asInstanceOf[TimeZone]
      getRegionID.equals(zone.getRegionID) && getVersionID.equals(zone.getVersionID) && getGroupID.equals(zone.getGroupID)
    }
    else false
  }

  /**
   * Finds the zone rules group for the stored group ID, such as 'TZDB'.
   * <p>
   * Time zone rules are provided by groups referenced by an ID.
   * <p>
   * Fixed time-zones are not provided by a group, thus this method throws
   * an exception if the time-zone is fixed.
   * <p>
   * Callers of this method need to be aware of an unusual scenario.
   * It is possible to obtain a     { @code TimeZone } instance even when the
   * rules are not available. This typically occurs when a     { @code TimeZone }
   * is loaded from a previously stored version but the rules are not available.
   * In this case, the     { @code TimeZone } instance is still valid, as is
   * any associated object, such as     { @link ZonedDateTime }. It is impossible to
   * perform any calculations that require the rules however, and this method
   * will throw an exception.
   *
   * @return the time-zone rules group ID, never null
   * @throws CalendricalException if the time-zone is fixed
   * @throws CalendricalException if the group ID cannot be found
   */
  def getGroup: ZoneRulesGroup

  /**
   * Returns a copy of this time-zone with the specified version ID.
   * <p>
   * For group based time-zones, this returns a     { @code TimeZone } with the
   * same group and region, but a floating version.
   * The group and region IDs are not validated.
   * <p>
   * For fixed time-zones,     { @code this } is returned.
   *
   * @return the new updated time-zone, never null
   * @throws CalendricalException if the time-zone is fixed
   */
  def withFloatingVersion: TimeZone

  /**
   * Gets the time-zone rules group ID, such as 'TZDB'.
   * <p>
   * Time zone rules are provided by groups referenced by an ID.
   * <p>
   * For fixed time-zones, the group ID will be an empty string.
   *
   * @return the time-zone rules group ID, never null
   */
  def getGroupID: String

  /**
   * Checks of the time-zone is fixed, such that the offset never varies.
   * <p>
   * It is intended that     { @link OffsetDateTime },     { @link OffsetDate } and
   * { @link OffsetTime } are used in preference to fixed offset time-zones
   * in     { @link ZonedDateTime }.
   *
   * @return true if the time-zone is fixed and the offset never changes
   */
  def isFixed: Boolean

  /**
   * Checks if the version is the latest version.
   * <p>
   * For floating group based time-zones, true is returned.
   * <p>
   * For non-floating group based time-zones, this returns true if the version
   * stored is the same as the latest version available for the group and region.
   * The group and region IDs are validated in order to calculate the latest version.
   * <p>
   * For fixed time-zones, true is returned.
   *
   * @return true if the version is the latest available
   * @throws CalendricalException if the version is non-floating and the group or region ID is not found
   */
  def isLatestVersion: Boolean

  /**
   * Gets the textual name of this zone.
   *
   * @return the time-zone name, never null
   */
  def getName: String = getRegionID

  /**
   * A hash code for this time-zone ID.
   *
   * @return a suitable hash code
   */
  override def hashCode: Int = getGroupID.hashCode ^ getRegionID.hashCode ^ getVersionID.hashCode

  /**
   * Checks if the version is floating.
   * <p>
   * A floating version will track the latest available version of the rules.
   * <p>
   * For group based time-zones, this returns true if the version ID is empty,
   * which is the definition of a floating zone.
   * <p>
   * For fixed time-zones, true is returned.
   *
   * @return true if the version is floating
   */
  def isFloatingVersion: Boolean

  /**
   * Checks if this time-zone is valid such that rules can be obtained for it.
   * <p>
   * This will return true if the rules are available for the group, region
   * and version ID combination. If this method returns true, then
   * { @link # getRules ( ) } will return a valid rules instance.
   * <p>
   * A time-zone can be invalid if it is deserialized in a JVM which does not
   * have the same rules loaded as the JVM that stored it.
   * <p>
   * If this object declares a floating version of the rules and a background
   * thread is used to update the available rules, then the result of calling
   * this method may vary over time.
   * Each individual call will be still remain thread-safe.
   * <p>
   * If this is a fixed time-zone, then it is always valid.
   *
   * @return true if this time-zone is valid and rules are available
   */
  def isValid: Boolean

  /**
   * Gets the time-zone rules allowing calculations to be performed, ensuring that
   * the date-time and offset specified is valid for the returned rules.
   * <p>
   * The rules provide the functionality associated with a time-zone,
   * such as finding the offset for a given instant or local date-time.
   * Different rules may be returned depending on the group, version and zone.
   * <p>
   * If this object declares a specific version of the rules, then the result will
   * be of that version providing that the specified date-time is valid for those rules.
   * If this object declares a floating version of the rules, then the latest
   * version of the rules where the date-time is valid will be returned.
   * <p>
   * A time-zone can be invalid if it is deserialized in a JVM which does not
   * have the same rules loaded as the JVM that stored it. In this case, calling
   * this method will throw an exception.
   * <p>
   * If this object declares a floating version of the rules and a background
   * thread is used to update the available rules, then the result of calling
   * this method may vary over time.
   * Each individual call will be still remain thread-safe.
   *
   * @param dateTime a date-time for which the rules must be valid, not null
   * @return the latest rules for this zone where the date-time is valid, never null
   * @throws CalendricalException if the zone ID cannot be found
   * @throws CalendricalException if no rules match the zone ID and date-time
   */
  def getRulesValidFor(dateTime: OffsetDateTime): ZoneRules

  /**
   * Gets the value of the specified calendrical rule.
   * <p>
   * This method queries the value of the specified calendrical rule.
   * If the value cannot be returned for the rule from this offset then
   * { @code null } will be returned.
   *
   * @param rule the rule to use, not null
   * @return the value for the rule, null if the value cannot be returned
   */
  def get[T](rule: CalendricalRule[T]): Option[T] = {
    if (rule.equals(ZoneOffset.rule) && isFixed) Some(rule.reify(getRules.getOffset(Instant.Epoch)))
    else Some(rule.deriveValueFor(rule, this, this))
  }

  /**
   * Returns a copy of this time-zone with the latest available version ID.
   * <p>
   * For floating and non-floating group based time-zones, this returns a zone with the same
   * group and region, but the latest version that has been registered.
   * The group and region IDs are validated in order to calculate the latest version.
   * <p>
   * For fixed time-zones,     { @code this } is returned.
   *
   * @return the new updated time-zone, never null
   * @throws CalendricalException if the version is non-floating and the group or region ID is not found
   */
  def withLatestVersion: TimeZone

  /**
   * Gets the time-zone region identifier, such as 'Europe/London'.
   * <p>
   * The time-zone region identifier is of a format specific to the group.
   * The default 'TZDB' group generally uses the format     { area } /    { city }, such as 'Europe/Paris'.
   *
   * @return the time-zone rules region ID, never null
   */
  def getRegionID: String

  /**
   * Returns a copy of this time-zone with the latest version that is valid
   * for the specified date-time and offset.
   * <p>
   * This method validates the group and region IDs.
   *
   * @param dateTime the date-time to get the latest version for
   * @return the new updated time-zone, never null
   * @throws CalendricalException if the group or region ID is not found
   * @throws CalendricalException if there are no valid rules for the date-time
   */
  def withLatestVersionValidFor(dateTime: OffsetDateTime): TimeZone

  /**
   * Returns a copy of this time-zone with the specified version ID.
   * <p>
   * For group based time-zones, this returns a     { @code TimeZone }
   * with the same group and region, but the specified version.
   * The group and region IDs are validated to ensure that the version is valid.
   * <p>
   * For fixed time-zones, the version must be an empty string, otherwise an
   * exception is thrown.
   *
   * @param versionID the version ID to use, empty means floating version, not null
   * @return the new updated time-zone, never null
   * @throws CalendricalException if the time-zone is fixed and the version is not empty
   * @throws CalendricalException if the group, region or version ID is not found
   */
  def withVersion(versionID: String): TimeZone

  /**
   * Gets the time-zone rules allowing calculations to be performed.
   * <p>
   * The rules provide the functionality associated with a time-zone,
   * such as finding the offset for a given instant or local date-time.
   * Different rules may be returned depending on the group, version and zone.
   * <p>
   * If this object declares a specific version of the rules, then the result will
   * be of that version. If this object declares a floating version of the rules,
   * then the latest version available will be returned.
   * <p>
   * A time-zone can be invalid if it is deserialized in a JVM which does not
   * have the same rules loaded as the JVM that stored it. In this case, calling
   * this method will throw an exception.
   * <p>
   * If this object declares a floating version of the rules and a background
   * thread is used to update the available rules, then the result of calling
   * this method may vary over time.
   * Each individual call will be still remain thread-safe.
   *
   * @return the rules, never null
   * @throws CalendricalException if the group, region or version ID cannot be found
   */
  def getRules: ZoneRules

  /**
   * Gets the short textual name of this zone.
   *
   * @return the time-zone short name, never null
   */
  def getShortName: String = getRegionID

  /**
   * Checks if this time-zone is valid such that rules can be obtained for it
   * which are valid for the specified date-time and offset.
   * <p>
   * This will return true if the rules are available for the group, region
   * and version ID combination that are valid for the specified date-time.
   * If this method returns true, then     { @link # getRulesValidFor ( OffsetDateTime ) }
   * will return a valid rules instance.
   * <p>
   * A time-zone can be invalid if it is deserialized in a JVM which does not
   * have the same rules loaded as the JVM that stored it.
   * <p>
   * If this object declares a floating version of the rules and a background
   * thread is used to update the available rules, then the result of calling
   * this method may vary over time.
   * Each individual call will be still remain thread-safe.
   * <p>
   * If this is a fixed time-zone, then it is valid if the offset matches the date-time.
   *
   * @param dateTime a date-time for which the rules must be valid, null returns false
   * @return true if this time-zone is valid and rules are available
   */
  def isValidFor(dateTime: OffsetDateTime): Boolean

  /**
   * Gets the time-zone rules group version, such as '2009b'.
   * <p>
   * Time zone rules change over time as governments change the associated laws.
   * The time-zone groups capture these changes by issuing multiple versions
   * of the data. An application can reference the exact set of rules used
   * by using the group ID and version. Once loaded, there is no way to unload
   * a version of the rules, however new versions may be added.
   * <p>
   * The version can be an empty string which represents the floating version.
   * This always uses the latest version of the rules available.
   * <p>
   * For fixed time-zones, the version ID will be an empty string.
   *
   * @return the time-zone rules version ID, empty if the version is floating, never null
   */
  def getVersionID: String

  /**
   * Returns a string representation of the time-zone.
   * <p>
   * This returns     { @link # getID ( ) }.
   *
   * @return the time-zone ID, never null
   */
  override def toString: String = getID
}