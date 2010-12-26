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
package javax.time.calendar.zone

import java.io.ByteArrayInputStream
import java.io.DataInputStream
import java.io.IOException
import java.io.InputStream
import java.io.StreamCorruptedException
import java.net.URL
import java.util.Arrays
import java.util.Enumeration
import java.util.concurrent.atomic.AtomicReferenceArray


import collection.mutable.ArrayBuffer
import collection.immutable.HashSet
import collection.mutable.{HashSet => MutableHashSet}

import javax.time.CalendricalException

/**
 * Loads time-zone rules stored in a file accessed via class loader.
 * <p>
 * ResourceZoneRulesDataProvider is thread-safe and immutable.
 *
 * @author Stephen Colebourne
 */

object ResourceZoneRulesDataProvider {

  /**
   * Version of the rules.
   * <p>
   * ResourceZoneRulesVersion is thread-safe and immutable.
   */
  /**Constructor. */
  private[zone] class ResourceZoneRulesVersion(val provider: ResourceZoneRulesDataProvider,
                                               val versionID: String,
                                               val regionArray: Array[String],
                                               val ruleIndices: Array[Short])
    extends ZoneRulesVersion {
    def getZoneRules(regionID: String): ZoneRules = {
      val index: Int = Arrays.binarySearch(regionArray.asInstanceOf[Array[Object]], regionID)
      if (index < 0) {
        return null
      }
      try {
        return provider.loadRule(ruleIndices(index))
      }
      catch {
        case ex: Exception => {
          throw new CalendricalException("Unable to load rules: " + provider.groupID + ':' + regionID + '#' + versionID, ex)
        }
      }
    }

    def isRegionID(regionID: String): Boolean = Arrays.binarySearch(regionArray.asInstanceOf[Array[AnyRef]], regionID) >= 0

    def getRegionIDs: Set[String] = HashSet[String](regionArray: _*)

    def getVersionID: String = versionID

    override def toString: String = versionID

  }

  /**
   * Loads any time-zone rules data stored in files.
   *
   * @throws RuntimeException if the time-zone rules cannot be loaded
   */
  private[zone] def load: Unit = {
    for (provider <- loadResources) {
      ZoneRulesGroup.registerProvider(provider)
    }
  }

  /**
   * Loads the rules from files in the class loader, often jar files.
   *
   * @return the list of loaded rules, never null
   * @throws Exception if an error occurs
   */
  private def loadResources: Seq[ResourceZoneRulesDataProvider] = {
    var providers: ArrayBuffer[ResourceZoneRulesDataProvider] = new ArrayBuffer[ResourceZoneRulesDataProvider]
    var url: URL = null
    try {
      val en: Enumeration[URL] = Thread.currentThread.getContextClassLoader.getResources("javax/time/calendar/zone/ZoneRules.dat")
      var loaded: MutableHashSet[String] = new MutableHashSet[String]
      while (en.hasMoreElements) {
        url = en.nextElement
        if (loaded.add(url.toExternalForm)) {
          providers += (new ResourceZoneRulesDataProvider(url))
        }
      }
    }
    catch {
      case ex: Exception => {
        throw new RuntimeException("Unable to load time-zone rule data: " + url, ex)
      }
    }
    return providers
  }
}

/**
 * Loads the rules from a URL, often in a jar file.
 *
 * @param providers the list to add to, not null
 * @param url the jar file to load, not null
 * @throws Exception if an error occurs
 */
final class ResourceZoneRulesDataProvider private(url: URL) extends ZoneRulesDataProvider {
  var throwing: Boolean = false
  var in: InputStream = null
  try {
    in = url.openStream
    var dis: DataInputStream = new DataInputStream(in)
    if (dis.readByte != 1) {
      throw new StreamCorruptedException("File format not recognised")
    }
    this.groupID = dis.readUTF
    var versionCount: Int = dis.readShort
    var versionArray: Array[String] = new Array[String](versionCount)

    {
      var i: Int = 0
      while (i < versionCount) {
        versionArray(i) = dis.readUTF
        i += 1;
      }
    }
    var regionCount: Int = dis.readShort
    var regionArray: Array[String] = new Array[String](regionCount)

    {
      var i: Int = 0
      while (i < regionCount) {
        regionArray(i) = dis.readUTF
        i += 1;
      }
    }
    this.regions = HashSet[String](regionArray: _*)
    var versionSet: MutableHashSet[ZoneRulesVersion] = new MutableHashSet[ZoneRulesVersion] /*(versionCount)*/ {
      var i: Int = 0
      while (i < versionCount) {
        var versionRegionCount: Int = dis.readShort
        var versionRegionArray: Array[String] = new Array[String](versionRegionCount)
        var versionRulesArray: Array[Short] = new Array[Short](versionRegionCount)

        {
          var j: Int = 0
          while (j < versionRegionCount) {
            versionRegionArray(j) = regionArray(dis.readShort)
            versionRulesArray(j) = dis.readShort
            j += 1;
          }
        }
        versionSet.add(new ResourceZoneRulesDataProvider.ResourceZoneRulesVersion(this, versionArray(i), versionRegionArray, versionRulesArray))
        i += 1;
      }
    }
    this.versions = versionSet
    var ruleCount: Int = dis.readShort
    this.rules = new AtomicReferenceArray[AnyRef](ruleCount)
    var i: Int = 0
    while (i < ruleCount) {
      var bytes: Array[Byte] = new Array[Byte](dis.readShort)
      dis.readFully(bytes)
      rules.set(i, bytes)
      i += 1;
    }
  }
  catch {
    case ex: IOException => {
      throwing = true
      throw ex
    }
  }
  finally {
    if (in != null) {
      try {
        in.close
      }
      catch {
        case ex: IOException => {
          if (throwing == false) {
            throw ex
          }
        }
      }
    }
  }
  /**
   * The rules.
   */
  private var rules: AtomicReferenceArray[AnyRef] = null
  //FIXME? val
  /**{@inheritDoc}*/
  def getVersions: Set[ZoneRulesVersion] = versions.toSet

  /**{@inheritDoc}*/
  def getGroupID: String = groupID

  /**{@inheritDoc}*/
  def getRegionIDs: Set[String] = regions

  override def toString: String = groupID + ":#" + versions


  /**
   * All the regions in the provider.
   */
  private var regions: Set[String] = null
  //FIXME? val

  /**
   * Loads the rule.
   * @param index the index to retrieve
   * @return the rules, should not be null
   */
  private[zone] def loadRule(index: Short): ZoneRules = {
    var obj: AnyRef = rules.get(index)
    if (obj.isInstanceOf[Array[Byte]]) {
      val bytes: Array[Byte] = obj.asInstanceOf[Array[Byte]]
      val dis: DataInputStream = new DataInputStream(new ByteArrayInputStream(bytes))
      obj = Ser.read(dis)
      rules.set(index, obj)
    }
    return obj.asInstanceOf[ZoneRules]
  }

  /**
   * The group ID.
   */
  private var groupID: String = null
  //FIXME? val
  /**
   * All the versions in the provider.
   */
  private var versions: MutableHashSet[ZoneRulesVersion] = null
  //FIXME? val
}