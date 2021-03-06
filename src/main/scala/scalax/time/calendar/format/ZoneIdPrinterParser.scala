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

import util.control.Breaks._
import collection.mutable.HashMap
import collection.immutable.HashSet

import scalax.time.calendar.Calendrical
import scalax.time.calendar.TimeZone
import scalax.time.calendar.ZoneOffset
import scalax.time.calendar.format.DateTimeFormatterBuilder.TextStyle
import scalax.time.calendar.zone.ZoneRulesGroup

/**
 * Prints or parses a zone offset.
 * <p>
 * ZoneIdPrinterParser is immutable and thread-safe.
 *
 * @author Stephen Colebourne
 */
object ZoneIdPrinterParser {
  /**
   * The cached IDs.
   */
  private var preparedIDs: Set[String] = null

  /**
   * Model a tree of substrings to make the parsing easier. Due to the nature
   * of time-zone names, it can be faster to parse based in unique substrings
   * rather than just a character by character match.
   * <p>
   * For example, to parse America/Denver we can look at the first two
   * character "Am". We then notice that the shortest time-zone that starts
   * with Am is America/Nome which is 12 characters long. Checking the first
   * 12 characters of America/Denver giver America/Denv which is a substring
   * of only 1 time-zone: America/Denver. Thus, with just 3 comparisons that
   * match can be found.
   * <p>
   * This structure maps substrings to substrings of a longer length. Each
   * node of the tree contains a length and a map of valid substrings to
   * sub-nodes. The parser gets the length from the root node. It then
   * extracts a substring of that length from the parseText. If the map
   * contains the substring, it is set as the possible time-zone and the
   * sub-node for that substring is retrieved. The process continues until the
   * substring is no longer found, at which point the matched text is checked
   * against the real time-zones.
   */

  /**
   * Constructor.
   *
   * @param length the length of this tree (The length of the substring this node of the tree contains.
   * Subtrees will have a longer length.)
   */
  private[format] class SubstringTree(val length: Int) {

    private[format] def get(substring2: String): ZoneIdPrinterParser.SubstringTree = substringMap.getOrElse(substring2, null)

    /**
     * Values must be added from shortest to longest.
     *
     * @param newSubstring the substring to add, not null
     */
    private[format] def add(newSubstring: String): Unit = {
      var idLen: Int = newSubstring.length
      if (idLen == length) {
        substringMap.put(newSubstring, null)
      }
      else if (idLen > length) {
        val substring: String = newSubstring.substring(0, length)
        var parserTree: ZoneIdPrinterParser.SubstringTree = substringMap.getOrElse(substring, null)
        if (parserTree == null) {
          parserTree = new ZoneIdPrinterParser.SubstringTree(idLen)
          substringMap.put(substring, parserTree)
        }
        parserTree.add(newSubstring)
      }
    }

    /**
     * Map of a substring to a set of substrings that contain the key.
     */
    private final val substringMap = new HashMap[String, ZoneIdPrinterParser.SubstringTree]
  }

  /**
   * Builds an optimized parsing tree.
   *
   * @param availableIDs the available IDs, not null, not empty
   * @return the tree, never null
   */
  private def prepareParser(availableIDs: Set[String]): ZoneIdPrinterParser.SubstringTree = {
    val ids: List[String] = availableIDs.toList

    //    def comparator(str1: String, str2: String): Int = if (str1.length == str2.length) str1.compareTo(str2) else str1.length - str2.length
    //    ids.sortWith(comparator)

    val tree: ZoneIdPrinterParser.SubstringTree = new ZoneIdPrinterParser.SubstringTree(ids.head.length)
    tree
  }

  /**
   * The cached tree to speed up parsing.
   */
  private var preparedTree: ZoneIdPrinterParser.SubstringTree = null
}

/**
 * Constructor.
 *
 * @param textStyle the test style to output, not null
 */
final class ZoneIdPrinterParser private[format](textStyle: DateTimeFormatterBuilder.TextStyle)
  extends DateTimePrinter with DateTimeParser {

  import ZoneIdPrinterParser._

  /**
   * Constructor.
   */
  private[format] def this() {
    this (null)
  }

  /**{@inheritDoc}*/
  override def print(calendrical: Calendrical, appendable: Appendable, symbols: DateTimeFormatSymbols): Unit = {
    val zone: TimeZone = calendrical.get(TimeZone.rule).getOrElse(throw new CalendricalPrintException("Unable to print TimeZone"))

    if (textStyle == null) {
      appendable.append(zone.id)
    }
    else if (textStyle == TextStyle.Full) {
      appendable.append(zone.name)
    }
    else {
      appendable.append(zone.shortName)
    }
  }

  /**{@inheritDoc}*/
  override def isPrintDataAvailable(calendrical: Calendrical): Boolean = (calendrical.get(TimeZone.rule) != null)

  /**
   * {@inheritDoc }
   * <p>
   * This implementation looks for the longest matching string.
   * For example, parsing Etc/GMT-2 will return Etc/GMC-2 rather than just
   * Etc/GMC although both are valid.
   * <p>
   * This implementation uses a tree to search for valid time-zone names in
   * the parseText. The top level node of the tree has a length equal to the
   * length of the shortest time-zone as well as the beginning characters of
   * all other time-zones.
   */
//  def parse(context: DateTimeParseContext, parseText: String, position: Int): Int = {
//    val length: Int = parseText.length
//    if (position > length) {
//      throw new IndexOutOfBoundsException
//    }
//    var ids: Set[String] = ZoneRulesGroup.parsableIDs
//    if (ids.size == 0) {
//      return ~position
//    }
//    var tree: ZoneIdPrinterParser.SubstringTree = null
//    classOf[ZoneIdPrinterParser].synchronized{
//      if (preparedTree == null || preparedIDs.size < ids.size) {
//        ids = HashSet[String]() ++ ids
//        preparedTree = prepareParser(ids)
//        preparedIDs = ids
//      }
//      tree = preparedTree
//    }
//    if (parseText.substring(position).startsWith("UTC")) {
//      val newContext: DateTimeParseContext = new DateTimeParseContext(context.getSymbols)
//      val startPos: Int = position + 3
//      val endPos: Int = new ZoneOffsetPrinterParser("", true, true).parse(newContext, parseText, startPos)
//      if (endPos < 0) {
//        context.setParsed(TimeZone.rule, TimeZone.UTC)
//        return startPos
//      }
//      val zone: TimeZone = TimeZone.of(newContext.getParsed(ZoneOffset.rule).asInstanceOf[ZoneOffset])
//      context.setParsed(TimeZone.rule, zone)
//      return endPos
//    }
//    var parsedZoneId: String = null
//    var count: Int = 0
//    breakable{
//      while (tree != null) {
//        val nodeLength: Int = tree.length
//        if (position + nodeLength > length) {
//          break
//        }
//        parsedZoneId = parseText.substring(position, position + nodeLength)
//        tree = tree.get(parsedZoneId)
//        count += 1;
//      }
//    }
//    if (parsedZoneId != null && preparedIDs.contains(parsedZoneId)) {
//      var zone: TimeZone = TimeZone.of(parsedZoneId)
//      var pos: Int = position + parsedZoneId.length
//      if (pos + 1 < length && parseText.charAt(pos) == '#') {
//        val versions: Set[String] = zone.group.availableVersionIDs
//        breakable{
//          for (version <- versions) {
//            if (parseText.regionMatches(pos + 1, version, 0, version.length)) {
//              zone = zone.withVersion(version)
//              pos += version.length + 1
//              break
//            }
//          }
//        }
//      }
//      context.setParsed(TimeZone.rule, zone)
//      return pos
//    }
//    else return ~position
//  }
  def parse(context: DateTimeParseContext, parseText: String, position: Int): Int = throw new Exception("Not implemented!") //FIXME

  /**{@inheritDoc}*/
  override def toString: String = if (textStyle == null) "TimeZone()" else "ZoneText(" + textStyle + ")"
}