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

import java.io.BufferedReader
import java.io.ByteArrayOutputStream
import java.io.DataOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.FileReader
import java.text.ParsePosition
import java.util.Arrays
import java.util.StringTokenizer
import java.util.jar.JarOutputStream
import java.util.zip.ZipEntry
import javax.time.calendar.DateAdjusters
import javax.time.calendar.DayOfWeek
import javax.time.calendar.ISOChronology
import javax.time.calendar.LocalDate
import javax.time.calendar.LocalDateTime
import javax.time.calendar.LocalTime
import javax.time.calendar.MonthOfYear
import javax.time.calendar.Period
import javax.time.calendar.Year
import javax.time.calendar.ZoneOffset
import javax.time.calendar.format.DateTimeFormatter
import javax.time.calendar.format.DateTimeParseContext
import javax.time.calendar.zone.ZoneRulesBuilder.TimeDefinition
import collection.mutable.{HashSet, HashMap, ArrayBuffer}
import collection.immutable.{TreeSet, TreeMap}

/**
 * A builder that can read the TZDB TimeZone files and build ZoneRules instances.
 * <p>
 * TZDBZoneRulesCompiler is thread-safe.
 *
 * @author Stephen Colebourne
 */
object TZDBZoneRulesCompiler {
  /**
   * Output usage text for the command line.
   */
  private def outputHelp: Unit = {
    System.out.println("Usage: TZDBZoneRulesCompiler <options> <tzdb source filenames>")
    System.out.println("where options include:")
    System.out.println("   -srcdir <directory>   Where to find source directories (required)")
    System.out.println("   -dstdir <directory>   Where to output generated files (default srcdir)")
    System.out.println("   -version <version>    Specify the version, such as 2009a (optional)")
    System.out.println("   -help                 Print this usage message")
    System.out.println("   -verbose              Output verbose information during compilation")
    System.out.println(" There must be one directory for each version in srcdir")
    System.out.println(" Each directory must have the name of the version, such as 2009a")
    System.out.println(" Each directory must contain the unpacked tzdb files, such as asia or europe")
    System.out.println(" Directories must match the regex [12][0-9][0-9][0-9][A-Za-z0-9._-]+")
    System.out.println(" There will be one jar file for each version and one combined jar in dstdir")
    System.out.println(" If the version is specified, only that version is processed")
  }

  /**
   * Time parser.
   */
  private val TimeParser: DateTimeFormatter = null
  /**
   * Outputs the file.
   */
  private def outputFile(dstFile: File, version: String, builtZones: TreeMap[String, ZoneRules]): Unit = {
    var loopAllBuiltZones = new TreeMap[String, TreeMap[String, ZoneRules]]
    loopAllBuiltZones = loopAllBuiltZones.updated(version, builtZones)
    val loopAllRegionIds: TreeSet[String] = TreeSet[String]() ++ builtZones.keySet
    val loopAllRules = HashSet[ZoneRules](builtZones.values.toSeq: _*)
    outputFile(dstFile, loopAllBuiltZones, loopAllRegionIds, loopAllRules)
  }

  /**
   * Outputs the file.
   */
  private def outputFile(dstFile: File, allBuiltZones: Map[String, TreeMap[String, ZoneRules]], allRegionIds: TreeSet[String], allRules: HashSet[ZoneRules]): Unit = {
    try {
      val jos: JarOutputStream = new JarOutputStream(new FileOutputStream(dstFile))
      jos.putNextEntry(new ZipEntry("javax/time/calendar/zone/ZoneRules.dat"))
      val out: DataOutputStream = new DataOutputStream(jos)
      out.writeByte(1)
      out.writeUTF("TZDB")
      val versionArray: Array[String] = allBuiltZones.keySet.toArray
      out.writeShort(versionArray.length)
      for (version <- versionArray) {
        out.writeUTF(version)
      }
      val regionArray: Array[String] = allRegionIds.toArray
      out.writeShort(regionArray.length)
      for (regionId <- regionArray) {
        out.writeUTF(regionId)
      }
      val rulesList = ArrayBuffer[ZoneRules](allRules.toSeq: _*)
      for (version <- allBuiltZones.keySet) {
        out.writeShort(allBuiltZones(version).size)
        for (entry <- allBuiltZones(version)) {
          val regionIndex: Int = Arrays.binarySearch(regionArray.asInstanceOf[Array[AnyRef]], entry._1)
          val rulesIndex: Int = rulesList.indexOf(entry._2)
          out.writeShort(regionIndex)
          out.writeShort(rulesIndex)
        }
      }
      out.writeShort(rulesList.size)
      val baos: ByteArrayOutputStream = new ByteArrayOutputStream(1024)
      for (rules <- rulesList) {
        baos.reset
        val dataos: DataOutputStream = new DataOutputStream(baos)
        Ser.write(rules, dataos)
        dataos.close
        val bytes: Array[Byte] = baos.toByteArray
        out.writeShort(bytes.length)
        out.write(bytes)
      }
      jos.closeEntry
      out.close
    }
    catch {
      case ex: Exception => {
        System.out.println("Failed: " + ex.toString)
        ex.printStackTrace
        System.exit(1)
      }
    }
  }

  /**
   * Reads a set of TZDB files and builds a single combined data file.
   *
   * @param args the arguments
   */
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      outputHelp
      return
    }
    var version: String = null
    var baseSrcDir: File = null
    var dstDir: File = null
    var verbose: Boolean = false
    var i: Int = 0

    i = 0
    while (i < args.length) {
      {
        val arg: String = args(i)
        if (arg.startsWith("-") == false) {
          //break //todo: break is not supported
        }
        if ("-srcdir".equals(arg)) {
          if (baseSrcDir == null && ({
            i += 1;
            i - 1
          }) < args.length) {
            baseSrcDir = new File(args(i))
            //continue //todo: continue is not supported
          }
        }
        else if ("-dstdir".equals(arg)) {
          if (dstDir == null && ({
            i += 1;
            i - 1
          }) < args.length) {
            dstDir = new File(args(i))
            //continue //todo: continue is not supported
          }
        }
        else if ("-version".equals(arg)) {
          if (version == null && ({
            i += 1;
            i - 1
          }) < args.length) {
            version = args(i)
            //continue //todo: continue is not supported
          }
        }
        else if ("-verbose".equals(arg)) {
          if (verbose == false) {
            verbose = true
            //continue //todo: continue is not supported
          }
        }
        else if ("-help".equals(arg) == false) {
          System.out.println("Unrecognised option: " + arg)
        }
        outputHelp
        return
      }
      i += 1;
    }
    if (baseSrcDir == null) {
      System.out.println("Source directory must be specified using -srcdir: " + baseSrcDir)
      return
    }
    if (baseSrcDir.isDirectory == false) {
      System.out.println("Source does not exist or is not a directory: " + baseSrcDir)
      return
    }
    dstDir = (if (dstDir != null) dstDir else baseSrcDir)
    var srcFileNames = ArrayBuffer(args.slice(i, args.length): _*)
    if (srcFileNames.isEmpty) {
      System.out.println("Source filenames not specified, using default set")
      System.out.println("(africa antarctica asia australasia backward etcetera europe northamerica southamerica)")
      srcFileNames = ArrayBuffer("africa", "antarctica", "asia", "australasia", "backward", "etcetera", "europe", "northamerica", "southamerica")
    }
    var srcDirs = new ArrayBuffer[File]
    if (version != null) {
      var srcDir: File = new File(baseSrcDir, version)
      if (srcDir.isDirectory == false) {
        System.out.println("Version does not represent a valid source directory : " + srcDir)
        return
      }
      srcDirs += srcDir
    }
    else {
      val dirs: Array[File] = baseSrcDir.listFiles
      for (dir <- dirs) {
        if (dir.isDirectory && dir.getName.matches("[12][0-9][0-9][0-9][A-Za-z0-9._-]+")) {
          srcDirs += dir
        }
      }
    }
    if (srcDirs.isEmpty) {
      System.out.println("Source directory contains no valid source folders: " + baseSrcDir)
      return
    }
    if (dstDir.exists == false && dstDir.mkdirs == false) {
      System.out.println("Destination directory could not be created: " + dstDir)
      return
    }
    if (dstDir.isDirectory == false) {
      System.out.println("Destination is not a directory: " + dstDir)
      return
    }
    process(srcDirs, srcFileNames, dstDir, verbose)
    System.exit(0)
  }

  /**
   * Process to create the jar files.
   */
  private def process(srcDirs: Seq[File], srcFileNames: Seq[String], dstDir: File, verbose: Boolean): Unit = {
    val deduplicateMap = new HashMap[Any, Any]
    var allBuiltZones = new TreeMap[String, TreeMap[String, ZoneRules]]
    var allRegionIds = new TreeSet[String]
    var allRules = new HashSet[ZoneRules]
    for (srcDir <- srcDirs) {
      var srcFiles = new ArrayBuffer[File]
      for (srcFileName <- srcFileNames) {
        var file: File = new File(srcDir, srcFileName)
        if (file.exists) {
          srcFiles += file
        }
      }
      if (!srcFiles.isEmpty) {
        val loopVersion: String = srcDir.getName
        val compiler: TZDBZoneRulesCompiler = new TZDBZoneRulesCompiler(loopVersion, srcFiles, verbose)
        compiler.setDeduplicateMap(deduplicateMap)
        try {
          val builtZones: TreeMap[String, ZoneRules] = compiler.compile
          val dstFile: File = new File(dstDir, "jsr-310-TZDB-" + loopVersion + ".jar")
          if (verbose) {
            System.out.println("Outputting file: " + dstFile)
          }
          outputFile(dstFile, loopVersion, builtZones)
          allBuiltZones = allBuiltZones.updated(loopVersion, builtZones)
          allRegionIds ++= builtZones.keySet
          allRules ++= builtZones.values
        }
        catch {
          case ex: Exception => {
            System.out.println("Failed: " + ex.toString)
            ex.printStackTrace
            System.exit(1)
          }
        }
      }
    }
    val dstFile: File = new File(dstDir, "jsr-310-TZDB-all.jar")
    if (verbose) {
      System.out.println("Outputting combined file: " + dstFile)
    }
    outputFile(dstFile, allBuiltZones, allRegionIds, allRules)
  }
}


/**
 * Constructor used if you want to invoke the compiler manually.
 *
 * @param version the version, such as 2009a, not null
 * @param sourceFiles the list of source files, not empty, not null
 * @param verbose whether to output verbose messages
 */
final class TZDBZoneRulesCompiler(version: String, sourceFiles: Seq[File], verbose: Boolean) {
  /**
   * Compile the rules file.
   * @return the map of region ID to rules, not null
   * @throws Exception if an error occurs
   */
  def compile: TreeMap[String, ZoneRules] = {
    printVerbose("Compiling TZDB version " + version)
    parseFiles
    buildZoneRules
    printVerbose("Compiled TZDB version " + version)
    return builtZones
  }

  private def parseMonth(_str: String): MonthOfYear = {
    var str = _str
    str = str.toLowerCase
    for (moy <- MonthOfYear.values) {
      if (matches(str, moy.name.toLowerCase)) {
        return moy
      }
    }
    throw new IllegalArgumentException("Unknown month: " + str)
  }

  private def parseYear(_str: String, defaultYear: Int): Int = {
    var str = _str
    str = str.toLowerCase
    if (matches(str, "minimum")) {
      return Year.MinYear
    }
    else if (matches(str, "maximum")) {
      return Year.MaxYear
    }
    else if (str.equals("only")) {
      return defaultYear
    }
    return str.toInt
  }

  private def parseOptional(str: String): String = if (str.equals("-")) null else str

  /**
   * Parses a Rule line.
   * @param st the tokenizer, not null
   * @param mdt the object to parse into, not null
   */
  private def parseMonthDayTime(st: StringTokenizer, mdt: TZDBZoneRulesCompiler#TZDBMonthDayTime): Unit = {
    mdt.month = parseMonth(st.nextToken)
    if (st.hasMoreTokens) {
      var dayRule: String = st.nextToken
      if (dayRule.startsWith("last")) {
        mdt.dayOfMonth = -1
        mdt.dayOfWeek = parseDayOfWeek(dayRule.substring(4))
        mdt.adjustForwards = false
      }
      else {
        var index: Int = dayRule.indexOf(">=")
        if (index > 0) {
          mdt.dayOfWeek = parseDayOfWeek(dayRule.substring(0, index))
          dayRule = dayRule.substring(index + 2)
        }
        else {
          index = dayRule.indexOf("<=")
          if (index > 0) {
            mdt.dayOfWeek = parseDayOfWeek(dayRule.substring(0, index))
            mdt.adjustForwards = false
            dayRule = dayRule.substring(index + 2)
          }
        }
        mdt.dayOfMonth = dayRule.toInt
      }
      if (st.hasMoreTokens) {
        val timeStr: String = st.nextToken
        var secsOfDay: Int = parseSecs(timeStr)
        if (secsOfDay == 86400) {
          mdt.endOfDay = true
          secsOfDay = 0
        }
        val time: LocalTime = deduplicate(LocalTime.ofSecondOfDay(secsOfDay))
        mdt.time = time
        mdt.timeDefinition = parseTimeDefinition(timeStr.charAt(timeStr.length - 1))
      }
    }
  }

  /**A map to deduplicate object instances. */
  private var deduplicateMap = new HashMap[Any, Any]
  /**
   * Parses the source files.
   * @throws Exception if an error occurs
   */
  private def parseFiles: Unit = {
    for (file <- sourceFiles) {
      printVerbose("Parsing file: " + file)
      parseFile(file)
    }
  }

  /**
   * Deduplicates an object instance.
   *
   * @param < T > the generic type
   * @param object the object to deduplicate
   * @return the deduplicated object
   */
  private[zone] def deduplicate[T](obj: T): T = {
    if (deduplicateMap.contains(obj) == false) {
      deduplicateMap(obj) = obj
    }
    deduplicateMap.get(obj).asInstanceOf[T]
  }

  private def parsePeriod(str: String): Period = {
    val secs: Int = parseSecs(str)
    deduplicate(Period.ofSeconds(secs).normalized)
  }

  /**
   * Parses a Rule line.
   * @param st the tokenizer, not null
   */
  private def parseRuleLine(st: StringTokenizer): Unit = {
    var rule: TZDBZoneRulesCompiler#TZDBRule = new TZDBRule
    val name: String = st.nextToken
    if (rules.contains(name) == false) {
      rules.put(name, new ArrayBuffer[TZDBZoneRulesCompiler#TZDBRule])
    }
    rules(name) += rule
    rule.startYear = parseYear(st.nextToken, 0)
    rule.endYear = parseYear(st.nextToken, rule.startYear)
    if (rule.startYear > rule.endYear) {
      throw new IllegalArgumentException("Year order invalid: " + rule.startYear + " > " + rule.endYear)
    }
    parseOptional(st.nextToken)
    parseMonthDayTime(st, rule)
    rule.savingsAmount = parsePeriod(st.nextToken)
    rule.text = parseOptional(st.nextToken)
  }

  private def parseSecs(str: String): Int = {
    if (str.equals("-")) return 0
    var pos: Int = if (str.startsWith("-")) 1 else 0
    val pp: ParsePosition = new ParsePosition(pos)
    val cal: DateTimeParseContext = TZDBZoneRulesCompiler.TimeParser.parse(str, pp)
    if (pp.getErrorIndex >= 0) {
      throw new IllegalArgumentException(str)
    }
    val hour: Int = cal.getParsed(ISOChronology.hourOfDayRule).asInstanceOf[Int]
    val min: Int = cal.getParsed(ISOChronology.minuteOfHourRule).asInstanceOf[Int]
    val sec: Int = cal.getParsed(ISOChronology.secondOfMinuteRule).asInstanceOf[Int]
    val secs: Int = hour * 60 * 60 + (if (min != null) min else 0) * 60 + (if (sec != null) sec else 0)
    return if (pos == 1) -secs else secs
  }

  /**
   * Sets the deduplication map.
   */
  private[zone] def setDeduplicateMap(deduplicateMap: HashMap[Any, Any]): Unit = this.deduplicateMap = deduplicateMap

  /**
   * Build the rules, zones and links into real zones.
   * @throws Exception if an error occurs
   */
  private def buildZoneRules: Unit = {
    for (_zoneId <- zones.keySet) {
      printVerbose("Building zone " + _zoneId)
      val zoneId = deduplicate(_zoneId)
      val tzdbZones: ArrayBuffer[TZDBZoneRulesCompiler#TZDBZone] = zones(zoneId)
      var bld: ZoneRulesBuilder = new ZoneRulesBuilder
      for (tzdbZone <- tzdbZones) {
        bld = tzdbZone.addToBuilder(bld, rules)
      }
      val buildRules: ZoneRules = bld.toRules(zoneId, deduplicateMap)
      builtZones = builtZones.updated(zoneId, deduplicate(buildRules))
    }
    for (_aliasId <- links.keySet) {
      val aliasId = deduplicate(_aliasId)
      var realId: String = links(aliasId)
      printVerbose("Linking alias " + aliasId + " to " + realId)
      var realRules: ZoneRules = builtZones.getOrElse(realId, null)
      if (realRules == null) {
        realId = links(realId)
        printVerbose("Relinking alias " + aliasId + " to " + realId)
        realRules = builtZones.getOrElse(realId, null)
        if (realRules == null) {
          throw new IllegalArgumentException("Alias '" + aliasId + "' links to invalid zone '" + realId + "' for '" + version + "'")
        }
      }
      builtZones = builtZones.updated(aliasId, realRules)
    }
    builtZones = builtZones - ("UTC")
    builtZones = builtZones - ("GMT")
  }

  private def parseDayOfWeek(str: String): DayOfWeek =
    DayOfWeek.values.find(_.name.equalsIgnoreCase(str)).getOrElse(throw new IllegalArgumentException("Unknown day-of-week: " + str))

  /**
   * Parses a source file.
   * @param file the file being read, not null
   * @throws Exception if an error occurs
   */
  private def parseFile(file: File): Unit = {
    var lineNumber: Int = 1
    var line: String = null
    var in: BufferedReader = null
    try {
      in = new BufferedReader(new FileReader(file))
      var openZone: ArrayBuffer[TZDBZoneRulesCompiler#TZDBZone] = null
      while (({
        line = in.readLine;
        line
      }) != null) {
        val index: Int = line.indexOf('#')
        if (index >= 0) {
          line = line.substring(0, index)
        }
        if (line.trim.length != 0) {
          val st: StringTokenizer = new StringTokenizer(line, " \t")
          if (openZone != null && Character.isWhitespace(line.charAt(0)) && st.hasMoreTokens) {
            if (parseZoneLine(st, openZone)) {
              openZone = null
            }
          }
          else {
            if (st.hasMoreTokens) {
              val first: String = st.nextToken
              if (first.equals("Zone")) {
                if (st.countTokens < 3) {
                  printVerbose("Invalid Zone line in file: " + file + ", line: " + line)
                  throw new IllegalArgumentException("Invalid Zone line")
                }
                openZone = new ArrayBuffer[TZDBZoneRulesCompiler#TZDBZone]
                zones.put(st.nextToken, openZone)
                if (parseZoneLine(st, openZone)) {
                  openZone = null
                }
              }
              else {
                openZone = null
                if (first.equals("Rule")) {
                  if (st.countTokens < 9) {
                    printVerbose("Invalid Rule line in file: " + file + ", line: " + line)
                    throw new IllegalArgumentException("Invalid Rule line")
                  }
                  parseRuleLine(st)
                }
                else if (first.equals("Link")) {
                  if (st.countTokens < 2) {
                    printVerbose("Invalid Link line in file: " + file + ", line: " + line)
                    throw new IllegalArgumentException("Invalid Link line")
                  }
                  val realId: String = st.nextToken
                  val aliasId: String = st.nextToken
                  links(aliasId) = realId
                }
                else {
                  throw new IllegalArgumentException("Unknown line")
                }
              }
            }
          }
        }
        lineNumber += 1;
      }
    }
    catch {
      case ex: Exception => {
        throw new Exception("Failed while processing file '" + file + "' on line " + lineNumber + " '" + line + "'", ex)
      }
    }
    finally {
      try {
        in.close
      }
      catch {
        case ex: Exception => {
        }
      }
    }
  }

  /**
   * Prints a verbose message.
   * @param message the message, not null
   */
  private def printVerbose(message: String): Unit = {
    if (verbose) {
      System.out.println(message)
    }
  }

  /**
   * Class representing a rule line in the TZDB file.
   */
  private[zone] final class TZDBRule extends TZDBMonthDayTime {
    /**The start year. */
    private[zone] var startYear: Int = 0

    private[zone] def addToBuilder(bld: ZoneRulesBuilder): Unit = {
      adjustToFowards(2004)
      bld.addRuleToWindow(startYear, endYear, month, dayOfMonth, dayOfWeek, time, endOfDay, timeDefinition, savingsAmount)
    }

    /**The end year. */
    private[zone] var endYear: Int = 0
    /**The amount of savings. */
    private[zone] var savingsAmount: Period = null
    /**The text name of the zone. */
    private[zone] var text: String = null
  }

  /**
   * Parses a Zone line.
   * @param st the tokenizer, not null
   * @return true if the zone is complete
   */
  private def parseZoneLine(st: StringTokenizer, zoneList: ArrayBuffer[TZDBZoneRulesCompiler#TZDBZone]): Boolean = {
    val zone: TZDBZoneRulesCompiler#TZDBZone = new TZDBZone
    zoneList += zone
    zone.standardOffset = parseOffset(st.nextToken)
    var savingsRule: String = parseOptional(st.nextToken)
    if (savingsRule == null) {
      zone.fixedSavings = Period.Zero
      zone.savingsRule = null
    }
    else {
      try {
        zone.fixedSavings = parsePeriod(savingsRule)
        zone.savingsRule = null
      }
      catch {
        case ex: Exception => {
          zone.fixedSavings = null
          zone.savingsRule = savingsRule
        }
      }
    }
    zone.text = st.nextToken
    if (st.hasMoreTokens) {
      zone.year = Year.of(st.nextToken.toInt)
      if (st.hasMoreTokens) {
        parseMonthDayTime(st, zone)
      }
      return false
    }
    else {
      return true
    }
  }

  private def parseOffset(str: String): ZoneOffset = {
    val secs: Int = parseSecs(str)
    ZoneOffset.ofTotalSeconds(secs)
  }

  private def matches(str: String, search: String): Boolean = {
    return str.startsWith(search.substring(0, 3)) && search.startsWith(str) && str.length <= search.length
  }

  private def parseTimeDefinition(c: Char): ZoneRulesBuilder.TimeDefinition = {
    c match {
      case 's' | 'S' => return TimeDefinition.Standard
      case 'u' | 'U' | 'g' | 'G' | 'z' | 'Z' => return TimeDefinition.UTC
      case 'w' | 'W' | _ => return TimeDefinition.Wall
    }
  }

  /**
   * Class representing a month-day-time in the TZDB file.
   */
  private[zone] abstract class TZDBMonthDayTime {
    /**Whether this is midnight end of day. */
    private[zone] var endOfDay: Boolean = false
    /**The time of the cutover. */
    private[zone] var time: LocalTime = LocalTime.Midnight
    /**The day-of-month of the cutover. */
    private[zone] var dayOfMonth: Int = 1
    /**The month of the cutover. */
    private[zone] var month: MonthOfYear = MonthOfYear.January
    /**Whether to adjust forwards. */
    private[zone] var adjustForwards: Boolean = true

    private[zone] def adjustToFowards(year: Int): Unit = {
      if (adjustForwards == false && dayOfMonth > 0) {
        val adjustedDate: LocalDate = LocalDate.of(year, month, dayOfMonth).minusDays(6)
        dayOfMonth = adjustedDate.getDayOfMonth
        month = adjustedDate.getMonthOfYear
        adjustForwards = true
      }
    }

    /**The time of the cutover. */
    private[zone] var timeDefinition: ZoneRulesBuilder.TimeDefinition = TimeDefinition.Wall
    /**The day-of-week of the cutover. */
    private[zone] var dayOfWeek: DayOfWeek = null
  }

  /**The TZDB zones. */
  private val zones = new HashMap[String, ArrayBuffer[TZDBZoneRulesCompiler#TZDBZone]]

  /**
   * Class representing a linked set of zone lines in the TZDB file.
   */
  private[zone] final class TZDBZone extends TZDBMonthDayTime {
    /**The standard offset. */
    private[zone] var standardOffset: ZoneOffset = null
    /**The savings rule. */
    private[zone] var savingsRule: String = null

    private def toDateTime(year: Int): LocalDateTime = {
      adjustToFowards(year)
      var date: LocalDate = null
      if (dayOfMonth == -1) {
        dayOfMonth = month.getLastDayOfMonth(ISOChronology.isLeapYear(year))
        date = LocalDate.of(year, month, dayOfMonth)
        if (dayOfWeek != null) {
          date = date.`with`(DateAdjusters.previousOrCurrent(dayOfWeek))
        }
      }
      else {
        date = LocalDate.of(year, month, dayOfMonth)
        if (dayOfWeek != null) {
          date = date.`with`(DateAdjusters.nextOrCurrent(dayOfWeek))
        }
      }
      date = deduplicate(date)
      var ldt: LocalDateTime = LocalDateTime.of(date, time)
      if (endOfDay) {
        ldt = ldt.plusDays(1)
      }
      return ldt
    }

    /**The text name of the zone. */
    private[zone] var text: String = null

    private[zone] def addToBuilder(bld: ZoneRulesBuilder, rules: HashMap[String, ArrayBuffer[TZDBZoneRulesCompiler#TZDBRule]]): ZoneRulesBuilder = {
      if (year != null) {
        bld.addWindow(standardOffset, toDateTime(year.getValue), timeDefinition)
      }
      else {
        bld.addWindowForever(standardOffset)
      }
      if (fixedSavings != null) {
        bld.setFixedSavingsToWindow(fixedSavings)
      }
      else {
        val tzdbRules: ArrayBuffer[TZDBZoneRulesCompiler#TZDBRule] = rules.getOrElse(savingsRule, throw new IllegalArgumentException("Rule not found: " + savingsRule))
        for (tzdbRule <- tzdbRules) {
          tzdbRule.addToBuilder(bld)
        }
      }
      return bld
    }

    /**The fixed savings amount. */
    private[zone] var fixedSavings: Period = null
    /**The year of the cutover. */
    private[zone] var year: Year = null
  }

  /**The TZDB links. */
  private var links = new HashMap[String, String]

  /**The built zones. */
  private var builtZones = new TreeMap[String, ZoneRules]

  /**The TZDB rules. */
  private val rules = new HashMap[String, ArrayBuffer[TZDBZoneRulesCompiler#TZDBRule]]
}