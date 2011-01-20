package javax.time

import calendar._
import java.util.concurrent.TimeUnit._
import javax.time.calendar.MonthOfYear._

object Main {
  def main(arg: Array[String]) {

    val someDate = LocalDate(2000, January, 31)
    /* Exception in thread "main" javax.time.calendar.InvalidCalendarFieldException:
       Illegal value for DayOfMonth field, value 31 is not valid for month javax.time.calendar.MonthOfYear$February$ */
    //val someInvalidDate = LocalDate(2000, February, 31)


    val someTime = LocalTime.of(11, 22, 33)
    /* Exception in thread "main" javax.time.calendar.IllegalCalendarFieldValueException:
       Illegal value for ISO.HourOfDay field, value 25 is not in the range 0 to 23 */
    //val someInvalidTime = LocalTime.of(25, 26, 17)

    val someDay = DayOfWeek.Friday

    val someMonth = MonthOfYear.August

    val someYear = Year(2011)

    val someMonthDay = MonthDay(April, 1)
    val otherMonthDay = someMonthDay.copy(March)

    val someYearMonth = YearMonth(2000, December)
    val otherYearMonth = someYearMonth.copy(month = August)

    val someLocalDate = LocalDate(2010, January, 20)
    val otherLocalDate = someLocalDate.copy(day = 21)

    val someDateTime = LocalDateTime.of(someDate, someTime)
    val otherDateTime = LocalDateTime.of(2011, 12, 13)(12, 12, 12)

    val someZonedDateTime = ZonedDateTime.of(2010, 12, 1)(22, 10, 5)(ZoneId.of(ZoneOffset.UTC))

    val someInstant = Instant.now
    val someDuration = Duration.of(10, MINUTES)
    val somePeriod = Period.of(years = 1, months = 2, days = 3)

    val newTime = someTime + someDuration
    val newInstant = someInstant + someDuration

    def patternMatch(a: Any) = a match {
      case Year(year) => "Year: " + year
      case YearMonth(_, December) => "YearMonth ... and it's december!"
      case YearMonth => "YearMonth"
      case MonthDay => "MonthDay"
      case moy: MonthOfYear => "Some month: " + moy
      case _ => ""
    }

    val timeThings = List(someDate, someTime, someDay, someMonth, someYear, someMonthDay, otherMonthDay,
      someYearMonth, otherYearMonth, someDateTime, otherDateTime, someZonedDateTime, someInstant, someDuration,
      somePeriod, newTime, newInstant)

    if (someInstant < newInstant) println("Smaller!")
    else println("Bigger!")

    timeThings foreach println

    timeThings map patternMatch filterNot (_.isEmpty) foreach println

  }
}

