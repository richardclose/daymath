package org.phasanix.daymath

import java.util.Calendar

/**
 * Value class that represents base dates without a time or timezone.
 * Intended for fast arithmetic, avoidance of allocation and
 * compact JSON represee
 */
class Days(val dayNumber: Int) extends AnyVal {

  /** As milliseconds since 1/1/1970 */
  def asMillis: Long = (dayNumber  - Days.StartOfUnixEpoch) * Days.MillisPerDay


  /** as PackedDate */
  def asPackedDate: PackedDate = Days.toPackedDate(dayNumber)

  /** As tuple of (day, month, year), where Jan == 1 */
  def asDMY: (Int, Int, Int) = Days.toDMY(dayNumber)

  /** Day of week number, where SUNDAY=1, MONDAY=2, as per j.u.Calendar */
  def dowNumber: Int = ((dayNumber + 3) % 7) + 1

  /** Day of week */
  def dayOfWeek: DayOfWeek = Days.DaysOfWeek(dowNumber - 1)

  /** Day of month */
  def dayOfMonth: Int = Days.toDayOfMonth(dayNumber)

  /** Month number (Jan == 1) */
  def month: Int = Days.toMonth(dayNumber)

  /** Year */
  def year: Int = Days.toYear(dayNumber)
  def addDays(count: Int): Days = Days(dayNumber + count)

  /** True if this date is after that date */
  def after(that: Days): Boolean = dayNumber > that.dayNumber

  /** True if this date is before that date */
  def before(that: Days): Boolean = dayNumber < that.dayNumber

  /** True if this date is a weekend */
  def isWeekend: Boolean =
    dowNumber == Calendar.SATURDAY || dowNumber == Calendar.SUNDAY

  /** True if this date is a weekday */
  def isWeekday: Boolean = !isWeekend

  /** Last day of the current month */
  def lastDayOfThisMonth: Days = {
    // Find a date that is definitely in the following month,
    // then move back to end of the current month.
    val attempt1 = addDays(28)
    val attempt2 = if (attempt1.month == this.month) attempt1.addDays(7) else attempt1
    attempt2.addDays(-attempt2.dayOfMonth)
  }

  /** First day of following month */
  def firstDayOfNextMonth: Days = lastDayOfThisMonth.addDays(1)

  /** Next instance of the given day of week, including this date */
  def nextWeekday(dow: Int): Days = {
    val x = dowNumber - 1
    val diff = ((dow - 1 - x) + 7) % 7
    addDays(diff)
  }
}

/** Day of week constants */
sealed class DayOfWeek(val day: Int, val name: String) {
  override def toString: String = name
}

object Sunday extends DayOfWeek(Calendar.SUNDAY, "Sunday")
object Monday extends DayOfWeek(Calendar.MONDAY, "Monday")
object Tuesday extends DayOfWeek(Calendar.TUESDAY, "Tuesday")
object Wednesday extends DayOfWeek(Calendar.WEDNESDAY, "Wednesday")
object Thursday extends DayOfWeek(Calendar.THURSDAY, "Thursday")
object Friday extends DayOfWeek(Calendar.FRIDAY, "Friday")
object Saturday extends DayOfWeek(Calendar.SATURDAY, "Saturday")


object Days {

  /** Day number of start of Unix epoch */
  val StartOfUnixEpoch = toDayNumber(1, 1, 1970)

  /** Day number of start of modified Julian epoch */
  val StartOfModifiedJulianEpoch = toDayNumber(16, 11, 1858)

  /** Number of milliseconds in a day */
  val MillisPerDay: Long = 1000 * 60 * 60 * 24

  val DaysOfWeek: Seq[DayOfWeek] = Seq(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)

  def daysBetween(d1: Days, d2: Days): Int = d2.dayNumber - d1.dayNumber

  def apply(days: Int): Days = new Days(days)

  def apply(day: Int, month: Int, year: Int): Days = new Days(toDayNumber(day, month, year))

  /** day number to DMY tuple */
  def toDMY(dayNumber: Int): (Int, Int, Int) = {
    var y = (10000L * dayNumber + 14780) / 3652425
    var ddd = dayNumber - (y * 365 + y / 4 - y / 100 + y / 400)
    if (ddd < 0) {
      y -= 1
      ddd = dayNumber - (y * 365 + y / 4 - y / 100 + y / 400)
    }
    val mi = (52 + 100 * ddd) / 3060
    val year = y + (mi + 2) / 12
    val month = (mi + 2) % 12 + 1
    val day = ddd - (mi * 306 + 5) / 10 + 1
    (day.toInt, month.toInt, year.toInt)
  }

  /** day number to PackedDate */
  def toPackedDate(dayNumber: Int): PackedDate = {
    var y = (10000L * dayNumber + 14780) / 3652425
    var ddd = dayNumber - (y * 365 + y / 4 - y / 100 + y / 400)
    if (ddd < 0) {
      y -= 1
      ddd = dayNumber - (y * 365 + y / 4 - y / 100 + y / 400)
    }
    val mi = (52 + 100 * ddd) / 3060
    val year = y + (mi + 2) / 12
    val month = (mi + 2) % 12 + 1
    val day = ddd - (mi * 306 + 5) / 10 + 1
    PackedDate(year.toInt, month.toInt, day.toInt)
  }

  /** day number to day of month */
  def toDayOfMonth(dayNumber: Int): Int = {

    var y = (10000L * dayNumber + 14780) / 3652425
    var ddd = dayNumber - (y * 365 + y / 4 - y / 100 + y / 400)
    if (ddd < 0) {
      y -= 1
      ddd = dayNumber - (y * 365 + y / 4 - y / 100 + y / 400)
    }
    val mi = (52 + 100 * ddd) / 3060
    (ddd - (mi * 306 + 5) / 10 + 1).toInt
  }

  /** day number to year */
  def toYear(dayNumber: Int): Int = {

    var y = (10000L * dayNumber + 14780) / 3652425
    var ddd = dayNumber - (y * 365 + y / 4 - y / 100 + y / 400)
    if (ddd < 0) {
      y -= 1
      ddd = dayNumber - (y * 365 + y / 4 - y / 100 + y / 400)
    }
    val mi = (52 + 100 * ddd) / 3060
    (y + (mi + 2) / 12).toInt
  }

  /** day number to month number (Jan == 0) */
  def toMonth(dayNumber: Int): Int = {

    var y = (10000L * dayNumber + 14780) / 3652425
    var ddd = dayNumber - (y * 365 + y / 4 - y / 100 + y / 400)
    if (ddd < 0) {
      y -= 1
      ddd = dayNumber - (y * 365 + y / 4 - y / 100 + y / 400)
    }
    val mi = (52 + 100 * ddd) / 3060
    ((mi + 2) % 12 + 1).toInt
  }

  /** (day, month, year) to day number */
  def toDayNumber(d: Int, m: Int, y: Int): Int = {
    val m1 = (m + 9) % 12
    val y1 = y - m1 / 10
    (365 * y1) + y1 / 4 - y1 / 100 + y1 / 400 + (m1 * 306 + 5) / 10 + (d - 1)
  }

  /**
   * Stream of days between the given dates
   */
  def range(start: Days, end: Days, step: Int = 1): Stream[Days] =
    if (start.after(end)) Stream.empty
    else start #:: range(start.addDays(step), end, step)

  /**
   * Stream of weekdays between the given dates
   */
  def weekdays(start: Days, end: Days): Stream[Days] = for (d <- range(start, end) if d.isWeekday) yield d


  /**
   * Stream of first days of month between the given dates
   */
  def monthBoundaries(start: Days, end: Days): Stream[Days] = {
    val next = start.firstDayOfNextMonth
    if (!start.before(end)) Stream.empty
    else start #:: monthBoundaries(next, end)
  }

  /**
   * Stream of monthBoundaries, then the end date.
   */
  def monthBoundariesInclusive(start: Days, end: Days): Stream[Days] =
    monthBoundaries(start, end) :+ end

  /**
   * Infinite stream of days, starting at the given date
   */
  def days(start: Days): Stream[Days] = start #:: days(start.addDays(1))

  /**
   * Infinite stream of weekdays, starting at the given date
   */
  def weekdays(start: Days): Stream[Days] = days(start) filter (_.isWeekday)
}
