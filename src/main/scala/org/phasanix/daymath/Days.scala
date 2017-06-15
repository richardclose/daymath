package org.phasanix.daymath

import java.util.Calendar

/**
 * Value class that represents base dates without a time or timezone.
 * Intended for fast arithmetic, avoidance of allocation and
 * compact JSON representation
 */
class Days(val dayNumber: Int) extends AnyVal {

  /**
   * As milliseconds since 1/1/1970
   *
   * Probably wrong, because of leap-seconds and such. Need to check.
   */
  def asMillis: Long = (dayNumber  - Days.StartOfUnixEpoch) * Days.MillisPerDay


  /** as PackedDate */
  def asPackedDate: PackedDate = Days.toPackedDate(dayNumber)

  /** As tuple of (day, month, year), where Jan == 1 */
  def asDMY: (Int, Int, Int) = Days.toDMY(dayNumber)

  /** Day of week number, where SUNDAY=1, MONDAY=2, as per j.u.Calendar */
  def dowNumber: Int = ((dayNumber + 3) % 7) + 1

  /** Day of week */
  def dayOfWeek: DayOfWeek = Days.DAYS_OF_WEEK(dowNumber - 1)

  /** Day of month */
  def dayOfMonth: Int = Days.toDayOfMonth(dayNumber)

  /** Month number (Jan == 1) */
  def month: Int = Days.toMonth(dayNumber)

  /** Year */
  def year: Int = Days.toYear(dayNumber)

  /** Days instance which is count days after (or if count is
    * negative, before) this date */
  def addDays(count: Int): Days = Days(dayNumber + count)

  /** Add (or subtract) weekdays, taking account of weekends.
    * If this is a weekend date, the count will start from the
    * next (or previous) weekday.
    */
  def addWeekdays(count: Int): Days = {
    val dow = dowNumber

    // Adjustment to set current day to next (previous) weekday
    val adj = dow match {
      case Calendar.SATURDAY => if (count < 0) -1 else 2
      case Calendar.SUNDAY => if (count < 0) -2 else 1
      case _ => 0
    }

    // Adjust DOW to next (previous) weekday, then normalise such that Mon=0
    val normDow = adj + (if (dow == Calendar.SUNDAY) 6 else dow - 2)

    val weeks = count / 5
    val rem = count % 5

    // Add 2 days if adding the remainder includes a weekend
    val includedWeekend =
      if (normDow + rem < 0) -2
      else if (normDow + rem > 4) 2
      else 0

    val diff = (weeks * 7) + adj + rem + includedWeekend

    Days(dayNumber + diff)
  }

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
  def nextOccurrenceOfDow(dow: Int): Days = {
    val x = dowNumber - 1
    val diff = ((dow - 1 - x) + 7) % 7
    addDays(diff)
  }

  /** Next instance of the given day of week, including this date */
  def nextOccurrenceOf(dow: DayOfWeek): Days = nextOccurrenceOfDow(dow.dowNumber)

  override def toString: String = s"$dayOfMonth/$month/$year"
}

object DayOfWeek {
  private val names = Seq("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

  def name(dowNumber: Int): String = {
    if (dowNumber < 1 || dowNumber > 7)
      "?"
    else
      names(dowNumber-1)
  }

  def isWeekday(dayOfWeek: DayOfWeek): Boolean =
    !(dayOfWeek.dowNumber == Calendar.SUNDAY || dayOfWeek.dowNumber == Calendar.SATURDAY)

  val SUNDAY: DayOfWeek = DayOfWeek(Calendar.SUNDAY)
  val MONDAY: DayOfWeek = DayOfWeek(Calendar.MONDAY)
  val TUESDAY: DayOfWeek = DayOfWeek(Calendar.TUESDAY)
  val WEDNESDAY: DayOfWeek = DayOfWeek(Calendar.WEDNESDAY)
  val THURSDAY: DayOfWeek = DayOfWeek(Calendar.THURSDAY)
  val FRIDAY: DayOfWeek = DayOfWeek(Calendar.FRIDAY)
  val SATURDAY: DayOfWeek = DayOfWeek(Calendar.SATURDAY)

  val DAYS_OF_WEEK = Seq(SUNDAY, MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY)

}


/** Day of week constants */
case class DayOfWeek(dowNumber: Int) extends AnyVal {
  override def toString: String = DayOfWeek.name(dowNumber)

  def isWeekday: Boolean = DayOfWeek.isWeekday(this)
}


object Days {

  /** Day number of start of Unix epoch */
  val StartOfUnixEpoch: Int = toDayNumber(1, 1, 1970)

  /** Day number of start of modified Julian epoch */
  val StartOfModifiedJulianEpoch: Int = toDayNumber(16, 11, 1858)

  /** nil value for Days */
  val Nil: Days = apply(0)

  /** Number of milliseconds in a day */
  val MillisPerDay: Long = 1000 * 60 * 60 * 24

  val DAYS_OF_WEEK: Seq[DayOfWeek] = DayOfWeek.DAYS_OF_WEEK

  /** Number of days between two days */
  def daysBetween(d1: Days, d2: Days): Int = d2.dayNumber - d1.dayNumber

  /** Create Days instance from day number */
  def apply(days: Int): Days = new Days(days)

  /** Create Days instance from day, month, year */
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

  private abstract class DaysIterator(start: Days, end: Days) extends Iterator[Days] {

    protected def move(days: Days): Days

    private var _current = Nil // position before start
    private var _next = start

    def hasNext: Boolean = !_next.after(end)

    def next(): Days = {
      _current = _next
      _next = move(_next)
      _current
    }
  }

  /**
   * iterator over range of days
   */
  def iterator(start: Days, end: Days, step: Int = 1): Iterator[Days] = new DaysIterator(start, end) {
    protected def move(days: Days): Days = days.addDays(step)
  }

  /**
   * iterator over range of weekdays
   */
  def weekdaysIterator(start: Days, end: Days): Iterator[Days] =
    iterator(start, end).filter(_.isWeekday)

  /**
   * iterator over first days of month, falling between start and end dates
   */
  def monthStartIterator(start: Days, end: Days): Iterator[Days] = new DaysIterator(start.firstDayOfNextMonth, end) {
    protected def move(days: Days): Days = days.firstDayOfNextMonth
  }

  /**
   * iterator over first days of month, including start and end dates
   */
  def monthStartIteratorInclusive(start: Days, end: Days): Iterator[Days] = {
    val it = new DaysIterator(start, end) {
      protected def move(days: Days): Days = days.firstDayOfNextMonth
    }
    if (end.dayOfMonth == 1)
      it
    else
      it ++ Iterator.single(end)
  }

  /**
   * iterator over last days of month, falling between start and end dates
   */
  def monthEndIterator(start: Days, end: Days): Iterator[Days] = new DaysIterator(start.lastDayOfThisMonth, end) {
    protected def move(days: Days): Days = {
      val ldotm = days.lastDayOfThisMonth
      if (days.before(ldotm)) ldotm else ldotm.addDays(1).lastDayOfThisMonth
    }
  }

  /**
   * iterator over last days of month, including start and end dates
   */
  def monthEndIteratorInclusive(start: Days, end: Days): Iterator[Days] = {
    val it = new DaysIterator(start, end) {
      protected def move(days: Days): Days = {
        val ldotm = days.lastDayOfThisMonth
        if (days.before(ldotm)) ldotm else ldotm.addDays(1).lastDayOfThisMonth
      }
    }
    if (end == end.lastDayOfThisMonth)
      it
    else
      it ++ Some(end).iterator
  }

}
