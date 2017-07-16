package org.phasanix.daymath

import java.util.{Calendar, Date, GregorianCalendar, TimeZone}

/**
 * Conversions between java.util.{Date,Calendar} and Days
 */
object Jdk7 {

  implicit class RichDate(val date: Date) extends AnyVal {
    def asDays(tz: TimeZone): Days = toDays(date.getTime, tz)
  }

  implicit class RichDays(val days: Days) {

    def asDate(tz: TimeZone): Date = {
      val cal = Calendar.getInstance(tz)
      val pd = days.asPackedDate
      cal.set(Calendar.DAY_OF_MONTH, pd.day)
      cal.set(Calendar.MONTH, pd.month - 1)
      cal.set(Calendar.YEAR, pd.year)
      cal.getTime
    }

    def asCalendar(tz: TimeZone): Calendar = {
      val cal = new GregorianCalendar()
      cal.setTimeZone(tz)
      cal.setTime(asDate(tz))
      cal
    }

  }

  implicit class RichCalendar(val cal: Calendar) extends AnyVal {
    def asDays: Days = Days(cal.get(Calendar.DAY_OF_MONTH), 1 + cal.get(Calendar.MONTH), cal.get(Calendar.YEAR))
  }

  /** Convert milliseconds to <code>Days</code> for the given time zone */
  def toDays(millis: Long, tz: TimeZone): Days = {
    val cal =  Calendar.getInstance(tz)
    cal.setTimeInMillis(millis)
    cal.asDays
  }

  /** Number of days between the given dates */
  def daysBetween(d1: Date, d2: Date): Int = {
    val cal = Calendar.getInstance(TimeZone.getDefault)
    cal.setTime(d1)
    val days1 = cal.asDays
    cal.setTime(d2)
    val days2 = cal.asDays
    days2.dayNumber - days1.dayNumber
  }
}
