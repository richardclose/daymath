package org.phasanix.daymath

import java.util.{TimeZone, Calendar, Date}

/**
 * Conversions between java.util.{Date,Calendar} and Days
 */
object Jdk7 {

  /** Days j.u.Date */
  def toDate(days: Days, tz: TimeZone): Date = {
    val cal = Calendar.getInstance(tz)
    val pd = days.asPackedDate
    cal.set(Calendar.DAY_OF_MONTH, pd.day)
    cal.set(Calendar.MONTH, pd.month - 1)
    cal.set(Calendar.YEAR, pd.year)
    cal.getTime
  }

  /** Convert j.u.Calendar to Days */
  def toDays(cal: Calendar): Days = Days(cal.get(Calendar.DAY_OF_MONTH), 1 + cal.get(Calendar.MONTH), cal.get(Calendar.YEAR))

  /** Convert milliseconds to <code>Days</code> for the given time zone */
  def toDays(millis: Long, tz: TimeZone): Days = {
    val cal =  Calendar.getInstance(tz)
    cal.setTimeInMillis(millis)
    toDays(cal)
  }

  /** Convert j.u.Date to Days */
  def toDays(d: Date, tz: TimeZone): Days = toDays(d.getTime, tz)

  /** Number of days between the given dates */
  def daysBetween(d1: Date, d2: Date): Int = {
    val cal = Calendar.getInstance(TimeZone.getDefault)
    cal.setTime(d1)
    val days1 = toDays(cal)
    cal.setTime(d2)
    val days2 = toDays(cal)
    days2.dayNumber - days1.dayNumber
  }
}
