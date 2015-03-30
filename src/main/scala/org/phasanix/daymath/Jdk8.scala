package org.phasanix.daymath

import java.time.LocalDate

/**
 * Conversions between java.time.LocalDate and Days
 * (FIXME: incomplete)
 */
object Jdk8 {

  def toDays(ld: LocalDate): Days = {
    Days(ld.getDayOfMonth, ld.getMonthValue, ld.getYear)
  }

  def toLocalDate(days: Days): LocalDate = {
    val pd = days.asPackedDate
    LocalDate.of(pd.year, pd.month, pd.day)
  }

}


