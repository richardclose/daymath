package org.phasanix.daymath

import java.time.LocalDate

/**
 * Conversions between java.time.LocalDate and Days
 * (FIXME: incomplete)
 */
object Jdk8 {

  implicit class RichDays(val days: Days) {
    def asLocalDate: LocalDate = {
      val pd = days.asPackedDate
      LocalDate.of(pd.year, pd.month, pd.day)
    }
  }

  implicit class RichLocalDate(val ld: LocalDate) extends AnyVal {
    def asDays: Days = {
      Days(ld.getDayOfMonth, ld.getMonthValue, ld.getYear)
    }
  }

}


