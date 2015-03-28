package org.phasanix.daymath

import java.time.LocalDate

/**
 * Conversions between java.time.LocalDate and Days
 * (FIXME: incomplete)
 */
object Jdk8 {

  def localDateAsDays(ld: LocalDate): Days = {
    Days(ld.getDayOfMonth, ld.getMonthValue, ld.getYear)
  }
}


