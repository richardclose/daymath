package org.phasanix.daymath

/**
 * Representation of a date as bitfields of an Int,
 * to avoid allocation.
 *
 * Suitable for fast access to d/m/y fields and sorting.
 * Not suitable for date arithmetic.
 */
class PackedDate(val value: Int) extends AnyVal {

  /** day of month */
  def day: Int = value & 0x1f

  /** month of year (Jan = 1) */
  def month: Int = (value >> 5) & 0xf

  /** year */
  def year: Int = (value >> 9) & 0x3fff

  def asDays: Days = Days(day, month, year)

  def toStr: String = s"$year/$month/$day"
}

object PackedDate {
  def apply(value: Int): PackedDate = new PackedDate(value)

  def apply(year: Int, month: Int, day: Int) = {
    new PackedDate((year << 9) | (month << 5) | day)
  }
}  
