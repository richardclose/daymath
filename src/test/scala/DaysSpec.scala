import java.time.LocalDate

import org.phasanix.daymath.{Jdk8, Jdk7, Days}
import org.scalatest._
import java.text.SimpleDateFormat
import java.util.TimeZone

class DaysSpec extends FlatSpec with Matchers {

  val jdk7dfmt = new SimpleDateFormat("yyyy/MM/dd z")
  val utc = TimeZone.getTimeZone("UTC")

  val dates = Seq(
    "1844/11/11",
    "2011/01/11",
    "2015/03/25",
    "2015/03/29",
    "2015/03/30",
    "2015/03/31",
    "2015/04/01",
    "2022/02/03",
    "2045/03/04")

  "Days" should "convert correctly from java.util.Date to Days" in {

    val diffs = for (dstr <- dates) yield {
      val d = jdk7dfmt.parse(dstr + " UTC")
      val days = Jdk7.toDays(d, utc)
      (dstr, days.asMillis - d.getTime)
    }

    diffs.filter(_._2 != 0L) shouldBe empty
  }

  it should "convert correctly from java.time.LocalDate to Days" in {

    val fmt = java.time.format.DateTimeFormatter.ofPattern("yyyy/MM/dd")

    val diffs = for (dstr <- dates) yield {
      val ld = LocalDate.parse(dstr, fmt)
      val days = Jdk8.toDays(ld)
      days.dayOfMonth == ld.getDayOfMonth && days.month == ld.getMonthValue && days.year == ld.getYear
    }

    diffs.filter(_ == false) shouldBe empty
  }

  "month start iterator" should "generate month start dates" in {
    val xs = Days.monthStartIterator(Days(14, 5, 2015), Days(4, 8, 2015)).toIndexedSeq
    println(xs)
    xs.length shouldBe 3
    xs.count(_.dayOfMonth == 1) shouldBe xs.length
  }

  it should "generate month start dates (inclusive)" in {
    val xs = Days.monthStartIteratorInclusive(Days(14, 5, 2015), Days(4, 8, 2015)).toIndexedSeq
    println(xs)
    xs.length shouldBe 5
    xs.count(_.dayOfMonth == 1) shouldBe xs.length - 2
  }

  "month end iterator" should "generate month start dates" in {
    val xs = Days.monthStartIterator(Days(14, 5, 2015), Days(4, 8, 2015)).toIndexedSeq
    println(xs)
    xs.length shouldBe 3
    xs.count(_.dayOfMonth == 1) shouldBe xs.length
  }

  it should "generate month start dates (inclusive)" in {
    val xs = Days.monthStartIteratorInclusive(Days(14, 5, 2015), Days(4, 8, 2015)).toIndexedSeq
    println(xs)
    xs.length shouldBe 5
    xs.count(_.dayOfMonth == 1) shouldBe xs.length - 2
  }

  "month end logic" should "find end of current month" in {
    val d = Days(12, 4, 2011)
    d.lastDayOfThisMonth shouldBe Days(30, 4, 2011)
    val leapYearFeb = Days(4, 2, 2012)
    leapYearFeb.lastDayOfThisMonth shouldBe Days(29, 2, 2012)
  }

  it should "identify current day when it is the last of the month" in {
    val d = Days(31, 3, 2011)
    d.lastDayOfThisMonth shouldBe d
  }

}
