import java.time.LocalDate
import org.phasanix.daymath._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.text.SimpleDateFormat
import java.util.TimeZone

class DaysSpec extends AnyFlatSpec with Matchers {

  val jdk7dfmt = new SimpleDateFormat("yyyy/MM/dd z")
  private val utc = TimeZone.getTimeZone("UTC")

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

  {
    import Jdk7._

    "Jdk7 conversions" should "convert java.util.Date -> Days" in {

      val diffs = for (dstr <- dates) yield {
        val d = jdk7dfmt.parse(dstr + " UTC")
        val days = d.asDays(utc)
        (dstr, days.asMillis - d.getTime)
      }

      diffs.filter(_._2 != 0L) shouldBe empty
    }

    it should "convert Days -> java.util.Calendar -> Days" in {
      val d = PackedDate(2011, 11, 12).asDays
      val cal = d.asCalendar(utc)
      cal.asDays.dayNumber shouldBe d.dayNumber
    }

  }

  {
    import Jdk8._

    "Jdk8 conversions" should "convert java.time.LocalDate -> Days" in {

      val fmt = java.time.format.DateTimeFormatter.ofPattern("yyyy/MM/dd")

      val diffs = for (dstr <- dates) yield {
        val ld = LocalDate.parse(dstr, fmt)
        val days = ld.asDays
        days.dayOfMonth == ld.getDayOfMonth && days.month == ld.getMonthValue && days.year == ld.getYear
      }

      diffs.filter(_ == false) shouldBe empty
    }
  }

  "Day math" should "add weekdays correctly" in {
    val d = Days(28, 7, 2016) // Thursday

    d.dayOfWeek shouldBe DayOfWeek.THURSDAY

    val d1 = d.addWeekdays(2)
    val d2 = d.addWeekdays(7)
    val d3 = d.addWeekdays(-4)
    val d4 = d.addWeekdays(-11)

    d1 shouldBe Days(1, 8, 2016) // expected: Monday 1/8/2016
    d2 shouldBe Days(8, 8, 2016) // expected: Monday 8/8/2016
    d3 shouldBe Days(22, 7, 2016) // Expected: Friday 22/7/2016
    d4 shouldBe Days(13, 7, 2016) // Expected: Wednesday 13/7/2016
  }

  "month start iterator" should "generate month start dates" in {
    val xs = Days.monthStartIterator(Days(14, 5, 2015), Days(4, 8, 2015)).toIndexedSeq
    xs.length shouldBe 3
    xs.count(_.dayOfMonth == 1) shouldBe xs.length
  }

  it should "generate month start dates (inclusive)" in {
    val xs = Days.monthStartIteratorInclusive(Days(14, 5, 2015), Days(4, 8, 2015)).toIndexedSeq
    xs.length shouldBe 5
    xs.count(_.dayOfMonth == 1) shouldBe xs.length - 2
  }

  "month end iterator" should "generate month end dates" in {
    val xs = Days.monthEndIterator(Days(14, 5, 2015), Days(4, 8, 2015)).toIndexedSeq
    xs.length shouldBe 3
  }

  it should "generate month end dates (inclusive)" in {
    val xs = Days.monthEndIteratorInclusive(Days(14, 5, 2015), Days(4, 8, 2015)).toIndexedSeq
    xs.length shouldBe 5
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
