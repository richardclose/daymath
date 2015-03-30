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

}
