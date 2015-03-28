import org.phasanix.daymath.{Jdk7, Days}
import org.scalatest._
import java.util.Calendar
import java.text.SimpleDateFormat
import java.util.TimeZone

class DaysSpec extends FlatSpec with Matchers {

  val dfmt = new SimpleDateFormat("yyyy/MM/dd")

  "Days" should "convert correctly" in {

      val tz = TimeZone.getDefault

      val dates = Seq("1844/11/11", "2011/1/11", "2015/3/25", "2015/3/29", "2015/3/30", "2015/3/31", "2015/4/1", "2022/2/3")

      val diffs = for (dstr <- dates) yield {
        val d = dfmt.parse(dstr)
        val days = Jdk7(d, tz)
        (dstr, days.asMillis - d.getTime)
      }

      for ((date, diff) <- diffs) println(s"$date -- $diff")

    }

}
