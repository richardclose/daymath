/**
 * Benchmarking.
 *
 * (Work in progress: for some reason, SBT is not picking this up yet).
 */

import org.phasanix.daymath.Days
import org.scalameter.api.PerformanceTest

object DaysBench extends PerformanceTest.Quickbenchmark {

  val day1 = Days(31, 4, 2013)

  performance of "Days" in {

    measure method "asPackedDate" in {
      val range = day1.dayNumber until day1.dayNumber + 1000

      val x = range.foldLeft(0) { (acc, n) =>
        val day = Days(n)
        val pd = day.asPackedDate
        acc + pd.day
      }
    }

  }

}
