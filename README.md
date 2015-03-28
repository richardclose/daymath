# DayMath
Date calculations with fast integer math, excluding timezone and time of day.

## Introduction
This library provides date calculations using Gary Katch's [algorithm]
(http://alcor.concordia.ca/~gpkatch/gdate-algorithm.html). Value types are 
used to avoid memory allocations.

## API

### Days
An instance of `Days` represents a calendar date, without regard to timezone.
This class has several utility methods for generating date ranges, date 
arithmetic and so forth.

#### Examples
```scala
import org.phasanix.daymath._
val d = Days(24,7,1978)

println(d.dayOfWeek) // Monday

println(d.lastDayOfThisMonth) // 31/7/1978

for (day <- Days.weekdays(d, d.firstDayOfNextMonth))
  println(s"$day (${day.dayOfWeek})")
  
/** prints:
24/7/1978 (Monday)
25/7/1978 (Tuesday)
26/7/1978 (Wednesday)
27/7/1978 (Thursday)
28/7/1978 (Friday)
31/7/1978 (Monday)
1/8/1978 (Tuesday)
*/

for (day <- Days.monthBoundaries(d, d.addDays(300)))
  println(s"$day (${day.dayOfWeek})")

/** prints: 
24/7/1978 (Monday)
1/8/1978 (Tuesday)
1/9/1978 (Friday)
1/10/1978 (Sunday)
*/

```

### PackedDate
An instance of `PackedDate` is a triple of day/month/year, stored as bitfields
of an `Int`. The use case is fast access to the day/month/year fields, and 
correct sorting, but not date arithmetic.

### Jdk7
Conversions between `Days` and `java.util.Date`/`java.util.Calendar`.

### Jdk8
Conversions between `Days` and `java.time.LocalDate`.


