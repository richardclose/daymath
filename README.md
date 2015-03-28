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

### PackedDate
An instance of `PackedDate` is a triple of day/month/year, stored as bitfields
of an `Int`. The use case is fast access to the day/month/year fields, and 
correct sorting, but not date arithmetic.

### Jdk7
Conversions between `Days` and `java.util.Date`/`java.util.Calendar`.

### Jdk8
Conversions between `Days` and `java.time.LocalDate`.


