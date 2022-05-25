package homework3

import homework3.DateError.*

import scala.util.Try

enum DateError:
  case YearIsNotAnInteger(year: String)
  case MonthIsNotAnInteger(month: String)
  case DayIsNotAnInteger(day: String)
  case MonthOutOfRange(month: Int)
  case DayOutOfRange(day: Int)
  case InvalidDate(year: Int, month: Int, day: Int)

object DateValidation:
  def toInteger(n: String): Option[Int] = Try(n.toInt).toOption

  def validateInRange[E](range: Range)(error: E)(n: Int): Validated[E, Int] =
    if range.contains(n) then Valid(n)
    else Invalid(error)

  def validateDate(year: String, month: String, day: String): Validated[DateError, Date] =
    val validatedYear = toInteger(year).toValidated(YearIsNotAnInteger(year))

    val validatedMonth = for
      m <- toInteger(month).toValidated(MonthIsNotAnInteger(month))
      _ <- validateInRange(1 to 12)(MonthOutOfRange(m))(m)
    yield m

    val validatedDay = for
      d <- toInteger(day).toValidated(DayIsNotAnInteger(day))
      _ <- validateInRange(1 to 31)(DayOutOfRange(d))(d)
    yield d

    (
      validatedYear,
      validatedMonth,
      validatedDay
    ).zipN.flatMap { case (y, m, d) =>
      Date.applyOption(y, m, d).toValidated(InvalidDate(y, m, d))
    }
