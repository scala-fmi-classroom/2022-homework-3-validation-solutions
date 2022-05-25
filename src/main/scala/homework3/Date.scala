package homework3

import java.time.{DateTimeException, LocalDate}

import scala.util.Try

case class Date(year: Int, month: Int, day: Int) extends Ordered[Date]:
  def compare(that: Date): Int = Ordering[(Int, Int, Int)].compare(
    (year, month, day),
    (that.year, that.month, that.day)
  )

object Date:
  def applyOption(year: Int, month: Int, day: Int): Option[Date] = Try {
    LocalDate.of(year, month, day)

    Date(year, month, day)
  }.toOption
