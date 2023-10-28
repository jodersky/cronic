package cronic

import java.time.DateTimeException
import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset

/** A component of a schedule's field
  *
  * n      => Comp(n, n, 0)
  * n..m   => Comp(n, m, 1)
  * n/s    => Comp(n, -1, s)
  * n..m/s => Comp(n, m, s)
  *
  * Invariants:
  * - min < start < max
  * - stop < max
  * - step >= 0
  *
  */
case class Filter(start: Int, stop: Int, step: Int):
  require(step >= 0)


/** Specification of a recurring event, based on set of date and time filters.
  *
  * A schedule is considered to happen at a point in time when all its filters
  * are satisfied.
  *
  * Schedules work up to a precision of seconds.
  *
  * Notes:
  * - an empty filter is equivalent to '*' in cron
  * - contrary to POSIX cron, if weekday and day are set then both must be
  *   fulfilled
  */
case class Schedule(
  yearFilter: List[Filter],
  monthFilter: List[Filter],
  dayFilter: List[Filter],
  hourFilter: List[Filter],
  minuteFilter: List[Filter],
  secondFilter: List[Filter],
  dayOfWeekBitset: Int = 0
):
  import Schedule.findNextField

  override def toString =
    val b = StringBuilder()

    def printComp(c: Filter) =
      b.append(c.start)
      if c.stop != -1 then
        b ++= ".."
        b.append(c.stop)
      if c.step > 1 then
        b += '/'
        b.append(c.step)

    def printFilter(filter: List[Filter]) =
      if filter.isEmpty then b ++= "*"
      else
        printComp(filter.head)
        for c <- filter.tail do
          b ++= ","
          printComp(c)

    printFilter(yearFilter)
    b += '-'
    printFilter(monthFilter)
    b += '-'
    printFilter(dayFilter)
    b += ' '
    printFilter(hourFilter)
    b += ':'
    printFilter(minuteFilter)
    b += ':'
    printFilter(secondFilter)
    b.result()

  /** Find the next time that this schedule will match.
    *
    * @throws IllegalArgumentException if no suitable time could be found. This
    * can happen if the schedule is in the past, or no matching time in the
    * future could be predicted after multiple attempts.
    */
  def next(now: LocalDateTime = LocalDateTime.now()): LocalDateTime =
    var year = now.getYear()
    var month = now.getMonth().getValue()
    var day = now.getDayOfMonth()
    var hour = now.getHour()
    var minute = now.getMinute()
    var second = now.getSecond()

    var next: LocalDateTime = null

    // check if the current date is valid, and if so set the next field
    def isValid() =
      try
        next = LocalDateTime.of(
          year,
          month,
          day,
          hour,
          minute,
          second
        )
        true
      catch
        case _: Exception => false

    // try to find a valid next date
    def attemptNext(): Boolean =
      val nextYear = findNextField(yearFilter, year)
      if nextYear > year then
        year = nextYear
        month = 1
        day = 1
        hour = 0
        minute = 0
        second = 0
      else if nextYear < year then
        throw IllegalArgumentException("this schedule happened in the past")

      val nextMonth = findNextField(monthFilter, month)
      if nextMonth > month then
        month = nextMonth
        day = 1
        hour = 0
        minute = 0
        second = 0
      if nextMonth < month || !isValid() then
        year += 1
        month = 1
        day = 1
        hour = 0
        minute = 0
        second = 0
        return false

      val nextDay = findNextField(dayFilter, day)
      if nextDay > day then
        day = nextDay
        hour = 0
        minute = 0
        second = 0
      if nextDay < day || !isValid() then
        month += 1
        day = 1
        hour = 0
        minute = 0
        second = 0
        return false

      // make sure week day is allowed
      val dow: Int = next.getDayOfWeek().getValue() - 1 // 0-6
      if ((1 << dow) & dayOfWeekBitset) == 0 then
        try
          next = next.plusDays(1)
          year = next.getYear()
          month = next.getMonth().getValue()
          day = next.getDayOfMonth()
          hour = 0
          minute = 0
          second = 0
        catch
          case _: DateTimeException =>
        return false

      val nextHour = findNextField(hourFilter, hour)
      if nextHour > hour then
        hour = nextHour
        minute = 0
        second = 0
      if nextHour < hour || !isValid() then
        day += 1
        hour = 0
        minute = 0
        second = 0
        return false

      val nextMinute = findNextField(minuteFilter, minute)
      if nextMinute > minute then
        minute = nextMinute
        second = 0
      if nextMinute < minute || !isValid() then
        hour += 1
        minute = 0
        second = 0
        return false

      val nextSecond = findNextField(secondFilter, second)
      if nextSecond > second then
        second = nextSecond
      if nextSecond < second || !isValid() then
        minute += 1
        second = 0
        return false

      // isValid has been called and thus set the next datetime
      true
    end attemptNext

    val Max = Schedule.MaxIterations
    var i = 0
    while i < Max && !attemptNext() do i += 1
    if i >= Max then throw IllegalArgumentException(s"no matching date found after ${Max} attempts")
    else next
  end next

  /** Predict the next time that all filters of this schedule will be satisfied.
    *
    * @param now the current date and time
    *
    * @return None if the schedule is in the past, or no time could be
    * predicted. Otherwise, the next time that the schedule will take place.
    */
  def predict(now: LocalDateTime = LocalDateTime.now()): Option[LocalDateTime] =
    try
      Some(next(now))
    catch
      case _: IllegalArgumentException => None

  /** Predict the next time that all filters of this schedule will be satisfied,
    * as an instant in UTC. */
  def predictUtc(now: Instant = Instant.now()): Option[Instant] =
    val zone = ZoneOffset.UTC
    val nextDt = predict(LocalDateTime.ofInstant(now, zone))
    nextDt.map(_.toInstant(zone))

  /** The stream of all future predictions. */
  def predictions(now: LocalDateTime = LocalDateTime.now()): LazyList[LocalDateTime] =
    LazyList.unfold(now){ n =>
      predict(n).map(t => t -> t.plusSeconds(1))
    }

  /** The stream of all future predictions, as instants in UTC. */
  def predictionsUtc(now: Instant = Instant.now()): LazyList[Instant] =
    LazyList.unfold(now){ n =>
      predictUtc(n).map(t => t -> t.plusSeconds(1))
    }

object Schedule:
  private val MaxIterations = 1000

  // round up x to the next multiple of y
  private def roundUp(x: Int, y: Int) = ((x + y - 1) / y) * y

  /** Find the earliest value after or equal to now that is possible according
    * to fields's Filter.
    *
    * Return -1 if there are no more possible values after or equal to now
    *
    */
  private def findNextField(field: List[Filter], now: Int): Int =
    if field == Nil then return now

    var next = -1
    for comp <- field do
      if comp.start >= now then
        if next == -1 || comp.start < next then
          next = comp.start
      else if comp.step > 0 then
        val k = comp.start + roundUp(now - comp.start, comp.step)
        if (next == -1 || k < next) && (comp.stop < 0 || k <= comp.stop) then
          next = k
    next

  /** Parse a string as a schedule.
    *
    * The syntax is similar to that of systemd calendar specifications (`man
    * systemd.time`).
    *
    * E.g. `Mon..Fri *-5/2-1 *:20,34:00`
    *
    * meaning:
    *
    * - from monday to friday (inclusive), and
    * - any year, and
    * - from may and every second month thereafter, and
    * - any hour, and
    * - minute 20 and 34, and
    * - second 0
    *
    *
    * The schedule is parsed in three space-separated parts:
    *
    * 1. a day-of-week filter
    * 2. a date filter
    * 3. a time filter
    *
    * At least one of the of the parts must be specified, and they must be
    * specified in order.
    *
    * If a part is omitted, the following filters are assumed:
    *
    * - day-of-week part => any
    * - date part => any
    * - time part => 00:00:00
    *
    * In other words, if the day-of-week part is omitted, any day of week is
    * valid, if the date part is omitted then any date is valid, and if the time
    * part is omitted then only midnight is valid.
    *
    * In the date filter, if no year is specified, then any year matched. I.e.
    * `10-2` is equivalent to `*-10-2`
    *
    * In the time filter, if no second is specified, then second 0 is matched.
    * I.e. `20:30` is equivalent to `20:30:00`
    *
    * A filter may be either `*`, or a disjunction of numbers or a range. `*` is
    * the wildcard filter which matches anything. A disjunction is separated by
    * `,` and any of the numbers or ranges must match for the schedule to
    * happen. A range is given as `start[..step][/step]` (inclusive).
    *
    * ## Examples
    *
    * Example       | Expanded                  | Explanation
    * --------------|---------------------------|------------
    * `Mon..Fri`    | `Mon..Fri *-*-* 00:00:00` | Every monday to friday at midnight
    * `10-2`        | `Mon..Sun *-10-2 00:00:00` | Every 10th October to friday at midnight
    * `2023-10-28`  | `Mon..Sun 2023-10-2 00:00:00` | On 2023-10-2 at midnight (this happens only once)
    * `20:30`       | `Mon..Sun *-*-* 20:30:00` | Every day at 20:30
    * `20:30:15`    | `Mon..Sun *-*-* 20:30:15` | Every day at 20:30:15
    * `*-*-1/2`     | `Mon..Sun *-*-1/2 00:00:00` | On the first day of the month and every second day after (1st, 3rd, 5th etc...)
    * `Tue *-*-1 `  | `Tue *-*-1 00:00:00` | On the first day of the month, but only if it's a Tuesday
    * `*:20,40`     | `Mon..Sun *-*-* *:20:40` | At twenty past and forty past every hour.
    *
    *
    * ## Specification in extended BNF
    *
    * ```
    * schedule       ::= [dayofweekspec] [datespec] [timespec]
    * dayofweekspec  ::= dayofweekrange{,dayofweekrange}
    * dayofweekrange ::= dayofweek[..dayofweek]
    * dayofweek      ::= 'Mon' | 'Tue' | 'Wed' | 'Thu' | 'Fri' | 'Sat' | 'Sun'
    * datespec       ::= comps-comps | comps-comps-comps // either month-day or year-month-day
    * timespec       ::= comps:comps | comps:comps:comps // either hour:minute of hour:minute:second
    * comps          ::= comp{,comp}
    * comp           ::= '*' | filter
    * filter         ::= int[..int][/int]
    *```
    *
    * @throws IllegalArgumentException if the specification was invalid
    */
  def parse(str: String): Schedule =
    Parser(str).parse()

  /** Try to parse a string as a schedule, or return None if it was invalid.
    * @see parse
    */
  def tryParse(str: String): Option[Schedule] =
    try
      Some(parse(str))
    catch
      case _: IllegalArgumentException => None
