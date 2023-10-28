package cronic

import scala.collection.mutable.ListBuffer

private[cronic] class Parser(input0: String):
  import Parser.*

  private var token: Token = Token.Eof
  private var buffer: String = ""

  private var idx = 0
  private def nextToken(): Unit =
    while idx < input0.length && input0(idx).isSpaceChar do
      idx += 1

    if idx >= input0.length then
      token = Token.Eof
    else
      val c = input0(idx)
      c match
        case '*' =>
          token = Token.`*`
          idx += 1
        case ',' =>
          token = Token.`,`
          idx += 1
        case '-' =>
          token = Token.`-`
          idx += 1
        case ':' =>
          token = Token.`:`
          idx += 1
        case '.' =>
          idx += 1
          if idx >= input0.length || input0(idx) != '.' then
            token = Token.Bad
          else
            token = Token.`..`
            idx += 1
        case '/' =>
          token = Token.`/`
          idx += 1
        case d if d.isDigit =>
          val start = idx
          while idx < input0.length && input0(idx).isDigit do
            idx += 1
          token = Token.Number
          buffer = input0.substring(start, idx)
        case d if d.isLetter =>
          val start = idx
          while idx < input0.length && input0(idx).isLetter do
            idx += 1
          token = Token.Str
          buffer = input0.substring(start, idx)
        case _ =>
          token = Token.Bad
          buffer = input0(idx).toString
          idx += 1
  end nextToken

  private def accept(tok: Token): Boolean =
    if token == tok then
      nextToken()
      true
    else
      false

  private def tokenError(expected: String) =
    throw new IllegalArgumentException(s"unexpected token: expected $expected, found $token")

  def parse(): Schedule =
    var yearFilter: List[Filter] = Nil
    var monthFilter: List[Filter] = Nil
    var dayFilter: List[Filter] = Nil

    var hourFilter: List[Filter] = List(Filter(0,-1,0))
    var minuteFilter: List[Filter] = List(Filter(0,-1,0))
    var secondFilter: List[Filter] = List(Filter(0,-1,0))

    var dayOfWeekBitset = 0xffffffff

    nextToken()

    if token == Token.Str then
      dayOfWeekBitset = parseDayOneDayOfWeek()

    var firstWasTime = false
    var first = parseComponent()
    if token == Token.`-` then
      nextToken()
      val second = parseComponent()
      if token == Token.`-` then
        nextToken()
        dayFilter = parseComponent()
        monthFilter = second
        yearFilter = first
      else
        dayFilter = second
        monthFilter = first
    else if token == Token.`:` then
      nextToken()
      firstWasTime = true
      val second = parseComponent()
      if token == Token.`:` then
        nextToken()
        secondFilter = parseComponent()
        minuteFilter = second
        hourFilter = first
      else
        minuteFilter = second
        hourFilter = first
    else
      tokenError("expected - or :")

    if token != Token.Eof then
      first = parseComponent()
      if token == Token.`:` && !firstWasTime then
        nextToken()
        val second = parseComponent()
        if token == Token.`:` then
          nextToken()
          secondFilter = parseComponent()
          minuteFilter = second
          hourFilter = first
        else
          minuteFilter = second
          hourFilter = first
      else
        if firstWasTime then
          tokenError("eof")
        else
          // expected time
          tokenError("time component")

    for comp <- monthFilter do
      require(1 <= comp.start && comp.start <= 12)
      require(comp.stop < 0 || 1 <= comp.stop && comp.stop <= 12)

    for comp <- dayFilter do
      require(1 <= comp.start && comp.start <= 31)
      require(comp.stop < 0 || 1 <= comp.stop && comp.stop <= 31)

    for comp <- hourFilter do
      require(0 <= comp.start && comp.start <= 59)
      require(comp.stop < 0 || 0 <= comp.stop && comp.stop <= 59)

    for comp <- minuteFilter do
      require(0 <= comp.start && comp.start <= 59)
      require(comp.stop < 0 || 0 <= comp.stop && comp.stop <= 59)

    for comp <- secondFilter do
      require(0 <= comp.start && comp.start <= 59)
      require(comp.stop < 0 || 0 <= comp.stop && comp.stop <= 59)

    Schedule(yearFilter, monthFilter, dayFilter, hourFilter, minuteFilter, secondFilter, dayOfWeekBitset)
  end parse

  private def parseDayOneDayOfWeekIdx(): Int =
    if token != Token.Str then
      tokenError("string")

    val r = buffer.toLowerCase() match
      case "mon" => 0
      case "tue" => 1
      case "wed" => 2
      case "thu" => 3
      case "fri" => 4
      case "sat" => 5
      case "sun" => 6
      case _ =>
        tokenError("valid day of week")
      nextToken()
    r

  private def parseDayOfWeekRange(): Int =
    val first = parseDayOneDayOfWeekIdx()
    val end =
      if token == Token.`..` then
        nextToken()
        parseDayOneDayOfWeekIdx()
      else
        first

    var n = 0
    for i <- first to end do
      n |= (1 << i)
    n

  private def parseDayOneDayOfWeek(): Int =
    var n = parseDayOfWeekRange()
    while token == Token.`,` do
      n |= parseDayOfWeekRange()
    n

  private def parseComponentNonEmpty(): Filter =
    val start = buffer.toInt
    nextToken()

    val end =
      if accept(Token.`..`) then
        if token != Token.Number then tokenError("number")
        val e = buffer.toInt
        nextToken()
        e
      else -1

    val step =
      if accept(Token.`/`) then
        if token != Token.Number then tokenError("number")
        val r = buffer.toInt
        nextToken()
        r
      else
        0

    Filter(start, end, step)

  private def parseComponent(): List[Filter] =
    if accept(Token.`*`) then
      Nil
    else if token == Token.Number then
      val items = ListBuffer.empty[Filter]
      items += parseComponentNonEmpty()
      while token == Token.`,` do
        nextToken()
        items += parseComponentNonEmpty()
      items.result()
    else
      tokenError("* or a number")

private[cronic] object Parser:
  enum Token:
    case Bad
    case Eof
    case Number
    case Str
    case `*`
    case `,`
    case `-` // date separator
    case `:` // time separator
    case `..` // range separator
    case `/` // step separator
