import cronic.Schedule
import java.time.LocalDateTime
import utest.*

object GeneralTest extends TestSuite:
  val tests = Tests{
    test("schedule") {
      Schedule.parse("*-1,5/3-2 0:50..56/2").predictions(LocalDateTime.of(2023, 10, 26, 14, 0, 1)).take(10).toList ==>
        List(
          LocalDateTime.of(2023, 11, 2, 0, 50),
          LocalDateTime.of(2023, 11, 2, 0, 52),
          LocalDateTime.of(2023, 11, 2, 0, 54),
          LocalDateTime.of(2023, 11, 2, 0, 56),
          LocalDateTime.of(2024, 1, 2, 0, 50),
          LocalDateTime.of(2024, 1, 2, 0, 52),
          LocalDateTime.of(2024, 1, 2, 0, 54),
          LocalDateTime.of(2024, 1, 2, 0, 56),
          LocalDateTime.of(2024, 5, 2, 0, 50),
          LocalDateTime.of(2024, 5, 2, 0, 52)
        )
    }
    test("feb29th") {
      Schedule.parse("*-2-29").predictions(LocalDateTime.of(1890, 10, 26, 0, 0,0)).take(4).toList ==>
        List(
          LocalDateTime.of(1892, 2, 29, 0, 0, 0),
          LocalDateTime.of(1896, 2, 29, 0, 0, 0),
          LocalDateTime.of(1904, 2, 29, 0, 0, 0),
          LocalDateTime.of(1908, 2, 29, 0, 0, 0)
        )
    }
    test("schedule2") {
      Schedule.parse("Fri 2/2-*").predictions(LocalDateTime.of(2023, 10, 26, 0, 0,0)).take(11).toList ==>
        List(
          LocalDateTime.of(2023, 10, 27, 0, 0, 0),
          LocalDateTime.of(2023, 12, 1, 0, 0, 0),
          LocalDateTime.of(2023, 12, 8, 0, 0, 0),
          LocalDateTime.of(2023, 12, 15, 0, 0, 0),
          LocalDateTime.of(2023, 12, 22, 0, 0, 0),
          LocalDateTime.of(2023, 12, 29, 0, 0, 0),
          LocalDateTime.of(2024, 2, 2, 0, 0, 0),
          LocalDateTime.of(2024, 2, 9, 0, 0, 0),
          LocalDateTime.of(2024, 2, 16, 0, 0, 0),
          LocalDateTime.of(2024, 2, 23, 0, 0, 0),
          LocalDateTime.of(2024, 4, 5, 0, 0, 0)
        )

    }
  }
