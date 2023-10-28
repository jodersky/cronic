# cronic

Cron-like time parsing utilities for scala.

The most important class of this package is the `cronic.Schedule` class. It
represents a recurring event based on date and time filters. It can be parsed
from a cron-like/systemd-like string and used to predict future events.

## Example

(try it with `./mill cronic.jvm.console`)

```scala
// Create a schedule from a string (see apidoc for the syntax)
val schedule = cronic.Schedule.parse("*-*-1 20:0/15:00")

// predict future events
val now = java.time.LocalDateTime.of(2023, 10, 28, 0, 0, 0)
schedule.predictions(now).take(5).toList
// List(2023-11-01T20:00, 2023-11-01T20:15, 2023-11-01T20:30, 2023-11-01T20:45, 2023-12-01T20:00)
```

## Playground

There's an interactive application (based on ScalaJS) to generate schedules at
[jodersky.github.io/cronic](https://jodersky.github.io/cronic).

## Maven

This package is built for Scala 3 on the JVM, JS and Native.

```
ivy"io.crashbox::cronic::0.1.0"
```

**Note for JS and Native**: the end-user application (i.e. anything that
requires linking), will need to include and implementation of the `java.time`
package. It is recommended to also include the [scala-java-time
library](https://github.com/cquiroz/scala-java-time) in such projects.
