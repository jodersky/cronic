import cronic.Schedule
import org.scalajs.dom
import scalajs.js
import scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("updateSchedule")
def update(spec: String, elem: dom.Node) =
  Schedule.tryParse(spec) match
    case None => elem.innerText = "invalid schedule"
    case Some(sched) =>
      import scalatags.JsDom.all.*
      val newElem = frag(
        div("Schedule " + sched.toString),
        div(
          "Next 5 predictions",
          ul(
            for t <- sched.predictionsUtc().take(5).toList yield
              li(t.toString)
          )
        )
      ).render

      while elem.hasChildNodes() do elem.removeChild(elem.lastChild)
      elem.appendChild(newElem)
