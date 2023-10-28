import mill._, scalalib._, scalajslib._, scalanativelib._, publish._

object cronic extends Module {

  trait Publish extends PublishModule {
    def publishVersion = "0.1.0"
    def pomSettings = PomSettings(
      description = "cronic",
      organization = "io.crashbox",
      url = "https://github.com/jodersky/cronic",
      licenses = Seq(License.MIT),
      versionControl = VersionControl.github("jodersky", "cronic"),
      developers = Seq(
        Developer("jodersky", "Jakob Odersky", "https://github.com/jodersky")
      )
    )
  }

  trait CronicModule extends ScalaModule with Publish {
    def millSourcePath = super.millSourcePath / os.up
    def scalaVersion = "3.2.2"
    object test extends ScalaTests {
      def testFramework = "utest.runner.Framework"
      def ivyDeps = Agg(
        ivy"com.lihaoyi::utest::0.8.1"
      )
    }
    def scalacOptions = Seq("-deprecation", "-release", "8")
    def artifactName = "cronic"
  }

  object jvm extends CronicModule

  object js extends CronicModule with ScalaJSModule {
    def scalaJSVersion = "1.14.0"
  }

  object native extends CronicModule with ScalaNativeModule {
    def scalaNativeVersion = "0.4.13"
  }

}

object site extends ScalaModule with ScalaJSModule {
  def scalaVersion = "3.2.2"
  def scalaJSVersion = "1.14.0"
  def moduleDeps = Seq(cronic.js)
  def ivyDeps = Agg(
    ivy"org.scala-js::scalajs-dom::2.2.0",
    ivy"io.github.cquiroz::scala-java-time::2.5.0",
    ivy"com.lihaoyi::scalatags::0.12.0"
  )

  def index = T.source(millSourcePath / "index.html")
  def root: T[PathRef] = T {
    os.copy(index().path, T.dest / "index.html")
    os.copy(fastLinkJS().dest.path / "main.js", T.dest / "cronic.js")
    PathRef(T.dest)
  }
}
