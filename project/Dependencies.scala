import sbt._

object Dependencies {
  lazy val catsVersion      = "1.0.1"
  lazy val effectVersion    = "0.9"
  lazy val mouseVersion     = "0.16"
  lazy val fs2Version       = "0.10.1"
  lazy val e3dbVersion      = "2.2.0"
  lazy val shapelessVersion = "2.3.3"

  lazy val scalaTest  = "org.scalatest" %% "scalatest"   % "3.0.3"
  lazy val catsCore   = "org.typelevel" %% "cats-core"   % catsVersion
  lazy val catsMacros = "org.typelevel" %% "cats-macros" % catsVersion
  lazy val catsKernel = "org.typelevel" %% "cats-kernel" % catsVersion
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % effectVersion
  lazy val mouse      = "org.typelevel" %% "mouse"       % mouseVersion
  lazy val fs2Core    = "co.fs2"        %% "fs2-core"    % fs2Version
  lazy val fs2Io      = "co.fs2"        %% "fs2-io"      % fs2Version
  lazy val shapeless  = "com.chuusai"   %% "shapeless"   % shapelessVersion

  // Java dependencies
  lazy val e3db = "com.tozny.e3db" % "e3db-client-plain" % e3dbVersion
}
