name := "yabe-lift"

scalaVersion := "2.9.1"

seq(webSettings :_*)

scanDirectories in Compile := Nil 

unmanagedBase <<= baseDirectory { base => base / "custom_lib" }

scalacOptions += "-deprecation"

libraryDependencies ++= Seq(
  "net.liftweb" %% "lift-webkit" % "2.4-M4" % "compile->default",
  "net.liftweb" %% "lift-mapper" % "2.4-M4" % "compile->default",
  "net.liftweb" %% "lift-wizard" % "2.4-M4" % "compile->default",
  "net.liftweb" %% "lift-textile" % "2.4-M4" % "compile->default")

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.5" % "test->default",
  "org.eclipse.jetty" % "jetty-webapp" % "8.0.3.v20111011" % "container, test",
  "javax.servlet" % "servlet-api" % "2.5" % "provided->default",
  "com.h2database" % "h2" % "1.2.138",
  "ch.qos.logback" % "logback-classic" % "0.9.26" % "compile->default"
)
