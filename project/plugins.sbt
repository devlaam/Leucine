// Comment to get more information during initialization
logLevel := Level.Warn

addSbtPlugin("com.eed3si9n"       % "sbt-assembly"                  % "1.2.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.2.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.2.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "1.12.0")
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.4.10")
