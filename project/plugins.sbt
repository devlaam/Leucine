// Comment to get more information during initialization
logLevel := Level.Warn

addSbtPlugin("com.eed3si9n"       % "sbt-assembly"                  % "2.1.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "1.13.2")
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.4.15")
addSbtPlugin("ch.epfl.scala"      % "sbt-version-policy"            % "2.1.3")
