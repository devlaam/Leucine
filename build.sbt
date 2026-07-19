import BuildSupport.{Mode,compileExcluder}
import org.scalajs.linker.interface.ModuleKind

/* Build after:
 * https://github.com/portable-scala/sbt-crossproject
 * https://docs.scala-lang.org/overviews/contributors/index.html
 */

val latest  = "3.8.4"
val stable  = "3.3.8"

/* Select one of the possible compiler modes: Demo, Sona, Test or Wiki */
val mode: Mode = Mode.Demo

/* Select the version you want to compile with */
val compileWith = stable

version       :=  "0.7.0"
scalaVersion  :=  compileWith
usePipelining :=  false

val sharedSettings = Seq(
  name                    :=  "leucine",
  organization            :=  "com.sense2act",
  description             :=  "Small x-platform actor framework.",
  scalacOptions           ++= Seq("-feature","-deprecation","-unchecked","-explain","-Wunused:all","-Wnonunit-statement","-Wvalue-discard"),
  libraryDependencies     ++= Seq("com.lihaoyi" %% "utest" % "0.10.0-RC1" % Test).drop(Mode.Test.drop(mode)),
  libraryDependencies     ++= Seq("org.apache.logging.log4j" % "log4j-core" % "2.26.0").drop(Mode.Wiki.drop(mode)),
  testFrameworks          +=  new TestFramework("utest.runner.Framework"),
  Compile / excludeFilter := compileExcluder(mode)
  )

// We moeten positief gaan filteren op tests per platform.
// Of we moeten de tests die niet voor elk platform werken
// opnemen in een apparte tak. Lastige is een beetje dat sommige
// test voor twee platformen werken. We zouden de test objecten
// met _JVM_JS_NAT kunnen laten eindigen. Daar is gemakkelijk op
// te filteren en is simpel.

val coreDirectory = file("core").getCanonicalFile

val jvmSettings = Seq(
  assembly / assemblyJarName := "main.jar",
  //Test / testOptions := Seq(Tests.Filter(_.contains("_JVM")))
  Compile / unmanagedSourceDirectories += coreDirectory / "src" / "main" / "scala-mix",
  )

val jsSettings = Seq(
  scalaJSUseMainModuleInitializer := true,
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
  //Test / testOptions := Seq(Tests.Filter(_.contains("_JS")))
  ///* Remove test which cannot run on the JS-Emulated platform.*/
  Test / testOptions := Seq(Tests.Filter(s => !s.endsWith("NJS")))
  )

val nativeSettings = Seq(
  /* The default mode is 'debug', to get smaller/faster code use: */
  //nativeMode      := "release-full"
  /* This setting is a requirement for uTest on Native */
  //nativeLinkStubs := true,
  //Test / testOptions := Seq(Tests.Filter(_.contains("_NTV")))
  ///* Remove test which cannot run on the Native platform.*/
  Test / testOptions := Seq(Tests.Filter(s => !s.endsWith("NN"))),
  // uTest 0.9.5 and 0.10.0-RC1 depend on test-interface 0.5.8,
  // while Scala Native 0.5.12 selects test-interface 0.5.12.
  // Allow the eviction until uTest updates its dependency metadata.
  libraryDependencySchemes += ("org.scala-native" % "test-interface_native0.5_3" % VersionScheme.Always),
  Compile / unmanagedSourceDirectories += coreDirectory / "src" / "main" / "scala-mix",
  )

lazy val leucine = (projectMatrix in coreDirectory)
  /* To rename default leucine to leucineJVM */
  .defaultAxes(VirtualAxis.scalaABIVersion(compileWith))
  .settings(sharedSettings)
  .jvmPlatform(
    scalaVersions = Seq(compileWith),
    settings = jvmSettings,
  )
  .jsPlatform(
    scalaVersions = Seq(compileWith),
    settings = jsSettings,
  )
  .nativePlatform(
    scalaVersions = Seq(compileWith),
    settings = nativeSettings,
  )

lazy val leucineJVM    = leucine.jvm(compileWith)
lazy val leucineJS     = leucine.js(compileWith)
lazy val leucineNative = leucine.native(compileWith)
