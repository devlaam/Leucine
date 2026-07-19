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

val srcDir = file("src").getCanonicalFile
def compDir(loc: String) = srcDir / cmpDir / loc
def testDir(loc: String) = srcDir / tstDir / loc

val jvmSettings = Seq(
  assembly / assemblyJarName := "main.jar",
  //Test / testOptions := Seq(Tests.Filter(_.contains("_JVM")))
  Compile / unmanagedSourceDirectories := Seq(srcALL,srcJVM,srcMIX).map(compDir),
  Test / unmanagedSourceDirectories := Seq(srcALL,srcJVM).map(testDir),
  )

val jsSettings = Seq(
  scalaJSUseMainModuleInitializer := true,
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
  Compile / unmanagedSourceDirectories := Seq(srcALL,srcJS).map(compDir),
  Test / unmanagedSourceDirectories := Seq(srcALL,srcJS).map(testDir),
  )

val ntvSettings = Seq(
  /* The default mode is 'debug', to get smaller/faster code use: */
  //nativeMode      := "release-full"
  /* uTest 0.9.5 and 0.10.0-RC1 depend on test-interface 0.5.8,
   * while Scala Native 0.5.12 selects test-interface 0.5.12.
   * Allow the eviction until uTest updates its dependency metadata. */
  libraryDependencySchemes += ("org.scala-native" % "test-interface_native0.5_3" % VersionScheme.Always),
  Compile / unmanagedSourceDirectories := Seq(srcALL,srcNTV,srcMIX).map(compDir),
  Test / unmanagedSourceDirectories := Seq(srcALL,srcNTV).map(testDir),
  )

lazy val leucine = (projectMatrix in srcDir)
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
    settings = ntvSettings,
  )

lazy val leucineJVM = leucine.jvm(compileWith)
lazy val leucineJS  = leucine.js(compileWith)
lazy val leucineNTV = leucine.native(compileWith)
