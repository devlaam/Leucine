import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

/* Build after:
 * https://github.com/portable-scala/sbt-crossproject
 * https://docs.scala-lang.org/overviews/contributors/index.html
 */

import xerial.sbt.Sonatype._

/* Set to false for local publishing to exclude the demo files. */
val withDemo  = false

val publishSettings = Seq(
  name                   :=  "leucine",
  version                :=  "0.2.2",
  organization           :=  "com.sense2act",
  licenses               :=  Seq("MIT" -> url("https://opensource.org/license/mit/")),
  description            :=  "Small x-platform actor framework",
  publishTo              :=  sonatypePublishToBundle.value,
  publishMavenStyle      :=  true,
  sonatypeProjectHosting :=  Some(GitHubHosting("devlaam", "Leucine", "ruud@sense2act.com")),
  versionPolicyIntention :=  Compatibility.None
  )

val sharedSettings = Seq(
  scalaVersion            :=   "3.2.1",
  scalacOptions           ++=  Seq("-feature","-deprecation","-unchecked","-explain"),
  libraryDependencies     +=   "com.lihaoyi" %%% "utest" % "0.8.1" % Test,
  testFrameworks          +=   new TestFramework("s2a.control.LeucineFramework"),
  Compile / excludeFilter :=   new FileFilter { def accept(f: File) = !withDemo && f.getPath.containsSlice("/demo/") }
  )

val jvmSettings = Seq(
  assembly / assemblyJarName := "main.jar"
  )

val jsSettings = Seq(
  scalaJSUseMainModuleInitializer := true,
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )

val nativeSettings = Seq(
  /* The default mode is 'debug', to get smaller/faster code use: */
  //nativeMode      := "release-full"
  /* This setting is a requirement for uTest on Native */
  nativeLinkStubs := true,
  /* To ensure to tasks are put in determistic way on the custom main loop. */
  Test / parallelExecution := false,
  /* Remove test which cannot run Native platform.*/
  Test / testOptions := Seq(Tests.Filter(s => !s.endsWith("NN")))
  )

lazy val leucine = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(publishSettings)
  .settings(sharedSettings)
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)
