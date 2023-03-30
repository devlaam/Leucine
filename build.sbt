import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

/* Build after:
 * https://github.com/portable-scala/sbt-crossproject
 * https://docs.scala-lang.org/overviews/contributors/index.html
 */

/* Set to false for publishing to exclude the demo files. */
val withDemo  = false

ThisBuild / version       :=  "0.2.3"
ThisBuild / scalaVersion  :=  "3.2.1"

val sharedSettings = Seq(
  name                    :=  "leucine",
  organization            :=  "com.sense2act",
  description             :=  "Small x-platform actor framework.",
  versionPolicyIntention  :=  Compatibility.None,
  scalacOptions           ++= Seq("-feature","-deprecation","-unchecked","-explain"),
  libraryDependencies     +=  "com.lihaoyi" %%% "utest" % "0.8.1" % Test,
  testFrameworks          +=  new TestFramework("s2a.control.LeucineFramework"),
  Compile / excludeFilter :=  new FileFilter { def accept(f: File) = !withDemo && f.getPath.containsSlice("/demo/") }
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
  .settings(sharedSettings)
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)
