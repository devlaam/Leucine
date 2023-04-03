import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

/* Build after:
 * https://github.com/portable-scala/sbt-crossproject
 * https://docs.scala-lang.org/overviews/contributors/index.html
 */

/* Set to 1 for publishing to exclude the demo/test files. */
val publishMe: Int = 0

ThisBuild / version       :=  "0.3.0"
ThisBuild / scalaVersion  :=  "3.2.1"

val sharedSettings = Seq(
  name                    :=  "leucine",
  organization            :=  "com.sense2act",
  description             :=  "Small x-platform actor framework.",
  scalacOptions           ++= Seq("-feature","-deprecation","-unchecked","-explain"),
  libraryDependencies     ++= Seq("com.lihaoyi" %%% "utest" % "0.8.1" % Test).drop(publishMe),
  testFrameworks          +=  new TestFramework("s2a.control.LeucineFramework"),
  Compile / excludeFilter :=  new FileFilter { def accept(f: File) = (publishMe==1) && (f.getPath.containsSlice("/demo/") || f.getPath.containsSlice("/test/")) },
  )

val jvmSettings = Seq(
  assembly / assemblyJarName := "main.jar"
  )

val jsSettings = Seq(
  scalaJSUseMainModuleInitializer := true,
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
  /* Remove test which cannot run on the JS-Emulated platform.*/
  Test / testOptions := Seq(Tests.Filter(s => !s.endsWith("NJS")))
  )

val nativeSettings = Seq(
  /* The default mode is 'debug', to get smaller/faster code use: */
  //nativeMode      := "release-full"
  /* This setting is a requirement for uTest on Native */
  nativeLinkStubs := true,
  /* Remove test which cannot run on the Native platform.*/
  Test / testOptions := Seq(Tests.Filter(s => !s.endsWith("NN")))
  )

lazy val leucine = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(sharedSettings)
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)
