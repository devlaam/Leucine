import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

/* Build after:
 * https://github.com/portable-scala/sbt-crossproject
 */


/* Set to false for local publishing to exclude the demo files. */
val withDemo  = true

val sharedSettings = Seq(
  name                :=   "leucine",
  organization        :=   "s2a",
  version             :=   "0.1.0",
  scalaVersion        :=   "3.2.1",
  scalacOptions       ++=  Seq("-feature","-deprecation","-unchecked","-explain"),
  libraryDependencies +=   "com.lihaoyi" %%% "utest" % "0.8.1" % Test,
  testFrameworks      +=   new TestFramework("s2a.control.LeucineFramework"),
  Compile / excludeFilter := new FileFilter { def accept(f: File) = !withDemo && f.getPath.containsSlice("/demo/") }
  )

val jvmSettings = Seq(
  )

val jsSettings = Seq(
  scalaJSUseMainModuleInitializer := true,
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )

val nativeSettings = Seq(
  nativeLinkStubs := true
  )

lazy val leucine = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(sharedSettings)
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)
