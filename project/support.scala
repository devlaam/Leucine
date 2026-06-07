import java.io.File
import sbt.io.FileFilter

/** Contains helper functions for the build.sbt file */
object BuildSupport {
  /* Note: We must still use Scala 2 syntax here  */

  /* names we use for the excluder. */
  private val demo = "demo"
  private val sona = "sona"
  private val test = "test"
  private val wiki = "wiki"

  sealed trait Mode {
    def name: String
    def block: List[String]
    def filter(f: File): Boolean = block.exists(s => f.getPath.contains(s"/$s/"))
    def drop(mode: Mode): Int = if (mode.block.contains(this.name)) 1 else 0 }

  /* These are the possible modes for compilation */
  object Mode {
    /* Mode for pushing and releasing to github. */
    object Demo extends Mode { val name = demo;  val block = List(wiki) }
    /* Mode for publishing to Sonatype repository. */
    object Sona extends Mode { val name = sona;  val block = List(demo,test,wiki) }
    /* Mode to make compiling quicker when we mainly want to test. */
    object Test extends Mode { val name = test;  val block = List(demo,wiki) }
    /* Mode if we are developing for new examples on wiki. */
    object Wiki extends Mode { val name = wiki;  val block = List(demo,test) } }

  def compileExcluder(mode: Mode) = new FileFilter
  { def accept(f: File): Boolean = mode.filter(f) } }
