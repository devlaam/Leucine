package s2a.leucine.wiki.Shared

// This file is to be included in every Scastie instance that needs it

/* Class to make this demo work in Scastie for fixed commands
 * as well as from the command line. */
class ConsoleReader(echo: Boolean, cmd: String, cmds: String *) :
  /* Stack holds the list of remaining commands */
  private var stack = cmds.prepended(cmd)
  /* Read the next command from the stack. */
  private def readFromStack: String =
    val head = stack.head
    if stack.size > 1 then stack = stack.tail
    if echo then println(head)
    head
  private var _manual = false
  /* See if the last read was a manual input. */
  def manual: Boolean = _manual
  /* Call this to get a new command */
  def readln: String =
    if echo then print("? ")
    val read = io.StdIn.readLine()
    _manual = read != null
    if _manual then read else readFromStack


