package s2a.leucine.wiki.FamilyAid

// Scastie: https://scastie.scala-lang.org/devlaam/NGjR6QN3QxC2b0GeupO8ng

import s2a.leucine.actors.*

given ActorContext = ActorContext.system

/* Actor that recursively enters its structure to create and investigate itself.
 * The root of the actor structure has no parent, therefore the parent is optional in this case. */
class Tree(name: String, val parent: Option[Tree]) extends AcceptActor(Tree,name), FamilyTree[Tree] :

  /* Write the results of this actor to the console. */
  private def write(action: String) = println(s"$action $path")

  /* New children must be created with their parent as parameter.*/
  private def newChild(i: Int) = Tree(s"F$i",Some(this))

  /* Handle the incoming letters. */
  final protected def receive(letter: Letter, sender: Sender): Unit = letter match
    /* This message creates <width> new children for this actor. */
    case Tree.Create(width,level) =>
      /* Create 'width' number of new children. */
      (1 to width).foreach(newChild)
      /* In case we are not yet on the last level, relay this creation order
       * to the next level. */
      if (level > 1) then relayAll(Tree.Create(width,level - 1),this):Unit
    /* This message will travel forward through the tree structure. */
    case Tree.Forward =>
      /* Report that we are in the forward traversal. */
      write("=>>")
      /* Relay the message to all children, and see if we succeeded. */
      val relayed = relayAll(Tree.Forward,this)
      /* In case there were no children to accept the message, we are at the
       * end of the structure and start the traversal backwards. */
      if relayed == 0 then parent.foreach(_ ! Tree.Backward)
    /* This message will travel backward through the tree structure. */
    case Tree.Backward =>
      /* Report that we are in the backward traversal. */
      write("<<=")
      /* If we still have a parent, continue the backward traversal. */
      parent.foreach(_ ! Tree.Backward)

  /* Report that the actor was created. */
  write("Created")

/* Actor is of the type Accept without managed State */
object Tree extends AcceptDefine :
  sealed trait Letter extends Actor.Letter[Actor]
  /* Message to create the tree structure. The maximal number of levels
   * is given by depth, the number of actors created in each level given
   * by width. */
  case class  Create(width: Int, depth: Int) extends Letter
  /* Message to traverse the tree in forward direction. */
  case object Forward extends Letter
  /* Message to traverse the tree in backwards direction. */
  case object Backward extends Letter

/* Main entry point for the demo. */
object Main :
  /* We must provide a default sender. */
  given Actor.Anonymous = Actor.Anonymous
  /* Create the root of the tree. */
  private val tree = new Tree("F0",None)

  def main(args: Array[String]): Unit =
    /* Create the all branches of the tree*/
    tree ! Tree.Create(2,3)
    /* Give it some time to complete */
    Thread.sleep(200)
    /* Send a message upwards. */
    tree ! Tree.Forward
    /* Give it some time to complete */
    Thread.sleep(200)
