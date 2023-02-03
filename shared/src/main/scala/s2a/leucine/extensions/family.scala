package s2a.leucine.extensions


import s2a.leucine.actors.*

/** Experiment to see how easy the user can extend the possibilities of the actors. */
trait FamilyChildExtra :
  /** The super type for the letters the childeren may receive. */
  type ChildLetter <: Actor.Letter

  /* The type of the sender. */
  type Sender

  /* Methods to extend. */
  protected def children: Map[String,BareActor[ChildLetter,?]]
  protected def relay(letter: ChildLetter, sender: Sender, include: String => Boolean, bounce: () => Unit): Unit
  protected def pass(letter: ChildLetter, sender: Sender, name: String, bounce: () => Unit): Unit

  /** Forward a message to all children */
  protected def relay(letter: ChildLetter, sender: Sender): Unit = relay(letter,sender,_ => true,() => ())

  /** Forward a message one specific child if it exists */
  protected def pass(letter: ChildLetter, sender: Sender, name: String): Unit = pass(letter,sender,name,() => ())

  /** Test if the actor has a child with this name. */
  protected def has(name: String): Boolean  = children.contains(name)

  /** Get the child actor with this name. if it exists. */
  protected def get(name: String): Option[BareActor[ChildLetter,?]] = children.get(name)
