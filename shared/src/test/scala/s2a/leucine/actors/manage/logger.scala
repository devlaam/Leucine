package s2a.leucine.actors


import scala.util.Random
import scala.collection.mutable.Buffer
import utest.*


object ActorLoggerSortTest extends TestSuite :

  import LogHolder.Hold
  import ActorLogger.{Level, Entry, Timing, Channel}

  val tests = Tests :

    test("ActorLogger.sort sorts randomly dispersed two-level lists") :
      /* The number of top level lists in the Holder */
      val groupCount = 6
      /* The number of entries created */
      val entryCount = 24
      /* An index based collection with empty entry lists to be filled. */
      val groups = Array.fill[List[ActorLogger.Entry]](groupCount)(Nil)
      /* Method to add an entries on the second level in a random group. nr is just a counter to keep them apart. */
      def fillGroup(nr: Int): Entry =
        val entry = ActorLogger.Entry("parent.child",Level.Info,Timing.Recent,Channel.Pass,Static.Class,s"Class$nr",s"msg-$nr")
        val group = Random.nextInt(groups.length)
        groups(group) = entry :: groups(group)
        entry
      /* Construct the group and build a linear collection of all entries produced. */
      val entries = (1 to entryCount).map(fillGroup)
      /* Find min and max index of the produced entries. */
      val min = entries.map(_.index).min
      val max = entries.map(_.index).max
      /* Make the holder with the random list and correct parameters. */
      val dispersed: Hold[List[ActorLogger.Entry]] = Hold(min,groups.toList,max)
      /* Do the sorting. */
      val sorted = ActorLogger.sort(dispersed)
      /* Test is we did not corrupt the number of entries.  */
      sorted.length ==> dispersed.width
      /* Now see if each entry reached the correct slot. O(N^2)*/
      entries.foreach : entry =>
        val slot = (entry.index - min).toInt
        sorted(slot) ==> entry

    test("ActorLogger.sort on Hold.empty returns empty array") :
      val sorted = ActorLogger.sort(Hold.empty[List[ActorLogger.Entry]])
      sorted.length ==> 0

    test("ActorLogger.sort handles hold with only empty inner lists") :
      val hold = Hold[List[ActorLogger.Entry]](10L,List(Nil,Nil,Nil),12L)
      val sorted = ActorLogger.sort(hold)
      sorted.length ==> 3
      sorted.forall(_ == null) ==> true

    test("ActorLogger.sort ignores empty inner lists between populated lists") :
      val first  = ActorLogger.Entry("parent.child",Level.Warn,Timing.Recent,Channel.Pass,Static.Class,"ClassA","first")
      val second = ActorLogger.Entry("parent.child",Level.Warn,Timing.Recent,Channel.Pass,Static.Class,"ClassB","second")
      val min = math.min(first.index,second.index)
      val max = math.max(first.index,second.index)
      val hold = Hold[List[ActorLogger.Entry]](min,List(List(second),Nil,List(first),Nil),max)
      val sorted = ActorLogger.sort(hold)
      sorted((first.index-min).toInt) ==> first
      sorted((second.index-min).toInt) ==> second


object ActorLoggerStichTest extends TestSuite :

  import ActorLogger.{Entry, Level, Timing, Channel}
  import LogHolder.{Hold, Store}

  /**
   * Build a sequence of hold instances where each hold is mostly dense in the center,
   * while some values near the end (and occasionally one in the inner part) are postponed
   * to a next hold. The final hold contains all remaining entries. */
  private def buildHolds(all: Seq[Entry], rnd: Random): Seq[Hold[List[Entry]]] =

    /* An empty sequence of entries */
    val eseq = Seq.empty[Entry]

    /* Disperse entries among lists of lists of entries at random. */
    def randomGroups(entries: Seq[Entry]): List[List[Entry]] =
      val groups = Array.fill[List[Entry]](rnd.nextInt(8)+4)(Nil)
      entries.foreach : entry =>
        val index = rnd.nextInt(groups.length)
        groups(index) = entry :: groups(index)
      groups.toList

    /* Construct a holder for a list of entries correctly. */
    def makeHold(entries: Seq[Entry]): Hold[List[Entry]] =
      if entries.isEmpty then Hold.empty else
        val sorted = entries.sortBy(_.index)
        val min = sorted.head.index
        val max = sorted.last.index
        Hold[List[Entry]](min,randomGroups(sorted.reverse),max)

    /* Split the sequence randomly in two, with increasing chance to carry the more we are a the and*/
    def split(size: Int): ((Seq[Entry],Seq[Entry]),Entry) => (Seq[Entry],Seq[Entry]) =
      case ((carry,done),entry) =>
        if 4*rnd.nextInt(size) > done.size
        then (carry,done :+ entry)
        else (carry :+ entry,done)

    def loop(pending: Seq[Entry], acc: Seq[Hold[List[Entry]]]): Seq[Hold[List[Entry]]] =
      /* If pending is empty we have processed everything, return the result. */
      if      pending.isEmpty      then acc
      /* If pending is almost empty then do not split anymore, add the rest and return */
      else if pending.length <= 20 then (acc :+ makeHold(pending))
      /* Otherwise we cut off a front piece and randomize the content. */
      else
        /* Cut some piece of the pending entries for processing */
        val frontSize = math.min(pending.length, rnd.between(12,32))
        val frontData = pending.take(frontSize)
        /* Split the sequence randomly in two, with increasing chance to carry the more we are at the end. */
        val (carry,done) = frontData.foldLeft((eseq,eseq))(split(frontSize))
        /* Handle the remaining entries */
        loop(carry ++ pending.drop(frontSize),acc :+ makeHold(done))

    loop(all,Seq.empty)


  private def reorder(holds: Seq[Hold[List[Entry]]], maxArraySize: Int): (Seq[Entry],Seq[Int]) =
    /* Construct environment for testing. */
    val processed = Buffer.empty[Entry]
    val storeSizes = Buffer.empty[Int]
    var store = Store.empty
    /* Start the reorder process. */
    holds.zipWithIndex.foreach :
      case (hold,index) =>
        val completed = index == holds.length - 1
        store = ActorLogger.stichedSpool(hold,store,maxArraySize,completed,entry => processed += entry)
        storeSizes += store.entries.size
    /* Return the processed values and the list of all store sizes. */
    (processed.toSeq,storeSizes.toSeq)

  val sampleSize = 1000

  val tests = Tests :

    test("stichedSpool composes random hold stream into dense sorted output") :
      (1 to 100).foreach : seed =>
        /* Construct a list of log entries, the content is not important. */
        val allEntries = (1 to sampleSize-1).map : nr =>
          ActorLogger.Entry("parent.child",Level.Info,Timing.Recent,Channel.Pass,Static.Method,s"Source.$nr",s"message-$nr")
        /* Randomize the them into a two level holder list. */
        val holds = buildHolds(allEntries,Random(seed))
        /* Check if there is something there */
        holds.nonEmpty ==> true
        /* Process the entries with a large storage, we expect a neat ordered and dense result. */
        val (processedLarge,storeSizesLarge) = reorder(holds,sampleSize)
        /* Process the entries with a small storage, we expect some disorder, but all the entries must be there. */
        val (processedSmall,storeSizesSmall) = reorder(holds,sampleSize/20)
        /* See if all entries are present and sorted for the Large one  */
        processedLarge ==> allEntries.toSeq
        /* See if all entries are present for the small one.  */
        if (processedSmall.size != allEntries.size) then println(processedSmall.mkString("processedSmall = [",",","]"))
        processedSmall.size ==> allEntries.size
        /* See if there are no left overs: */
        storeSizesLarge.last ==> 0
        storeSizesSmall.last ==> 0
        /* See if the storeSizes never exceed the maximal arraySize */
        storeSizesLarge.foreach(size => assert(size <= sampleSize))
        storeSizesSmall.foreach(size => assert(size <= sampleSize/20))
