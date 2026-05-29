package s2a.leucine.actors

import utest.*


object HolderTest extends TestSuite :

  import LogHolder.Hold
  import ActorLogger.{Level,Filter}

  val tests = Tests :

    test("Hold.empty starts empty") :
      val hold = Hold.empty[Int]
      hold.entries.isEmpty ==> true
      hold.width ==> 0
      hold.min ==> LogHolder.minStart
      hold.max ==> LogHolder.maxStart

    test("width spans complete index range") :
      Hold[Int](10L,List(10,11,12),12L).width ==> 3
      Hold[Int](7L,List(9),9L).width ==> 3

    test("adding to grouped hold prepends when non-empty") :
      val left = Hold[Int](5L,List(1,2),7L)
      val right = Hold[List[Int]](9L,List(List(9)),11L)
      val merged = left + right

      merged.min ==> 5L
      merged.max ==> 11L
      merged.entries ==> List(List(1,2),List(9))

    test("adding empty hold leaves grouped hold untouched") :
      val empty = Hold.empty[Int]
      val right = Hold[List[Int]](20L,List(List(3,4)),22L)
      (empty + right) ==> right

    test("Filter.Pass filter always returns true") :
      Filter.Pass.onSource(Level.Error,Static.Class,"main.work")  ==> true
      Filter.Pass.onActor(Level.Error,"main.leaf") ==> true
      Filter.Pass.onMessage(Level.Trace,"troubles")  ==> true


    test("Hold.merge ignores empty holds") :
      val hold1 = Hold.empty[String]
      val hold2 = Hold(1, List(List("a")), 1)
      val merged = hold1 + hold2
      hold1.width ==> 0
      merged.min ==> 1
      merged.max ==> 1
      merged.entries ==> List(List("a"))



object LogHolderTest extends TestSuite :

  import ActorLogger.{Level, Timing, Channel, Capture, Filter}
  import Static.Kind
  import LogHolder.Hold

  val tests = Tests :

    test("new holder starts empty with zero incidents") :
      val logHolder = new LogHolder("/user/test",() => Level.Trace,Level.Warn,() => Timing.Recent)
      logHolder.isEmpty ==> true
      logHolder.getIncidents ==> 0

    test("make creates entries and counts incidents") :
      val logHolder = new LogHolder("parent.child",() => Level.Trace,Level.Warn,() => Timing.Millis)
      val incident  = logHolder.make(Capture(Level.Error,Channel.Pass,Filter.Pass,Static.Method,"/main/myclass/method1","Troubles"))
      val regular   = logHolder.make(Capture(Level.Info,Channel.Pass,Filter.Pass,Static.Class,"/main/myclass/method2","All is fine"))
      /* See if there is just one incident. */
      logHolder.getIncidents ==> 1
      /* Test the entry contents for construction errors. */
      incident.actorName   ==> "parent.child"
      incident.level       ==> Level.Error
      incident.timing      ==> Timing.Millis
      incident.sourceKind  ==> Static.Method
      incident.sourcePath  ==> "/main/myclass/method1"
      incident.message     ==> "Troubles"
      /* Test the entry contents for construction errors. */
      regular.actorName    ==> "parent.child"
      regular.level        ==> Level.Info
      regular.sourceKind   ==> Static.Class
      regular.sourcePath   ==> "/main/myclass/method2"
      regular.message      ==> "All is fine"
      /* See if no entries were added in make */
      logHolder.get.entries.isEmpty ==> true

    test("pass uses passLevel and actor-path filter") :
      var dynamicPassLevel: Level = Level.Info
      val logHolder = new LogHolder("parent.child1",() => dynamicPassLevel,Level.Warn,() => Timing.Recent)
      object Filter1 extends Filter :
        final def onSource(level: Level, kind: Kind, path: String) = true
        final def onActor(level: Level, path: String)              = (level != Level.Fatal) && (path == "parent.child1")
        final def onMessage(level: Level, msg: => String)          = true
      object Filter2 extends Filter :
        final def onSource(level: Level, kind: Kind, path: String) = true
        final def onActor(level: Level, path: String)              = (level != Level.Fatal) && (path == "parent.child2")
        final def onMessage(level: Level, msg: => String)          = true
      object Block extends Filter :
        final def onSource(level: Level, kind: Kind, path: String) = false
        final def onActor(level: Level, path: String)              = false
        final def onMessage(level: Level, msg: => String)          = false
      def capture(level: Level, filter: Filter) = Capture(level,Channel.Pass,filter,Static.Method,"/main/myclass/method1","Troubles")
      def fixPass(result: Boolean): Filter = if result then Filter.Pass else Block
      /* Test the pass method on fixed filters at various levels. */
      logHolder.pass(capture(Level.Warn,Filter1))   ==> true
      logHolder.pass(capture(Level.Warn,Filter2))   ==> false
      logHolder.pass(capture(Level.Debug,Filter1))  ==> false
      logHolder.pass(capture(Level.Debug,Filter2))  ==> false
      logHolder.pass(capture(Level.Fatal,fixPass(true)))  ==> true
      logHolder.pass(capture(Level.Fatal,fixPass(false))) ==> false
      logHolder.pass(capture(Level.Info,fixPass(true)))  ==> true
      logHolder.pass(capture(Level.Info,fixPass(false))) ==> false
      logHolder.pass(capture(Level.Beta,fixPass(true)))  ==> false
      logHolder.pass(capture(Level.Beta,fixPass(false))) ==> false
      /* Test the pass method on external filter change at various levels. */
      dynamicPassLevel = Level.Trace
      logHolder.pass(capture(Level.Debug,Filter1)) ==> true
      logHolder.pass(capture(Level.Debug,Filter2)) ==> false
      logHolder.pass(capture(Level.Fatal,Filter1)) ==> false
      logHolder.pass(capture(Level.Fatal,Filter2)) ==> false
      logHolder.pass(capture(Level.Trace,fixPass(true)))  ==> true
      logHolder.pass(capture(Level.Trace,fixPass(false))) ==> false

    test("get returns hold bounds and clear resets holder") :
      val logHolder = new LogHolder("parent.child",() => Level.Trace,Level.Warn,() => Timing.Recent)
      val entry1 = logHolder.make(Capture(Level.Warn,Channel.Pass,Filter.Pass,Static.Object,"classA.objectA","first"))
      val entry2 = logHolder.make(Capture(Level.Info,Channel.Pass,Filter.Pass,Static.Class,"classA.classB","second"))
      val entry3 = logHolder.make(Capture(Level.Trace,Channel.Pass,Filter.Pass,Static.Method,"classA.methodC","third"))
      val entry4 = logHolder.make(Capture(Level.Error,Channel.Pass,Filter.Pass,Static.Class,"classA.classC","forth"))
      val entry5 = logHolder.make(Capture(Level.Beta,Channel.Pass,Filter.Pass,Static.Object,"classA.objectB","fifth"))
      /* Add a few entries*/
      logHolder.add(entry1)
      logHolder.add(entry2)
      logHolder.add(entry3)
      logHolder.add(entry4)
      logHolder.add(entry5)
      /* See what have become of them */
      val hold = logHolder.get
      hold.entries ==> List(entry5,entry4,entry3,entry2,entry1)
      hold.min ==> math.min(entry1.index,entry5.index)
      hold.max ==> math.max(entry1.index,entry5.index)
      hold.width ==> (hold.max - hold.min + 1).toInt
      logHolder.isEmpty ==> false
      /* See if clear works */
      logHolder.clear()
      logHolder.isEmpty ==> true
      logHolder.get ==> Hold.empty[ActorLogger.Entry]
      /* Incident count is lifetime-based and must survive clear(). */
      logHolder.getIncidents ==> 2
