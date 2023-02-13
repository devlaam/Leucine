package s2a.control

class LeucineFramework extends utest.runner.Framework :
  /* Too quickly spot the relevant part */
  override def exceptionStackFrameHighlighter(s: StackTraceElement) = s.getClassName.contains("s2a.leucine")
  //TODO: remove some more of the unnecessary parts of the output.
  //But postpone this until we have the tests ready and see how testing under JS & native plays out.

