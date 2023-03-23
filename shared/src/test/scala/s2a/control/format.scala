package s2a.control

class LeucineFramework extends utest.runner.Framework :
  /* Too quickly spot the relevant part */
  override def exceptionStackFrameHighlighter(s: StackTraceElement) = s.getClassName.contains("s2a.leucine")
