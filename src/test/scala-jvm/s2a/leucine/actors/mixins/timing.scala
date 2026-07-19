package s2a.leucine.actors


import utest.*
import s2a.control.Helpers

object TimingAidTestEmulation extends TestSuite, TimingAidTest(using Helpers.emulatedContext)
