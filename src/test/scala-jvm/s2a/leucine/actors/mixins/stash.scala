package s2a.leucine.actors


import utest.*
import s2a.control.Helpers

object StashAidTestEmulation extends TestSuite, StashAidTest(using Helpers.emulatedContext)
