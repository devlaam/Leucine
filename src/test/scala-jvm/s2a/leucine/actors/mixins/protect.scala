package s2a.leucine.actors


import utest.*
import s2a.control.Helpers

object ProtectAidTestEmulation extends TestSuite, ProtectAidTest(using Helpers.emulatedContext)
