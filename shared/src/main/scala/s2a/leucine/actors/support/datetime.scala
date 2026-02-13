package s2a.leucine.actors

/**
 * MIT License
 *
 * Copyright (c) 2023 Ruud Vlaming
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 **/

/* Discussion:
 * There is no cross platform way (JVM, JS and Native) to calculate the date string from the
 * number of epoch seconds. For JVM there is LocalDateTime with .ofEpochSecond with UTC time zone
 * for JS you can use Date with getUTC methods, but Native requires an external library (java.time).
 * Since we use this for logging only, thus positive values, we can use a simplified form of
 * Howard Hinnant algoritm. See https://howardhinnant.github.io/date_algorithms.html */

/**
 * This class requires the time in nano seconds and produces the usual fields for
 * year,month,day,hour,minute,second. Further it produces nano,micro and milli seconds.
 * The latter are whole fields, so then nanos also include de mirco and milli second
 * values. Likewise for the micro seconds. Note: works only for positive timestamps!
 * Based upon https://howardhinnant.github.io/date_algorithms.html. */
class DateTime(epochTimeInNanoSeconds: Long) :
    /* Required constants for this calculation. */
    inline val zeroToEpochDays = 719468L
    inline val daysInEra       = 146097L
    inline val daysInYear      = 365L
    inline val daysIn4Years    = 1461L
    inline val secsPerDay      = 86400L
    inline val daysIn100Years  = 36524L
    inline val secsInMin       = 60L
    inline val secsInHour      = 3600L
    inline val metric3Step     = 1000L
    inline val nanosPerSec     = metric3Step * metric3Step * metric3Step

    /* From epoch nano seconds to epochSeconds */
    private val epochSeconds: Long = epochTimeInNanoSeconds / nanosPerSec
    /* From epochSeconds to days (since 1970) */
    private val days: Long  = epochSeconds / secsPerDay
    /* The remainder number of seconds in the day */
    private val sod: Long  = epochSeconds % secsPerDay
    /* The remainder number of seconds in the minute */
    private val mod: Long  = sod % secsInHour
    /*  shift the epoch from 1970-01-01 to 0000-03-01 */
    private val dsz: Long  = days + zeroToEpochDays
    /* compute the era from the serial date by dividing by the number of days in an 400 year era */
    private val era: Long  = dsz / daysInEra
    /* The days left in this era */
    private val doe: Long  = dsz % daysInEra
    /* Find the year in this era, with all required boundary corrections. */
    private val yoe: Long  = (doe - doe / (daysIn4Years-1) + doe / daysIn100Years - doe / (daysInEra-1)) / daysInYear
    /* Find the year number (still starting at 1 march). */
    private val ynr: Long  = yoe + era * 400L
    /* Find the date of year by subtracting from the day-of-era the days that have occurred in all prior years of this era */
    private val doy: Long  = (doe - (daysInYear * yoe + yoe / 4 - yoe / 100))
    /* Finds the zero based month part staring at march. */
    private val mp: Long   = ( (5 * doy + 2) / 153 )

    /* Find the one based day of the month */
    val day: Int    = (doy - (153 * mp + 2) / 5 + 1).toInt
    /* Finds the one based month part staring at january. */
    val month: Int  = (mp + (if mp < 10 then 3 else -9)).toInt
    /* Correct the year for the rebase from march to january. */
    val year: Int   = (ynr + (if month <= 2 then 1 else 0)).toInt

    /* Find hour, min and second now. */
    val hour: Int   = (sod / secsInHour).toInt
    val minute: Int = (mod / secsInMin).toInt
    val second: Int = (mod % secsInMin).toInt

    /* Find the parts of the remaining second. */
    val nano: Int   = (epochTimeInNanoSeconds % nanosPerSec).toInt
    val micro: Int  = (nano  / metric3Step).toInt
    val milli: Int  = (micro / metric3Step).toInt
