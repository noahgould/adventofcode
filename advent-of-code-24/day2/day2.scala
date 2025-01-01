//> using dep com.lihaoyi::os-lib:0.11.3

import os._
import scala.annotation.varargs
import scala.collection.mutable.ArrayBuffer

def parseReports(): List[String] = 
    val reportPath = os.pwd / "day2_input.txt"
    os.read.lines(reportPath).toList

def reportWithoutLevel(report: Array[Int], levelIndex: Int): Array[Int] =
    val firstPart = report.slice(0, levelIndex)
    val secondPart = report.slice(levelIndex + 1, report.length)
    return firstPart ++ secondPart

//example
// 1,2,3
// 1,6,7
// 7,5,7

//returns if its safe and if its increasing
def isSafeReport(report: Array[Int], increasing: Option[Boolean]): (Boolean, Option[Boolean]) =
    if (report.length < 2) then
            (true, increasing)
    else if report.length == 2 then
        var validLevelDifference = (report(0) - report(1)).abs <= 3 && (report(0) - report(1)).abs > 0
        var thisReportIncreasing = report(0) < report(1)
        increasing.fold((validLevelDifference, Some(thisReportIncreasing)))(shouldIncrease => ((validLevelDifference && ((thisReportIncreasing && shouldIncrease) || (!thisReportIncreasing && !shouldIncrease)), Some(shouldIncrease))))
    else 
        val mid = report.length / 2
        val (leftSafe, leftIncreasing) = isSafeReport(report.slice(0, mid), increasing) // true, None
        val (rightSafe, rightIncreasing) = isSafeReport(report.slice(mid, report.length), increasing) //true, None 

        val (validIncreasing, returnIncreasing) = (leftIncreasing, rightIncreasing) match {
            case (None, None) => (true, Some(report(0) < report(1)))
            case (None, x) => (true, x)
            case (x, None) => (true, x)
            case (Some(true), Some(true) ) => (true, Some(true))
            case (Some(false), Some(false)) => (true, Some(false))
            case _ => (false, None)
        }

        val (safeAtSplit, increaseAtSplit) = isSafeReport(report.slice(mid -1, mid+1), returnIncreasing)

        (leftSafe && rightSafe && validIncreasing && safeAtSplit && returnIncreasing == increaseAtSplit, returnIncreasing.orElse(increaseAtSplit))
    
    
        
        // var unsafeCount = 0

        // for i <- restOfLevels.indices if unsafeCount < allowedUnsafe do
        //     //check that it is increasing or decreasing according to rest of list
        //     //check that the increase or decrease is safe
        //     val currentLevel = restOfLevels(i)
        //     if currentLevel == lastLevel then
        //         unsafeCount +=1
        //         if !isSafeReport(reportWithoutLevel(restOfLevels, i), allowedUnsafe - unsafeCount) && !isSafeReport(reportWithoutLevel(restOfLevels, i-1), allowedUnsafe - unsafeCount) then
        //             unsafeCount +=1
        //     else if ((currentLevel > lastLevel) && !increasing) || ((currentLevel < lastLevel) && increasing) then
        //         unsafeCount +=1
        //         if !isSafeReport(reportWithoutLevel(restOfLevels, i), allowedUnsafe - unsafeCount) && !isSafeReport(reportWithoutLevel(restOfLevels, i-1), allowedUnsafe - unsafeCount) then
        //             unsafeCount +=1
        //     else if (currentLevel - lastLevel).abs > 3 then
        //         unsafeCount +=1
        //         if !isSafeReport(reportWithoutLevel(restOfLevels, i), allowedUnsafe - unsafeCount) && !isSafeReport(reportWithoutLevel(restOfLevels, i-1), allowedUnsafe - unsafeCount) then
        //             unsafeCount +=1
        //     else
        //         lastLevel = currentLevel

        // var isSafe = unsafeCount < allowedUnsafe
        // isSafe

def countSafeLevels(reports: List[String], allowedUnsafe: Int): Int =
    val intReports = reports.map(reportString => reportString.split(" ").map(_.toInt))
    val reportSafety = intReports.map(report => (isSafeReport(report, None)._1, report))
    reportSafety.count(_._1)

@main def levelReporter() =
    val reports = parseReports()
    println(s"num of reports: ${reports.length}")
    // val lessReports = reports.take(5)
    // println(s" reports: $lessReports")
    val numSafeLevels = countSafeLevels(reports = reports, allowedUnsafe = 2)
    println(s"safe levels: $numSafeLevels")