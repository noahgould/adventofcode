//> using dep com.lihaoyi::os-lib:0.11.3

import os._
import scala.annotation.varargs
import scala.collection.mutable.ArrayBuffer
import scala.util.chaining._

def parseReports(): List[String] = 
    val reportPath = os.pwd / "day2_input.txt"
    os.read.lines(reportPath).toList

def reportWithoutLevel(report: Seq[Int], levelIndex: Int): Seq[Int] =
    val firstPart: Seq[Int] = report.slice(0, levelIndex)
    val secondPart: Seq[Int] = report.slice(levelIndex + 1, report.length)
    val withoutLevel = firstPart ++ secondPart
    println(s"report without level: [$levelIndex] [$report] first: [$firstPart] second: [$secondPart] without level: [$withoutLevel] ")
    return withoutLevel

//example
// 1,2,3
// 1,6,7
// 7,5,7

//returns if its safe and if its increasing
def isSafeReport(report: Seq[Int], increasing: Option[Boolean]): (Boolean, Option[Boolean]) =
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

def checkReportAndSubReports(report: Seq[Int]): Boolean =
    val fullReportSafe = isSafeReport(report, None)._1
    if (fullReportSafe) then
        true
    else
        println(s"checking sub reports for report: $report")
        report.indices.exists(i => isSafeReport(reportWithoutLevel(report, i), None)._1)
        // println(s"got bad report count: $badReportCount")
        // badReportCount == 0

def countSafeLevels(reports: List[String]): Int =
    val intReports = reports.map(reportString => reportString.split(" ").map(_.toInt))
    intReports.count(report => checkReportAndSubReports(report))
    

@main def levelReporter() =
    val reports = parseReports()
    println(s"num of reports: ${reports.length}")
    // val lessReports = reports.take(5)
    // println(s" reports: $lessReports")
    val numSafeLevels = countSafeLevels(reports = reports)
    println(s"safe levels: $numSafeLevels")