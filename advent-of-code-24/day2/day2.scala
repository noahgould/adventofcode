//> using dep com.lihaoyi::os-lib:0.11.3

import os._
import scala.annotation.varargs

def parseReports(): List[String] = 
    val reportPath = os.pwd / "day2" / "day2_input.txt"
    os.read.lines(reportPath).toList

def countSafeLevels(reports: List[String]): Int =
    reports.map(reportString => reportString.split(" ").map(_.toInt)).count(report => {
        if (report.length < 2) then
            true
        else 
            var lastLevel = report.head.toInt
            var restOfLevels = report.slice(1, report.length)
            var increasing = (restOfLevels.head.toInt - lastLevel) > 0
            
            var unsafeCount = 0

            for i <- restOfLevels.indices if unsafeCount < 2 do
                //check that it is increasing or decreasing according to rest of list
                //check that the increase or decrease is safe
                val currentLevel = restOfLevels(i)

                if currentLevel == lastLevel then
                    unsafeCount += 1
                else if ((currentLevel > lastLevel) && !increasing) || ((currentLevel < lastLevel) && increasing) then
                    unsafeCount += 1
                else if (currentLevel - lastLevel).abs > 3 then
                    unsafeCount += 1
                else
                    lastLevel = currentLevel

            unsafeCount < 2
    })

@main def levelReporter() =
    val reports = parseReports()
    println(s"num of reports: ${reports.length}")
    // val lessReports = reports.take(5)
    // println(s" reports: $lessReports")
    val numSafeLevels = countSafeLevels(reports = reports)
    println(s"safe levels: $numSafeLevels")