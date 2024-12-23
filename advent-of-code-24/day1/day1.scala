

//> using dep com.lihaoyi::os-lib:0.11.3

import os._
import geny._

//parse file to read lists
//first num is first list, second num is second list
//sort the lists
// subtract num 1 from num 2, take the abs
// sum up all those

def parseLists(): List[String] =
    val twoListFilePath: os.Path = os.pwd / "input.txt"
    os.read.lines(twoListFilePath).toList

def getSortedLists(listFile: List[String]): (List[Int], List[Int]) =
    val allLocations: List[Array[Int]] = listFile.map(l => l.split("   ").map(_.toInt))
    val leftLocations = allLocations.map(a => a(0)).sorted
    val rightLocations = allLocations.map(a => a(1)).sorted

    val sortLeft = leftLocations.sorted
    val sortRight = rightLocations.sorted

    (sortLeft, sortRight)

def findDistance(leftList: List[Int], rightList: List[Int]): Int =

    val distances = (leftList zip rightList).map((l, r) => (l - r).abs)
    distances.sum


def findSimilarityScore(left: List[Int], right: List[Int]): Int =
//two ways to do it: two pointer way or hash map way
//two pointer way: go through the left list one at a time. for each number, check in right list how many times it appears.
//move it back to the start of this iteration each time in case a number is repeated
//since it is sorted, once we move past a number, we can move the pointer forward
//or, do some memoization in case of repeat numbers so we don't have to traverse each time

//hash map way: get counts of all the numbers in the right list and make a hash map of num to how many times it appears.

//two pointer + memoization sounds best

    var lastLeftNum = left(0) - 1
    var lastSimilarity = -1

    var rightPointer = 0

    val similarities = left.map(l => {
        if l == lastLeftNum then 
            lastSimilarity
        else
            var rOccurrences = 0
            while rightPointer < right.length && right(rightPointer) < l do
                rightPointer += 1
            while  rightPointer < right.length && right(rightPointer) == l do
                rightPointer += 1
                rOccurrences += 1
            
            lastSimilarity = rOccurrences * l
            lastLeftNum = l
            lastSimilarity
    })
    similarities.sum


@main def hello(): Unit =
    println("starting to read the lists")
    val sortedLists = getSortedLists(parseLists())

    // val leftShortList = sortedLists._1.take(5)
    // val rightShortList = sortedLists._2.take(5)
    // println(s"Chopped the lists. Left: [$leftShortList] right: [$rightShortList]")

    val distances = findDistance(sortedLists._1, sortedLists._2)
    val similarity = findSimilarityScore(sortedLists._1, sortedLists._2)
    println(s"got the summed distanc: $distances")
    println(s"got the similarity: $similarity")


