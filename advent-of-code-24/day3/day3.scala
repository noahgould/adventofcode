//> using dep com.lihaoyi::os-lib:0.11.3

import scala.util.matching.Regex

import os._
import scala.collection.mutable.ArrayBuilder
import scala.collection.mutable.ArrayBuffer


def loadInStr(): String = 
    val reportPath = os.pwd / "day3_input.txt"
    os.read(reportPath)

def findMulPairs(inputStr: String): List[(Int, Int)] =
    val mulPairRegex: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r
    val allMatches = mulPairRegex.findAllMatchIn(inputStr).toList
    allMatches.map(pair => (pair.group(1).toInt, pair.group(2).toInt))

def findPairsWithINstructions(inputStr: String): List[(Int, Int)] = 
    // split the string on do() and don't() - is it possible to split on both? 
    //or split one at a time, find the parts that are after a do, and before a don't
    // could get the indexes of do() and don't(), use that to create a new string after do()
    val doRegex = """.*do().*""".r
    val dontRegex = """.*don't().*""".r

    val doIndicies = doRegex.findAllMatchIn(inputStr).toList.map(m => m.start)
    val dontIndices = dontRegex.findAllMatchIn(inputStr).toList.map(m => m.start)

    val firstDoRange = (0, dontIndices.headOption.getOrElse(inputStr.length()))

    var doIndex = 0
    var dontIndex = 0

    val doRanges = ArrayBuffer[(Int, Int)]()

    var doing = true
    var startOfDo = 0
                // 0, 5....8....    
    // 123do()123dont()123do()123dont()do()123do()123do()123dont()345

    while (doIndex < doIndicies.length && dontIndex < dontIndices.length) 
        if doing then
            val curDont = dontIndices(dontIndex)
            if curDont > startOfDo then
                doRanges.addOne(startOfDo, curDont)
                doing = false
            dontIndex += 1
        else
            val curDo = doIndicies(doIndex)
            if curDo > doRanges(doRanges.size -1)._2 then
                doing = true
                startOfDo = curDo
            doIndex +=1
    
    val mulPairRegex: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r

    val inputInDo = doRanges.map[String](doRange => inputStr.slice(doRange._1, doRange._2))

    val fullInStr = inputInDo.fold("")((fullStr, x) => fullStr.appendedAll(x))

    findMulPairs(fullInStr)


def findPairsUsingDoSPlits(inputStr: String): List[(Int, Int)] =
    println("Input String:")
    println(inputStr)
    val piecesWithNoDont = inputStr.split("""don't()""")
    val firstPiece = piecesWithNoDont(0)
    println(s"first piece: $firstPiece")
    println("pieces no dont")
    piecesWithNoDont.foreach(p => println(p))

    val allGoodPieces = piecesWithNoDont.slice(1, piecesWithNoDont.length).map(p => p.split("do()")).filter(doSplit => doSplit.length > 1).map(doSplit => doSplit.slice(1, doSplit.length).fold("")((f, x) => f + x))

    println("all good pieces")

    println(s" first piece no don't: ${piecesWithNoDont(1)} ************* first piece good: ${allGoodPieces(0)}")

    val doString = firstPiece + allGoodPieces.fold("")((fullStr, x) => fullStr + x)
    println("do string")
    println(doString)
    findMulPairs(doString)



def sumAndMultiply(pairs: List[(Int, Int)]) = 
    pairs.foldLeft(0)((sum, p) => ( p._1 * p._2) + sum)

@main def mulFinder() = 
    val loadedInputStr = loadInStr()
    val pairsToMultiply = findPairsUsingDoSPlits(loadedInputStr)
    print(s"found paris: $pairsToMultiply")
    val multiplied = sumAndMultiply(pairsToMultiply)
    println(s"sum: $multiplied")