/**
  * Created by Administrator on 18/07/2017.
  */

import scala.collection.mutable.ListBuffer
import util._

object Main
{
  def main(args:Array[String]):Unit =
  {
    startUp("Please input a number to be converted:   ", 0)
  }

  def startUp(str:String, count:Int):Unit =
  {
    val doTry = Try(startNext(str))

    doTry match
    {
      case Success(a) => println("Success!\n")
      case Failure(a) =>
        count + 1 match
        {
          case x if x >= 3 =>  println("\n- - - - -\n\nToo many invalid attempts.\n")
          case x => startUp("\n- - - - -\n\nPlease input a valid whole number.   ", x)
        }
    }
  }

  def startNext(str:String):Unit =
  {
    println(str)
    val getNum = scala.io.StdIn.readLine().toLong
    printNum(getNum)
    startUp("Please input a number to be converted:   ", 0)
  }

  def printNum(y: Long):Unit =
  {
    val numLength:Long = y.toString().length.toLong
    val numSet = y.toString.toCharArray
    val OGnumList:ListBuffer[Long] = new ListBuffer()

    for (w <- 0 to (numLength - 1).toInt)
    {
      OGnumList += numSet(w).toString().toLong
    }

    val numList = OGnumList.reverse

    val longList = List(
      "thousand",
      "million",
      "milliard",
      "billion",
      "billiard",
      "trillion",
      "trilliard"
    )

    val shortList = List(
      "thousand",
      "million",
      "billion",
      "trillion",
      "quadrillion",
      "quintillion",
      "sextillion"
    )

    buildOut(numLength, numList, shortList, longList, "", "", 0)

  }

  def buildOut(
                numLength:Long,
                numList:ListBuffer[Long],
                shortList:List[String],
                longList:List[String],
                outStr:String,
                outStr2:String, z:Int): Unit = {

      val numLoop = math.floor(numLength / 3).toLong
      z match {
      case z if numLoop + 1 == z  =>
        // end the loop
        val finalOut = outStr.replaceAll("( )+", " ").trim()
        val finalOut2 = outStr2.replaceAll("( )+", " ").trim()
        println("\nShort Form: " + finalOut + "\nLong Form: " + finalOut2 + "\n\n- - - - -\n")

      case _ =>
        numLength >= (((z + 1) * 3) + 1) match {
          case true =>

            val theOut =
              shortList(z) + " " +
                (
                  numList((z * 3) + 2).toString +
                  numList((z * 3) + 1).toString +
                  numList(z * 3).toString
                ).replaceFirst("^0*", "") + " " + outStr

            val theOut2 =
              longList(z) + " " +
                (
                  numList((z * 3) + 2).toString +
                  numList((z * 3) + 1).toString +
                  numList(z * 3).toString()
                ).replaceFirst("^0*", "") + " " + outStr2

            numLength >= (((z + 1) * 3) + 4) match {
              case true =>
                (
                  numList((z * 3) + 5).toString +
                  numList((z * 3) + 4).toString +
                  numList((z * 3) + 3).toString
                ).replaceFirst("^0*", "").isEmpty match
                {
                  case true =>
                    val newOut = theOut.replace(shortList(z), "")
                    val newOut2 = theOut2.replace(longList(z), "")
                    buildOut(
                      numLength,
                      numList,
                      shortList,
                      longList,
                      newOut,
                      newOut2,
                      z + 1
                    )

                  case false =>
                    buildOut(numLength, numList, shortList, longList, theOut, theOut2, (z + 1))

                }
              case false =>
                buildOut(numLength, numList, shortList, longList, theOut, theOut2, (z + 1))

            }
          case false =>
            val theOut = " " + outStr
            val theOut2 = " " + outStr2
            val v = (((z - 1) * 3) + 3)

            outExtra(numLength, numList, shortList, longList, theOut, theOut2, (z + 1), v)
        }
    }
  }

  def outExtra(
                numLength:Long,
                numList:ListBuffer[Long],
                shortList:List[String],
                longList:List[String],
                theOut:String,
                theOut2:String,
                z:Int,
                v:Int):Unit =
  {
    v  match
    {
      case v if v == numLength =>
        buildOut(numLength, numList, shortList, longList, theOut, theOut2, z)

      case _ =>
        val theNewOut = numList(v).toString() + theOut
        val theNewOut2 = numList(v).toString() + theOut2

        outExtra(numLength, numList, shortList, longList, theNewOut, theNewOut2, z, (v + 1))

    }

  }

}
