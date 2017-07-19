/**
  * Created by Administrator on 18/07/2017.
  */

import scala.collection.mutable.ListBuffer

object Main
{
  def main(args:Array[String]) =
  {
    startUp("Please input a number to be converted:   ")
  }

  def startUp(str:String):Unit =
  {
    try
    {
      println(str)
      val getNum = scala.io.StdIn.readLong()
      printNum(getNum)
      startUp("Please input a number to be converted:   ")
    }
    catch
    {
      case _ => startUp("\n- - - - -\n\nPlease input a valid whole number.   ")
    }
  }

  def printNum(y: Long) =
  {
    val numLength:Long = y.toString().length.toLong
    val numSet = y.toString.toCharArray
    val OGnumList = new ListBuffer[Long]()

    for (w <- 0 to (numLength - 1).toInt)
    {
      OGnumList += numSet(w).toString().toLong
    }

    val numList = OGnumList.reverse

    val list = List(
      ("thousand", "thousand"),
      ("million", "million"),
      ("billion", "milliard"),
      ("trillion", "billion"),
      ("quadrillion", "billiard"),
      ("quintillion", "trillion"),
      ("sextillion", "trilliard"))

    val numLoop = math.floor(numLength / 3).toLong

    buildOut(numLoop, numLength, numList, list, "", "", 0)

  }

  def buildOut(
                numLoop:Long,
                numLength:Long,
                numList:ListBuffer[Long],
                list:List[(String, String)],
                outStr:String,
                outStr2:String, z:Int): Unit = {

    z == (numLoop + 1) match {
      case true =>
        // end the loop
        val finalOut = outStr.replaceAll("( )+", " ").trim()
        val finalOut2 = outStr2.replaceAll("( )+", " ").trim()
        println("\nShort Form: " + finalOut + "\nLong Form: " + finalOut2 + "\n\n- - - - -\n")

      case false =>
        numLength >= (((z + 1) * 3) + 1) match {
          case true =>

            val theOut =
              list(z)._1 + " " +
                (
                  numList((z * 3) + 2).toString +
                  numList((z * 3) + 1).toString +
                  numList(z * 3).toString
                ).replaceFirst("^0*", "") + " " + outStr

            val theOut2 =
              list(z)._2 + " " +
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
                    val newOut = theOut.replace(list(z)._1, "")
                    val newOut2 = theOut2.replace(list(z)._2, "")
                    buildOut(
                      numLoop,
                      numLength,
                      numList,
                      list,
                      newOut,
                      newOut2,
                      z + 1
                    )

                  case false =>
                    buildOut(numLoop, numLength, numList, list, theOut, theOut2, (z + 1))

                }
              case false =>
                buildOut(numLoop, numLength, numList, list, theOut, theOut2, (z + 1))

            }

          case false =>
            val theOut = " " + outStr
            val theOut2 = " " + outStr2
            val v = (((z - 1) * 3) + 3)

            outExtra(numLoop, numLength, numList, list, theOut, theOut2, (z + 1), v)


        }

    }

  }

  def outExtra(
                numLoop:Long,
                numLength:Long,
                numList:ListBuffer[Long],
                list:List[(String,String)],
                theOut:String,
                theOut2:String,
                z:Int,
                v:Int):Unit =
  {
    v == numLength  match
    {
      case true =>
        buildOut(numLoop, numLength, numList, list, theOut, theOut2, z)

      case false =>
        val theNewOut = numList(v).toString() + theOut
        val theNewOut2 = numList(v).toString() + theOut2

        outExtra(numLoop, numLength, numList, list, theNewOut, theNewOut2, z, (v + 1))

    }

  }

}
