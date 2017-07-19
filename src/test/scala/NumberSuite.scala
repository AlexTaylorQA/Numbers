import org.scalatest.FunSuite
import util._
import java.io.StringBufferInputStream
import scala.Console.setIn

/**
  * Created by Administrator on 18/07/2017.
  */

class NumberSuite extends FunSuite
{

  test("Input a valid number.")
  {
    Main.printNum(12345)
  }

  test("Input an invalid, negative number.")
  {
    val error = intercept[Exception] {Main.printNum(-1)}
    assert(error.getMessage === "For input string: \"-\"")
  }

  test("Input an invalid string instead of a number.")
  {
    val error = intercept[Exception] {Main.printNum("abc".toLong)}
    assert(error.getMessage === "For input string: \"abc\"")
  }

  test("Try to input a number with enough digits to trigger part of the buildOut function.")
  {
    Main.printNum(1234567890)
  }


  test("Try to input a number with a sequence of zeroes in the scope of \"thousand\", \"million\", etc.")
  {
    Main.printNum(1000000000.toLong)
  }

  test("Test Main method")
  {
    val theMsg = new StringBufferInputStream("1234567890987654321")
    setIn(theMsg)
    Main.main(Array())

  }

  test("Test startUp by feeding it a message and some input")
  {
    //val theInput = 123456789
    val theMsg = new StringBufferInputStream("1234567890987654321")
    setIn(theMsg)
    Main.startUp("", 0)
  }

  test("Test startUp by feeding it a message and some invalid input")
  {
    //val theInput = 123456789
    val theMsg = new StringBufferInputStream("abcdef")
    setIn(theMsg)
    Main.startUp("", 0)
  }

}
