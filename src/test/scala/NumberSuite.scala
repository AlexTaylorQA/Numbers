import org.scalatest.FunSuite

/**
  * Created by Administrator on 18/07/2017.
  */

class NumberSuite extends FunSuite
{

  test("Input a valid number.")
  {
    try(Main.printNum(12345))
  }

  test("Input an invalid, negative number.")
  {
    val error = intercept[Exception] {Main.printNum(-1)}
    assert(error.getMessage == "For input string: \"-\"")
  }

  test("Input an invalid string instead of a number.")
  {
    val error = intercept[Exception] {Main.printNum("abc".toLong)}
    assert(error.getMessage == "For input string: \"abc\"")
  }

}
