package module1
import scala.util.control.Breaks._

object App {

  def main(args: Array[String]): Unit = {

    def doomyFunc(a: String) = {
      Thread.sleep(1000)
      println(a)
    }

    val doomyFuncWithLoggingTime: String => Unit = hof.logRunningTime(doomyFunc)

    val v: PartialFunction[(Int, Int), Int] = {
      case (x, y) if y != 0 => x / y
    }

    val v2: PartialFunction[(Int, Int), Int] =
      new PartialFunction[(Int, Int), Int] {
        def isDefinedAt(v: (Int, Int)): Boolean = ???
        def apply(v: (Int, Int)): Int = ???
      }

    println(v.isDefinedAt(10, 1))
    println(v.isDefinedAt(10, 0))

    trait Printer {
      def print(str: String): Unit
    }

    val printer: Printer = str => println(str)

    printer.print("Hello")

//=========================================================
// examples with Options
    val opt1 = module1.opt.Option(5)
    val opt2 = module1.opt.Option("str")
    val opt3 = module1.opt.Option.None
    println(opt1.zip(opt2))
    println(opt1.zip(opt3))

    val opt4: opt.Option[Int] = module1.opt.Option(5)
    val opt5 = module1.opt.Option(-3)

    println(opt4.filter((v:Int) => v>0))
    println(opt5.filter((v:Int) => v>0))
    println(opt5.filter(_ => true))


    //=========================================================
    // examples with List
    val list = module1.list.List(5,3,1) //5 :: 3 :: Nil
    println(list)
    println(list.mkString(":::"))
    println(list.map(v => v+"0"))

    val list2 = module1.list.List(5,7,-3,0,-1,9,-10,11)
    println(list2.filter(v => v>=0))
    println(list2.filter(v => v>=0).incList)

//    val list3 = module1.list.List(5,7,"8",9)
//    println(list3.incList)
    val list4 = module1.list.List("s1","s2","3")
    println(list4.shoutString)
  }

}
