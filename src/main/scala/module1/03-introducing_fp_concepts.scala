package module1

import java.util.UUID

import scala.annotation.tailrec
import java.time.Instant

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.language.postfixOps



/**
 * referential transparency
 */
 object referential_transparency{


  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification

  object Notification{
    case class Email(email: String, text: Html) extends Notification
    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService{
    def sendNotification(notification: Notification): Unit
    def createNotification(abiturient: Abiturient): Notification
  }

  trait AbiturientService{

    def registerAbiturient(abiturientDTO: AbiturientDTO, uuid: String): Abiturient
  }

  class AbiturientServiceImpl(notificationService: NotificationService) extends AbiturientService{

    override def registerAbiturient(abiturientDTO: AbiturientDTO, uuid: String): Abiturient = {
      val abiturient = Abiturient(uuid, abiturientDTO.email, abiturientDTO.fio)
      notificationService.sendNotification(Notification.Email(abiturient.email, "Some message"))
      abiturient
    }

  }
}


 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

   def fact(n: Int): Int = {
       var _n = 1
       var i = 2
       while(i <= n){
           _n *=  i
           i += 1
       }
       _n
   }


   def factRec(n: Int): Int = {
    if (n<=0) 1 else n * factRec(n-1)
   }


   def factTailRec(n: Int): Int = {
       @tailrec
       def loop(n: Int, accum: Int): Int =
           if(n == 1) accum
           else loop(n - 1, n * accum)
        loop(n, 1)
   }




  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */


}

object hof{

   trait Consumer{
       def subscribe(topic: String): LazyList[Record]
   }

   case class Record(value: String)

   case class Request()

   object Request {
       def parse(str: String): Request = ???
   }

   def createRequestSubscription() = {
       val cons: Consumer = ???

       val stream: LazyList[Record] = cons.subscribe("request")

       stream.foreach{ rec =>
            val req: Request = Request.parse(rec.value)
            // save(request)
       }
   }

   def createSubscription[T](topic: String, action: Record => T): Unit = {
       val cons: Consumer = ???

       val stream: LazyList[Record] = cons.subscribe(topic)
       stream.foreach{ rec =>
            action(rec)
       }
   }
   def createRequestSubscription2 = createSubscription("request", r => {
       val req: Request = Request.parse(r.value)
            // save(request)
   })


  // обертки

  def logRunningTime[A, B](f: A => B): A => B = { a =>
        val start = System.currentTimeMillis()
        val result = f(a)
        val end = System.currentTimeMillis()
        println(end - start)
        result
  }






  // изменение поведения ф-ции

  val arr = Array(1, 2, 3, 4, 5)

  def isOdd(i: Int): Boolean = i % 2 > 0



  def not[A](f: A => Boolean): A => Boolean = a => ! f(a)


  lazy val isEven = not(isOdd)




  // изменение самой функции

  // Follow type implementation

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def sum(x: Int, y: Int): Int = x + y

  val res: Int => Int = partial(1, sum)

  res(3)


}






/**
 *  Реализуем тип Option
 */


 object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

   sealed trait Option[+T]{
       def isEmpty: Boolean = this match {
           case Option.Some(v) => false
           case Option.None => true
       }

       def get: T = this match {
           case Option.Some(v) => v
           case Option.None => throw new Exception("Get on empty option")
       }

       def map[B](f: T => B): Option[B] = this match {
           case Option.Some(v) => Option.Some(f(v))
           case Option.None => Option.None
       }

       def flatMap[B](f: T => Option[B]): Option[B] = this match {
           case Option.Some(v) => f(v)
           case Option.None => Option.None
       }

       def printIfAny: Unit = this match {
          case Option.Some(v) => println(v)
          case Option.None =>
        }

      def zip[B](b: Option[B]): Option[(T,B)] = this match {
        case Option.Some(v1) => {
          b match {
            case Option.Some(v2) => Option.Some((v1,v2))
            case Option.None => Option.None
          }
        }
        case Option.None => Option.None
      }

      def filter(p: T => Boolean):Option[T] = this match {
        case Option.Some(v) if p(v) => this
        case _ => Option.None
      }

   }

   object Option{
        case class Some[T](v: T) extends Option[T]
        case object None extends Option[Nothing]

        def apply[T](v: T): Option[T] = if(v == null) Option.None else Option.Some(v)


   }





  /**
   *
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */


  /**
   *
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */


  /**
   *
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */

 }

 object list {

   /**
    *
    * Реализовать односвязанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
    */

   sealed trait List[+T] {

     def ::[A >: T](elem: A): List[A] = new ::(elem, this)

     def mkString(dev: String): String = {
       @tailrec
       def addToString(acc: StringBuilder, v: List[T]): StringBuilder = {
         v match {
           case ::(head, tail) => addToString(acc.append(head).append(dev), tail)
           case _ => acc.replace(acc.length() - dev.length, acc.length(), "")
         }
       }

       addToString(new StringBuilder(), this).toString()
     }

     def map[R](f: T => R): List[R] = {
       @scala.annotation.tailrec
       def rec(listOld: List[T], listNew: List[R]): List[R] = listOld match {
         case ::(head, tail) => rec(tail, new ::(f(head), listNew))
         case Nil => listNew
       }

       rec(this, Nil).reverse()
     }

     def reverse(): List[T] = {
       @scala.annotation.tailrec
       def rec(listOld: List[T], listNew: List[T]): List[T] = listOld match {
         case ::(head, tail) => rec(tail, new ::(head, listNew))
         case Nil => listNew
       }

       rec(this, Nil)
     }

     def filter(f: T => Boolean): List[T] = {
       @scala.annotation.tailrec
       def rec(listOld: List[T], listNew: List[T]): List[T] = listOld match {
         case ::(head, tail) if f(head) => rec(tail, new ::(head, listNew))
         case ::(head, tail) => rec(tail, listNew)
         case Nil => listNew
       }

       rec(this, Nil).reverse()
     }

     def incList: List[Int] = {
       @scala.annotation.tailrec
       def rec(listOld: List[T], listNew: List[Int]): List[Int] = listOld match {
         case ::(head, tail) if head.isInstanceOf[Int] => rec(tail, new ::(head.asInstanceOf[Int] + 1, listNew))
         case ::(head, tail) if !head.isInstanceOf[Int] => throw new Exception("invalid type")
         case Nil => listNew
       }
       rec(this, Nil).reverse()
     }

     def shoutString: List[String] = {
       @scala.annotation.tailrec
       def rec(listOld: List[T], listNew: List[String]): List[String] = listOld match {
         case ::(head, tail) if head.isInstanceOf[String] => rec(tail, new ::("!"+head.asInstanceOf[String], listNew))
         case ::(head, tail) if !head.isInstanceOf[String] => throw new Exception("invalid type")
         case Nil => listNew
       }
       rec(this, Nil).reverse()
     }
   }
     case class ::[A](head: A, tail: List[A]) extends List[A]

     case object Nil extends List[Nothing]

    object List {
      def apply[T](v: T*): List[T] = if (v.isEmpty) Nil else new ::(v.head, apply(v.tail: _*))

      def incList1(l: List[Int]): List[Int] = {
        l.map(v => v+1)
      }

      def shoutString(l: List[String]): List[String] = {
        l.map(v => "!"+v)
      }
    }



    /**
     * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
     *
     */

    /**
      * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
      *
      */

    /**
      * Конструктор, позволяющий создать список из N - го числа аргументов
      * Для этого можно воспользоваться *
      *
      * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
      * def printArgs(args: Int*) = args.foreach(println(_))
      */

    /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */

    /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */


    /**
      *
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */

    /**
      *
      * Написать функцию incList котрая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */


    /**
      *
      * Написать функцию shoutString котрая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */

 }
