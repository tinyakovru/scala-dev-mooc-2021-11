package futures

import HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] =
  //    task"Реализуйте метод `fullSequence`"()
    futures.foldLeft(Future.successful((List.empty[A],List.empty[Throwable]))) { (ac, ft) =>
      ac.zipWith(ft)((tup, res) => res match {
        case res: A         => (res :: tup._1, tup._2)
        case res: Throwable => (tup._1, res :: tup._2)
      })
    }
}
