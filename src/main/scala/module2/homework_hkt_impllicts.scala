package module2

object homework_hkt_impllicts{

    /**
      *
      * Доработать сигнатуру tupleF и реализовать его
      * По итогу должны быть возможны подобные вызовы
      *   val r1 = println(tupleF(optA, optB))
      *   val r2 = println(tupleF(list1, list2))
      *
      */

    def tupleF[F[_], A, B](fa: F[A], fb: F[B])(implicit bindableA: F[A] => Bindable[F,A],
                                                        bindableB: F[B] => Bindable[F,B]):F[(A,B)] = {
      fa.flatMap(a => fb.map(b => (a, b)))
    }

    trait Bindable[F[_], A] {
        def map[B](f: A => B): F[B]
        def flatMap[B](f: A => F[B]): F[B]
    }

    object Bindable {
      implicit def optBindable[A](op: Option[A]): Bindable[Option, A] = new Bindable[Option,A] {
        override def map[B](f: A => B): Option[B] =  op.map(f)
        override def flatMap[B](f: A => Option[B]): Option[B] = op.flatMap(f)
      }
      implicit def listBindable[A](ls: List[A]): Bindable[List, A] = new Bindable[List,A] {
        override def map[B](f: A => B): List[B] =  ls.map(f)
        override def flatMap[B](f: A => List[B]): List[B] = ls.flatMap(f)
      }
    }

  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val r1 = println(tupleF(optA, optB))
  val r2 = println(tupleF(list1, list2))
}
