package org.talk

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

object Typeclassopedia {

  object Monad {

    trait Monad[F[_]] {
      /** Constructor (said to lift a value `A` in the `F[A]` monadic context). Also part of `Applicative`, see below. */
      def pure[A](a: A): F[A]

      /** FTW */
      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    }

    class FutureMonad(implicit ec: ExecutionContext) extends Monad[Future] {
      def pure[A](a: A): Future[A] =
        Future.successful(a)

      def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
        fa.flatMap(f)
    }

    object FutureMonad {
      implicit def instance(implicit ec: ExecutionContext): FutureMonad =
        new FutureMonad
    }

    def fib[F[_]](n: Int)(implicit F: Monad[F]): F[BigInt] = {
      def loop(n: Int, a: BigInt, b: BigInt): F[BigInt] =
        F.flatMap(F.pure(n)) { n =>
          if (n <= 1) F.pure(b)
          else loop(n - 1, b, a + b)
        }

      loop(n, BigInt(0), BigInt(1))
    }

    //val res: Future[BigInt] = fib[Future](40)
  }

  object ApplicativeExemple {

    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A => B): F[B]
    }

    trait Applicative[F[_]] extends Functor[F] {
      /** Constructor (lifts a value `A` in the `F[A]` applicative context). */
      def pure[A](a: A): F[A]

      /**
        * Maps over two references at the same time.
        * In other implementations the applicative operation is `ap`, but `map2` is easier to understand.
        */
      def map2[A, B, R](fa: F[A], fb: F[B])(f: (A, B) => R): F[R]
    }

    trait Monad[F[_]] extends Applicative[F] {
      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    }

    // Supplying an instance for Future isn't clean, ExecutionContext needed
    class FutureMonad(implicit ec: ExecutionContext) extends Monad[Future] {
      def pure[A](a: A): Future[A] =
        Future.successful(a)

      def map[A, B](fa: Future[A])(f: A => B): Future[B] =
        fa.map(f)

      def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
        fa.flatMap(f)

      // For Future there's no point in supplying an implementation that's
      // not based on flatMap, but that's not the case for Task ;-)

      def map2[A, B, R](fa: Future[A], fb: Future[B])(f: (A, B) => R): Future[R] =
        for (a <- fa; b <- fb) yield f(a, b)
    }

    object FutureMonad {
      implicit def instance(implicit ec: ExecutionContext): FutureMonad =
        new FutureMonad
    }

    def sequence[F[_], A](list: List[F[A]])(implicit F: Applicative[F]): F[List[A]] = {
      val seed = F.pure(List.empty[A])
      val r = list.foldLeft(seed)((acc, e) => F.map2(acc, e)((l, a) => a :: l))
      F.map(r)(_.reverse)
    }

  }

}
