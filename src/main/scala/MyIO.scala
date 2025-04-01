package io

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/** Класс типов, позволяющий комбинировать описания вычислений, которые могут либо успешно
 * завершиться с некоторым значением, либо завершиться неуспешно, выбросив исключение Throwable.
 * @tparam F
 *   тип вычисления
 */
trait Computation[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]
  def pure[A](a: A): F[A]
  def *>[A, B](fa: F[A])(another: F[B]): F[B]
  def as[A, B](fa: F[A])(newValue: => B): F[B]
  def void[A](fa: F[A]): F[Unit]
  def attempt[A](fa: F[A]): F[Either[Throwable, A]]
  def option[A](fa: F[A]): F[Option[A]]

  /** Если вычисление fa выбрасывает ошибку, то обрабатывает ее функцией f, без изменения типа
   * выходного значения.
   * @return
   *   результат вычисления fa или результат функции f
   */
  def handleErrorWith[A, AA >: A](fa: F[A])(f: Throwable => F[AA]): F[AA]

  /** Обрабатывает ошибку вычисления чистой функцией recover или преобразует результат вычисления
   * чистой функцией.
   * @return
   *   результат вычисления преобразованный функцией map или результат функции recover
   */
  def redeem[A, B](fa: F[A])(recover: Throwable => B, map: A => B): F[B]
  def redeemWith[A, B](fa: F[A])(recover: Throwable => F[B], bind: A => F[B]): F[B]

  /** Выполняет вычисление. "unsafe", потому что при неуспешном завершении может выбросить
   * исключение.
   * @param fa
   *   еще не начавшееся вычисление
   * @tparam A
   *   тип результата вычисления
   * @return
   *   результат вычисления, если оно завершится успешно.
   */
  def unsafeRunSync[A](fa: F[A]): A

  /** Оборачивает ошибку в контекст вычисления.
   * @param error
   *   ошибка
   * @tparam A
   *   тип результата вычисления. Т.к. вычисление сразу завершится ошибкой при выполнении, то может
   *   быть любым.
   * @return
   *   создает описание вычисления, которое сразу же завершается с поданной ошибкой.
   */
  def raiseError[A](error: Throwable): F[A]

}

sealed trait IORecStruct[A]
case class Pure[A](v: A)                               extends IORecStruct[A]
case class Suspend[A](call: () => MyIO[A])             extends IORecStruct[A]
case class FlatMap[A, B](fa: MyIO[A], f: A => MyIO[B]) extends IORecStruct[B]
case class RaiseError[A](e: Throwable)                 extends IORecStruct[A]

case class RedeemWith[A, B](fa: MyIO[A], recover: Throwable => MyIO[B], bind: A => MyIO[B])
  extends IORecStruct[B]

object Computation {
  def apply[F[_]: Computation]: Computation[F] = implicitly[Computation[F]]
}

final class MyIO[A](val recStruct: IORecStruct[A]) {

  def map[B](f: A => B)(implicit
                        comp: Computation[MyIO]
  ): MyIO[B] = comp.map(this)(f)

  def flatMap[B](f: A => MyIO[B])(implicit
                                  comp: Computation[MyIO]
  ): MyIO[B] = comp.flatMap(this)(f)

  def tailRecM[B](f: A => MyIO[Either[A, B]])(implicit
                                              comp: Computation[MyIO]
  ): MyIO[B] =
    comp.tailRecM[A, B](this.unsafeRunSync)(f)

  def *>[B](another: MyIO[B])(implicit
                              comp: Computation[MyIO]
  ): MyIO[B] = comp.*>(this)(another)

  def as[B](newValue: => B)(implicit
                            comp: Computation[MyIO]
  ): MyIO[B] = comp.as(this)(newValue)

  def void(implicit
           comp: Computation[MyIO]
          ): MyIO[Unit] = comp.void(this)

  def attempt(implicit
              comp: Computation[MyIO]
             ): MyIO[Either[Throwable, A]] = comp.attempt(this)

  def option(implicit
             comp: Computation[MyIO]
            ): MyIO[Option[A]] = comp.option(this)

  def handleErrorWith[AA >: A](f: Throwable => MyIO[AA])(implicit
                                                         comp: Computation[MyIO]
  ): MyIO[AA] =
    comp.handleErrorWith[A, AA](this)(f)

  def redeem[B](recover: Throwable => B, map: A => B)(implicit
                                                      comp: Computation[MyIO]
  ): MyIO[B] =
    comp.redeem(this)(recover, map)

  def redeemWith[B](recover: Throwable => MyIO[B], bind: A => MyIO[B])(implicit
                                                                       comp: Computation[MyIO]
  ): MyIO[B] =
    comp.redeemWith(this)(recover, bind)

  def unsafeRunSync(implicit
                    comp: Computation[MyIO]
                   ): A = comp.unsafeRunSync(this)

}

object MyIO {

  implicit def convert[A](in: IORecStruct[A]): MyIO[A] = new MyIO[A](in)

  implicit val computationInstanceForIO: Computation[MyIO] = new Computation[MyIO] {

    override def map[A, B](fa: MyIO[A])(f: A => B): MyIO[B] = FlatMap(fa, f.andThen(pure[B]))

    override def flatMap[A, B](fa: MyIO[A])(f: A => MyIO[B]): MyIO[B] = FlatMap(fa, f)

    override def tailRecM[A, B](a: A)(f: A => MyIO[Either[A, B]]): MyIO[B] =
      Suspend(() =>
        flatMap(f(a)) {
          case Right(value) => MyIO(value)
          case Left(value)  => tailRecM(value)(f)
        }
      )

    override def pure[A](a: A): MyIO[A] = MyIO.pure(a)

    override def *>[A, B](fa: MyIO[A])(another: MyIO[B]): MyIO[B] = flatMap(fa)(_ => another)

    override def as[A, B](fa: MyIO[A])(newValue: => B): MyIO[B] = map(fa)(_ => newValue)

    override def void[A](fa: MyIO[A]): MyIO[Unit] = map(fa)(_ => ())

    override def attempt[A](fa: MyIO[A]): MyIO[Either[Throwable, A]] =
      redeemWith(fa)(e => MyIO(Left(e)), a => MyIO(Right(a)))

    override def option[A](fa: MyIO[A]): MyIO[Option[A]] =
      redeemWith(fa)(_ => MyIO(None), a => MyIO(Some(a)))

    override def handleErrorWith[A, AA >: A](fa: MyIO[A])(f: Throwable => MyIO[AA]): MyIO[AA] =
      redeemWith(fa)(f, a => Pure[AA](a))

    override def redeem[A, B](fa: MyIO[A])(recover: Throwable => B, map: A => B): MyIO[B] =
      redeemWith(fa)(e => Pure(recover(e)), a => Pure(map(a)))

    override def redeemWith[A, B](
                                   fa: MyIO[A]
                                 )(recover: Throwable => MyIO[B], bind: A => MyIO[B]): MyIO[B] = RedeemWith(fa, recover, bind)

    override def unsafeRunSync[A](ioa: MyIO[A]): A =
      run(ioa)

    def step[A](ioa: MyIO[A]): Either[() => MyIO[A], A] =
      ioa.recStruct match {
        case Pure(v)       => Right(v)
        case Suspend(call) => Left(call)
        case RaiseError(e) => throw e
        case FlatMap(ioa, f) =>
          ioa.recStruct match {
            case Pure(v)           => Left(() => f(v))
            case Suspend(call)     => Left(() => FlatMap(call(), f))
            case FlatMap(ioa1, f1) => Left(() => FlatMap(ioa1, (x: Any) => FlatMap(f1(x), f)))
            case RaiseError(e)     => throw e
            case RedeemWith(ioa2, recover, bind) =>
              Left(() =>
                RedeemWith(ioa2, e => FlatMap(recover(e), f), (x: Any) => FlatMap(bind(x), f))
              )
          }
        case RedeemWith(ioa, recover, bind) =>
          ioa.recStruct match {
            case Pure(v) => Left(() => bind(v))
            case Suspend(call) =>
              Left(() =>
                try RedeemWith(call(), recover, bind)
                catch {
                  case e: Throwable => recover(e)
                }
              )
            case FlatMap(fa1, f1) =>
              Left(() =>
                RedeemWith(
                  fa1,
                  recover,
                  (x: Any) =>
                    try RedeemWith(f1(x), recover, bind)
                    catch {
                      case e: Throwable => recover(e)
                    }
                )
              )
            case RaiseError(e) => Left(() => recover(e))
            case RedeemWith(ioa1, recover1, bind1) =>
              Left(() =>
                RedeemWith(
                  ioa1,
                  e => FlatMap(recover1(e), bind),
                  (x: Any) => FlatMap(bind1(x), bind)
                )
              )
          }
      }

    @tailrec
    def run[A](ioa: MyIO[A]): A = step(ioa) match {
      case Right(v)   => v
      case Left(more) => run(more())
    }

    override def raiseError[A](error: Throwable): MyIO[A] = RaiseError[A](error)
  }

  def apply[A](body: => A): MyIO[A] = delay(body)

  def suspend[A](thunk: => MyIO[A]): MyIO[A] = Suspend(() => thunk)

  def delay[A](body: => A): MyIO[A] = Suspend(() => Pure(body))

  def pure[A](a: A): MyIO[A] = Pure(a)

  def fromEither[A](e: Either[Throwable, A])(implicit
                                             comp: Computation[MyIO]
  ): MyIO[A] =
    e match {
      case Right(a) => Pure(a)
      case Left(e)  => comp.raiseError(e)
    }

  def fromOption[A](option: Option[A])(orElse: => Throwable)(implicit
                                                             comp: Computation[MyIO]
  ): MyIO[A] =
    option match {
      case Some(a) => Pure(a)
      case None    => comp.raiseError(orElse)
    }

  def fromTry[A](t: Try[A])(implicit
                            comp: Computation[MyIO]
  ): MyIO[A] =
    t match {
      case Success(a) => Pure(a)
      case Failure(e) => comp.raiseError(e)
    }

  def none[A]: MyIO[Option[A]] = Pure[Option[A]](None)

  def raiseUnless(cond: Boolean)(e: => Throwable)(implicit
                                                  comp: Computation[MyIO]
  ): MyIO[Unit] =
    if (!cond) comp.raiseError(e) else unit

  def raiseWhen(cond: Boolean)(e: => Throwable)(implicit
                                                comp: Computation[MyIO]
  ): MyIO[Unit] =
    if (cond) comp.raiseError(e) else unit

  def raiseError[A](error: Throwable)(implicit
                                      comp: Computation[MyIO]
  ): MyIO[A] = comp.raiseError(error)

  def unlessA(cond: Boolean)(action: => MyIO[Unit]): MyIO[Unit] = if (!cond) action else unit

  def whenA(cond: Boolean)(action: => MyIO[Unit]): MyIO[Unit] = if (cond) action else unit

  val unit: MyIO[Unit] = Pure(())

}
