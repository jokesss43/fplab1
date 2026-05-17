trait Monad[M[_]]:
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def map[A, B](ma: M[A])(f: A => B): M[B]

case class Reader[Env, A](run: Env => A):
  def map[B](f: A => B): Reader[Env, B] =
    Reader(env => f(run(env)))
  def flatMap[B](f: A => Reader[Env, B]): Reader[Env, B] =
    Reader(env => f(run(env)).run(env))

object Reader:
  def pure[R, A](value: A): Reader[R, A] = Reader { _ => value }

case class Writer[A](log: List[String], value: A):
  def map[B](f: A => B): Writer[B] =
    Writer(log, f(value))
  def flatMap[B](f: A => Writer[B]): Writer[B] =
    val next = f(value)
    Writer(log ++ next.log, next.value)

case class State[S, A](run: S => (A, S)):
  def map[B](f: A => B): State[S, B] = State { s =>
    val (res, nextState) = run(s)
    (f(res), nextState)
  }
  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (res, nextState) = run(s)
    f(res).run(nextState)
  }

case class IO[A](unsafeRun: () => A):
  def map[B](f: A => B): IO[B] =
    IO(() => f(unsafeRun()))

  def flatMap[B](f: A => IO[B]): IO[B] =
    IO(() => f(unsafeRun()).unsafeRun())

object IO:
  def pure[A](a: A): IO[A] =
    IO(() => a)

  def delay[A](eff: => A): IO[A] =
    IO(() => eff)
