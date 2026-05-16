import scala.io.StdIn

// монады

trait Monad[M[_]]:
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def map[A, B](ma: M[A])(f: A => B): M[B]

case class Reader[Env, A](run: Env => A):
  def map[B](f: A => B): Reader[Env, B] =
    Reader(env => f(run(env)))
  def flatMap[B](f: A => Reader[Env, B]): Reader[Env, B] =
    Reader(env => f(run(env)).run(env))

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


// данные

case class AtmConfig(
  daylim: Double,
  comission: Double,
  banknotes: List[Int],
  round: Boolean
  )

case class AtmState(
  balances: Map[String, Double],
  cashInMachine: Map[Int, Int],
  todayWithdraw: Map[String, Double]
  )

// логика

object AtmLogic:

  def roundedAmount(amount: Int): Reader[AtmConfig, Int] = Reader { cfg =>
    val minNote = if (cfg.banknotes.isEmpty) 100 else cfg.banknotes.min
    val remainder = amount % minNote
    if (remainder == 0) amount
    else if (cfg.round) amount + (minNote - remainder)
    else amount - remainder
  }

  def transferFee(amount: Double): Reader[AtmConfig, Double] = Reader { cfg =>
    amount * (cfg.comission / 100.0)
  }

  def canWithdraw(balance: Double, alreadyToday: Double, amount: Double): Reader[AtmConfig, Writer[Boolean]] = Reader { cfg =>
    if (amount <= 0)
      Writer(List("Сумма должна быть больше нуля"), false)
    else if (balance < amount)
      Writer(List("Недостаточно денег на балансе"), false)
    else if (alreadyToday + amount > cfg.daylim)
      Writer(List("Превышен дневной лимит снятия"), false)
    else
      Writer(List("Проверка лимитов успешна"), true)
  }

  def withdrawPlan(amount: Int, cash: Map[Int, Int]): Reader[AtmConfig, Writer[Option[Map[Int, Int]]]] = Reader { cfg =>
    val sortedNotes = cfg.banknotes.sorted.reverse

    val (leftover, plan) = sortedNotes.foldLeft((amount, Map.empty[Int, Int])) { (acc, banknote) =>
      val left = acc._1
      val plan = acc._2
      val cashIn = cash.getOrElse(banknote, 0)
      val needed = left / banknote
      val toGive = if (needed < cashIn) needed else cashIn

      if (toGive > 0) (left - (toGive * banknote), plan + (banknote -> toGive))
      else (left, plan)
    }

    if (leftover == 0)
      val lines = plan.map((note, count) => s"Купюры $note руб. -> $count шт.").toList
      Writer(List("План выдачи успешно составлен") ++ lines, Some(plan))
    else
      Writer(List("В банкомате нет нужных купюр для выдачи такой суммы"), None)
  }

  def deposit(user: String, amount: Double): State[AtmState, Writer[Unit]] = State { state =>
    val oldBalance = state.balances.getOrElse(user, 0.0)
    val updatedBalances = state.balances.updated(user, oldBalance + amount)
    val nextState = state.copy(balances = updatedBalances)
    (Writer(List(s"Успешное пополнение счета $user на $amount"), ()), nextState)
  }

  def transfer(from: String, to: String, amount: Double, fee: Double): State[AtmState, Writer[Boolean]] = State { state =>
    val fromBalance = state.balances.getOrElse(from, 0.0)
    val totalAmount = amount + fee

    if (fromBalance >= totalAmount)
      val toBalance = state.balances.getOrElse(to, 0.0)
      val updatedBalances = state.balances
        .updated(from, fromBalance - totalAmount)
        .updated(to, toBalance + amount)
      val nextState = state.copy(balances = updatedBalances)
      (Writer(List(s"Перевод совершен: $amount отдан $to. Списана комиссия $fee"), true), nextState)
    else
      (Writer(List("Недостаточно средств для перевода и комиссии"), false), state)
  }

  def withdraw(user: String, amount: Int, plan: Map[Int, Int]): State[AtmState, Unit] = State { state =>
    val oldBalance = state.balances.getOrElse(user, 0.0)
    val oldWithdrawn = state.todayWithdraw.getOrElse(user, 0.0)

    val nextCash = state.cashInMachine.map { (note, count) =>
      val taken = plan.getOrElse(note, 0)
      (note, count - taken)
    }

    val nextState = state.copy(
      balances = state.balances.updated(user, oldBalance - amount),
      cashInMachine = nextCash,
      todayWithdraw = state.todayWithdraw.updated(user, oldWithdrawn + amount)
    )
    ((), nextState)
  }

  def nextDay: State[AtmState, Writer[Unit]] = State { state =>
    (Writer(List("Наступил новый день. Лимиты сброшены"), ()), state.copy(todayWithdraw = Map.empty))
  }

// IO

object MainApp:

  val config = AtmConfig(
    daylim = 40000.0,
    comission = 2.0,
    banknotes = List(100, 500, 1000),
    round = false
  )

  val startState = AtmState(
    balances = Map("Ivan" -> 15000.0, "Masha" -> 3000.0, "Oleg" -> 500.0),
    cashInMachine = Map(1000 -> 10, 500 -> 10, 100 -> 20),
    todayWithdraw = Map("Ivan" -> 2000.0)
  )

  def printLines(logs: List[String]): IO[Unit] =
    IO.delay {
      println("ЧЕК ОПЕРАЦИИ")
      logs.foreach(line => println(s"> $line"))
    }

  def putStrLn(text: String): IO[Unit] =
    IO.delay(println(text))

  def putStr(text: String): IO[Unit] =
    IO.delay(print(text))

  def readStr: IO[String] =
    IO.delay(StdIn.readLine())

  @main def run(): Unit =
    userMenu(startState).unsafeRun()

  def userMenu(state: AtmState): IO[Unit] =
    putStrLn(s"\nПользователи в системе: ${state.balances.keys.mkString(", ")}")
      .flatMap(_ => putStr("Введите имя (или 'exit' / 'next'): "))
      .flatMap(_ => readStr)
      .flatMap(input => handleUserMenuInput(input, state))

  def handleUserMenuInput(input: String, state: AtmState): IO[Unit] =
    if (input == "exit") {
      putStrLn("Завершение работы программы.")
    } else if (input == "next") {
      val (writer, nextState) = AtmLogic.nextDay.run(state)
      printLines(writer.log).flatMap(_ => userMenu(nextState))
    } else if (state.balances.contains(input)) {
      mainMenu(input, state)
    } else {
      putStrLn("Пользователь не найден! Повторите ввод.").flatMap(_ => userMenu(state))
    }

  // менюшка

  def mainMenu(user: String, state: AtmState): IO[Unit] =
    val userBalance = state.balances.getOrElse(user, 0.0)
    putStrLn(s"\nАккаунт: $user | Доступно: $userBalance руб.")
      .flatMap(_ => putStrLn("1. Снять наличные\n2. Пополнить счет\n3. Сделать перевод\n4. Сменить пользователя"))
      .flatMap(_ => putStr("Ваш выбор: "))
      .flatMap(_ => readStr)
      .flatMap(choice => menuchoice(choice, user, state))

  def menuchoice(choice: String, user: String, state: AtmState): IO[Unit] =
    if (choice == "1") doWithdraw(user, state)
    else if (choice == "2") doDeposit(user, state)
    else if (choice == "3") doTransfer(user, state)
    else if (choice == "4") userMenu(state)
    else putStrLn("Ошибка ввода!").flatMap(_ => mainMenu(user, state))

  // операции

  def doWithdraw(user: String, state: AtmState): IO[Unit] =
    putStr("Введите сумму для снятия: ")
      .flatMap(_ => readStr)
      .flatMap(input => processWithdraw(input, user, state))

  def processWithdraw(input: String, user: String, state: AtmState): IO[Unit] =
    val rawAmount = input.toIntOption.getOrElse(0)
    val amount = AtmLogic.roundedAmount(rawAmount).run(config)

    val bal = state.balances.getOrElse(user, 0.0)
    val withdrawn = state.todayWithdraw.getOrElse(user, 0.0)
    val checkWriter = AtmLogic.canWithdraw(bal, withdrawn, amount.toDouble).run(config)

    if (!checkWriter.value) {
      printLines(checkWriter.log).flatMap(_ => mainMenu(user, state))
    } else {
      val planWriter = AtmLogic.withdrawPlan(amount, state.cashInMachine).run(config)
      val fullLog = checkWriter.log ++ planWriter.log

      if (planWriter.value.isDefined) {
        val plan = planWriter.value.get
        val ((), nextState) = AtmLogic.withdraw(user, amount, plan).run(state)
        printLines(fullLog :+ s"Выдано: $amount").flatMap(_ => mainMenu(user, nextState))
      } else {
        printLines(fullLog).flatMap(_ => mainMenu(user, state))
      }
    }

  def doDeposit(user: String, state: AtmState): IO[Unit] =
    putStr("Введите сумму пополнения: ")
      .flatMap(_ => readStr)
      .flatMap(input => processDeposit(input, user, state))

  def processDeposit(input: String, user: String, state: AtmState): IO[Unit] =
    val amount = input.toDoubleOption.getOrElse(0.0)
    if (amount <= 0) {
      putStrLn("Сумма некорректна!").flatMap(_ => mainMenu(user, state))
    } else {
      val (writer, nextState) = AtmLogic.deposit(user, amount).run(state)
      printLines(writer.log).flatMap(_ => mainMenu(user, nextState))
    }

  def doTransfer(user: String, state: AtmState): IO[Unit] =
    putStr("Имя получателя: ")
      .flatMap(_ => readStr)
      .flatMap(toUser => handleTransferTarget(toUser, user, state))

  def handleTransferTarget(toUser: String, user: String, state: AtmState): IO[Unit] =
    if (!state.balances.contains(toUser)) {
      putStrLn("Такого пользователя нет!").flatMap(_ => mainMenu(user, state))
    } else if (user == toUser) {
      putStrLn("Ошибка: перевод самому себе невозможен.").flatMap(_ => mainMenu(user, state))
    } else {
      putStr("Сумма перевода: ")
        .flatMap(_ => readStr)
        .flatMap(input => processTransfer(input, user, toUser, state))
    }

  def processTransfer(input: String, user: String, toUser: String, state: AtmState): IO[Unit] =
    val amount = input.toDoubleOption.getOrElse(0.0)
    if (amount <= 0) {
      putStrLn("Сумма перевода должна быть больше нуля!").flatMap(_ => mainMenu(user, state))
    } else {
      val fee = AtmLogic.transferFee(amount).run(config)
      val (writer, nextState) = AtmLogic.transfer(user, toUser, amount, fee).run(state)
      printLines(writer.log).flatMap(_ => mainMenu(user, nextState))
    }