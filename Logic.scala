object AtmLogic:

  extension [R, A](fa: Reader[R, A])
    def flatMap[B](f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(fa.run(r)).run(r))
    def map[B](f: A => B): Reader[R, B] =
      Reader(r => f(fa.run(r)))

  trait CanWithdrawRule:
    def rule(balance: Double, alreadyToday: Double, amount: Double): Reader[AtmConfig, Writer[Boolean]]

  val sumMoreThanZeroRule: CanWithdrawRule = new CanWithdrawRule {
    def rule(balance: Double, alreadyToday: Double, amount: Double): Reader[AtmConfig, Writer[Boolean]] =
      val isOk = amount > 0
      val log = if (isOk) List("Сумма корректна") else List("Сумма должна быть больше нуля")
      Reader.pure(Writer(log, isOk))
  }

  val enoughBalanceRule: CanWithdrawRule = new CanWithdrawRule {
    def rule(balance: Double, alreadyToday: Double, amount: Double): Reader[AtmConfig, Writer[Boolean]] =
      val isOk = balance >= amount
      val log = if (isOk) List("Баланс проверен") else List("Недостаточно денег")
      Reader.pure(Writer(log, isOk))
  }

  val limitRule: CanWithdrawRule = new CanWithdrawRule {
    def rule(balance: Double, alreadyToday: Double, amount: Double): Reader[AtmConfig, Writer[Boolean]] =
      Reader { cfg =>
        val isOk = (alreadyToday + amount) <= cfg.daylim
        val log = if (isOk) List("Лимит в норме") else List("Превышен лимит")
        Writer(log, isOk)
      }
  }

  def canWithdraw(balance: Double, alreadyToday: Double, amount: Double): Reader[AtmConfig, Writer[Boolean]] =
    for {
      res1 <- sumMoreThanZeroRule.rule(balance, alreadyToday, amount)
      res2 <- enoughBalanceRule.rule(balance, alreadyToday, amount)
      res3 <- limitRule.rule(balance, alreadyToday, amount)
    } yield {
      Writer(
        res1.log ++ res2.log ++ res3.log,
        res1.value && res2.value && res3.value
      )
    }

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