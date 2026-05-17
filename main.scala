import scala.io.StdIn

object MainApp:

  val config = AtmConfig(
    daylim = 10000.0,
    comission = 2.0,
    banknotes = List(100, 200, 500, 1000, 2000),
    round = false
  )

  val startState = AtmState(
    balances = Map("Ivan" -> 15000.0, "Masha" -> 3000.0, "Oleg" -> 500.0),
    cashInMachine = Map(5000 -> 5, 1000 -> 10, 500 -> 10, 200 -> 15, 100 -> 20),
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
    putStrLn(s"\nПользователи: ${state.balances.keys.mkString(", ")}")
      .flatMap(_ => putStr("Введите имя (или 'exit' / 'next'): "))
      .flatMap(_ => readStr)
      .flatMap {
        case "exit" => putStrLn("Программа завершена.")
        case "next" =>
          val (writer, nextState) = AtmLogic.nextDay.run(state)
          printLines(writer.log).flatMap(_ => userMenu(nextState))
        case name if state.balances.contains(name) =>
          mainMenu(name, state)
        case _ =>
          putStrLn("Пользователь не найден!").flatMap(_ => userMenu(state))
      }

  case class MenuItem(name: String, exec: (String, AtmState) => IO[Unit])
  
  case class Menu(header: String, items: Seq[MenuItem]):
    def show: String =
      val lines = items.zipWithIndex.map { (item, i) =>
        s"${i + 1}. ${item.name}"
      }
      s"$header\n${lines.mkString("\n")}"

    def handleInput(user: String, state: AtmState, input: String): IO[Unit] =
      val index = input.toIntOption.map(_ - 1).getOrElse(-1)
      if (index >= 0 && index < items.length)
        items(index).exec(user, state)
      else
        MainApp.putStrLn("Неверный выбор. Попробуйте снова.")
          .flatMap(_ => MainApp.mainMenu(user, state))

  val atmMenu = Menu(
    "ГЛАВНОЕ МЕНЮ",
    Seq(
      MenuItem("Снять наличные", (u, s) => doWithdraw(u, s)),
      MenuItem("Пополнить счет", (u, s) => doDeposit(u, s)),
      MenuItem("Сделать перевод", (u, s) => doTransfer(u, s)),
      MenuItem("Сменить пользователя", (u, s) => userMenu(s))
    )
  )

  def mainMenu(user: String, state: AtmState): IO[Unit] =
    val userBalance = state.balances.getOrElse(user, 0.0)

    putStrLn(s"\n Аккаунт: $user | Доступно: $userBalance руб.")
      .flatMap(_ => putStrLn(atmMenu.show)) 
      .flatMap(_ => putStr("Ваш выбор: "))
      .flatMap(_ => readStr)
      .flatMap(choice => atmMenu.handleInput(user, state, choice)) 

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