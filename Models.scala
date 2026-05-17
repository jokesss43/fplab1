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