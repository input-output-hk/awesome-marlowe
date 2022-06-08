import {
    PK, Role, Account, Party, ada, AvailableMoney, Constant, ConstantParam,
    NegValue, AddValue, SubValue, MulValue, DivValue, ChoiceValue, TimeIntervalStart,
    TimeIntervalEnd, UseValue, Cond, AndObs, OrObs, NotObs, ChoseSomething,
    ValueGE, ValueGT, ValueLT, ValueLE, ValueEQ, TrueObs, FalseObs, Deposit,
    Choice, Notify, Close, Pay, If, When, Let, Assert, SomeNumber, AccountId,
    ChoiceId, Token, ValueId, Value, EValue, Observation, Bound, Action, Payee,
    Case, Timeout, ETimeout, TimeParam, Contract
} from 'marlowe-js';

/*

 The Marlowe analog of a stable coin.

 Characteristic of this contract:
 *  The party deposits ADA.
 *  An oracle records the price of ADA, and hence the value of ADA deposited.
 *  The counterparty deposits collateral to cover changes in the ADA price.
 *  The party signals that they are want to withdraw the funds.
 *  The oracle records the price of ADA again.
 *  The party receives ADA equivalent to the current value of their original deposit.

*/

(function (): Contract {

  // Make the contract.
  function makeContract() : Contract
  {
    const stableDeposit : Value = ConstantParam("Stable Deposit")
    const withdrawalDeadline : Timeout = TimeParam("Withdrawal Deadline")
    const depositDeadline : Timeout = TimeParam("Deposit Deadline")
    const party : Party = Role("Party")
    const counterparty : Party = Role("Counterparty")
    const oracle : Party = Role("Price Oracle")
    const priceBound : Bound[] = [Bound(1, 1000000000)]
    return When(
      [
        Case(
          Deposit(party, party, ada, stableDeposit)
        , When(
            [
              Case(Choice(ChoiceId("Initial Price", oracle), priceBound)
              , When(
                  [
                    Case(
                      Deposit(counterparty, counterparty, ada, MulValue(SubValue(ConstantParam("Reserve Ratio"), Constant(1)), stableDeposit))
                    , When(
                        [
                          Case(Choice(ChoiceId("Request Withdrawal", party), [Bound(1, 1)])
                          , When(
                              [
                                Case(Choice(ChoiceId("Final Price", oracle), priceBound)
                                , Pay(party, Account(counterparty), ada, AvailableMoney(ada, party)
                                  , Let("Stable Payment"
                                    , DivValue(MulValue( stableDeposit, ChoiceValue(ChoiceId("Initial Price", oracle))), ChoiceValue(ChoiceId("Final Price", oracle)))
                                    , Pay(counterparty, Party(party), ada
                                      , Cond(ValueLE(UseValue("Stable Payment"), AvailableMoney(ada, counterparty))
                                        , UseValue("Stable Payment")
                                        , AvailableMoney(ada, counterparty)
                                        )
                                      , Close
                                      )
                                    )
                                  )
                                )
                              ]
                            , withdrawalDeadline
                            , Close
                            )
                          )
                        ]
                      , withdrawalDeadline
                      , Close
                      )
                    )
                  ]
                , depositDeadline
                , Close
                )
              )
            ]
          , depositDeadline
          , Close
          )
        )
      ]
    , depositDeadline
    , Close
    )
  }

  // Create an example contract.
  const example : Contract = makeContract()

  return example

})
