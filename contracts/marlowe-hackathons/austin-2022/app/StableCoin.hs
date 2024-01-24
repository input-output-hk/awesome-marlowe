-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- Copyright   :  (c) 2022 IOHK
-- License     :  Apache 2.0
--
-- Maintainer  :  Brian W Bush <brian.bush@iohk.io>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | The Marlowe analog of a stable coin.
--
-- Characteristic of this contract:
-- *  The party deposits ADA.
-- *  An oracle records the price of ADA, and hence the value of ADA deposited.
-- *  The counterparty deposits collateral to cover changes in the ADA price.
-- *  The party signals that they are want to withdraw the funds.
-- *  The oracle records the price of ADA again.
-- *  The party receives ADA equivalent to the current value of their original deposit.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Trustworthy        #-}


module StableCoin (
-- * Entry point
  main
-- * Contracts
, example
, makeContract
) where


import Language.Marlowe.Extended


-- | Print the contract.
main :: IO ()
main = printJSON example


-- | Create an example contract.
example :: Contract
example = makeContract


-- | Make the contract.
makeContract :: Contract
makeContract =
  let
    stableDeposit = ConstantParam "Stable Deposit"
    withdrawalDeadline = TimeParam "Withdrawal Deadline"
    depositDeadline = TimeParam "Deposit Deadline"
    party = Role "Party"
    counterparty = Role "Counterparty"
    oracle = Role "Price Oracle"
    priceBound = [Bound  1 1_000_000_000]
  in
    When
      [Case
        (Deposit party party ada stableDeposit)
        (When
          [Case
            (Choice (ChoiceId "Initial Price" oracle) priceBound)
            (When
              [Case
                (Deposit counterparty counterparty ada
                  (
                    (ConstantParam "Reserve Ratio" `SubValue` Constant 1)
                      `MulValue` stableDeposit
                  )
                )
                (When
                  [Case
                    (Choice (ChoiceId "Request Withdrawal" party) [Bound 1 1])
                    (When
                      [Case
                        (Choice (ChoiceId "Final Price" oracle) priceBound)
                        (Pay party (Account counterparty) ada (AvailableMoney party ada)
                          (Let "Stable Payment"
                            (
                              (stableDeposit `MulValue` ChoiceValue (ChoiceId "Initial Price" oracle))
                                `DivValue` ChoiceValue (ChoiceId "Final Price" oracle)
                            )
                            (Pay counterparty (Party party) ada
                              (Cond (UseValue "Stable Payment" `ValueLE` AvailableMoney counterparty ada)
                                (UseValue "Stable Payment")
                                (AvailableMoney counterparty ada)
                              )
                              Close
                            )
                          )
                        )
                      ]
                      withdrawalDeadline
                      Close
                    )
                  ]
                  withdrawalDeadline
                  Close
                )
              ]
              depositDeadline
              Close
            )
          ]
          depositDeadline
          Close
        )
      ]
      depositDeadline
      Close
