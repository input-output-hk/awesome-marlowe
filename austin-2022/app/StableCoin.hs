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
-- | A first-price open-bid auction contract for Marlowe.
--
-- Characteristic of this contract:
-- *  A seller auctions one unit of an asset.
-- *  Any number of bidders bid on the contract.
-- *  Bids may occur in any order.
-- *  A bid is rejected if it isn't higher than all previous bids.
-- *  A bid is rejected if it isn't immediately followed by a deposit of the Lovelace that was bid.
-- *  Funds are returned to unsuccessful bidders.
-- *  There is deadline for depositing the asset.
-- *  Each bidding round has a deadline.
-- *  Bidders may only bid once.
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
