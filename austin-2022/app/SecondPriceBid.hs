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
-- | A second-price open-bid auction contract for Marlowe.
--
-- Characteristic of this contract:
-- *  A seller auctions one unit of an asset.
-- *  Any number of bidders bid on the contract.
-- *  Bids may occur in any order.
-- *  A bid is rejected if it isn't the highest or second highest of the bids so far.
-- *  A bid is rejected if it isn't immediately followed by a deposit of the Lovelace that was bid.
-- *  Funds are returned to unsuccessful bidders.
-- *  There is deadline for depositing the asset.
-- *  Each bidding round has a deadline.
-- *  Bidders may only bid once.
-- *  The winner pays the value of the second highest of the bids, and receives a refund of the difference between the highest and second highest.
-- *  If there is only one bid, the winner pays their bid.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Trustworthy        #-}


module SecondPriceBid (
-- * Entry point
  main
-- * Contracts
, example
, makeContract
) where


import Language.Marlowe.Extended

import Data.List   (permutations)
import Data.String (fromString)


-- | Print the contract.
main :: IO ()
main = printJSON example


-- | Create an example contract.
example :: Contract
example =
  let
    asset = Token "1Ada2Ada3Ada4Ada5Ada6Ada7Ada8Ada9Ada10Ada11Ada12Ada13Ada" "The Asset"
  in
    makeContract 3
      (Bound 2_000_000 1_000_000_000_000)
      asset


-- | The party that sells the item at auction.
seller :: Party
seller = Role "Seller"


-- | The quantity of items that is auctioned.
assetAmount :: Value
assetAmount = Constant 1


-- | The value of the highest bid.
highestBid :: ValueId
highestBid = "Highest Bid"


-- | The value of the second highest bid.
secondHighestBid :: ValueId
secondHighestBid = "Second highest Bid"


-- | Create the Marlowe contract for a second-price open-bid auction.
makeContract :: Int       -- ^ The number of bidders.
             -> Bound     -- ^ The range for valid bids, in Lovelace.
             -> Token     -- ^ The token representing the asset being bid upon.
             -> Contract  -- ^ The second-price open-bid auction.
makeContract n bidBounds assetToken =
  let
    (bids, deadlines) =
      unzip
        [
          (bid, deadline)
        |
          i <- [1..n]
        , let party = fromString $ "Bidder " <> show i
              bid = ChoiceId (fromString $ "Bid " <> show i) party
              deadline = TimeParam . fromString $ "Bid Deadline " <> show i
        ]
  in
   -- Deposit the asset, then make the bids, but close if no one bids.
    makeAssetDeposit assetToken
      $ makeBids bidBounds assetToken deadlines bids
        Close


-- | Deposit the asset that is the subject of the bidding.
makeAssetDeposit :: Token     -- ^ The token representing the asset being bid upon.
                 -> Contract  -- ^ The contract to be executed after the asset is deposited.
                 -> Contract  -- ^ The contract for the asset deposit and subsequent activity.
makeAssetDeposit asset continuation =
  let
    assetDeadline = TimeParam "Deadline to Deposit Asset"
  in
    When
      [
        -- The seller deposits the asset being auctioned.
        Case (Deposit seller seller asset assetAmount)
          continuation
      ]
      -- End the contract if the deposit is not made.
      assetDeadline
      Close


-- | Make the contract for bids.
makeBids :: Bound       -- ^ The range of valid bids, in Lovelace.
         -> Token       -- ^ The token representing the asset being bid upon.
         -> [Timeout]   -- ^ The deadlines for the rounds of bidding.
         -> [ChoiceId]  -- ^ The choices the bidders will make.
         -> Contract    -- ^ The contract to be executed at the end of the bidding.
         -> Contract    -- ^ The bidding contract.
makeBids _ _ [] _ continuation = continuation
makeBids _ _ _ [] continuation = continuation
makeBids bounds assetToken (deadline : remainingDeadlines) bids continuation =
  When
    [
      -- Let the bidder make their bid.
      Case (Choice bid [bounds])
        $ let
            bidAmount = ChoiceValue bid
            winAmount =
              Cond (UseValue secondHighestBid `ValueEQ` Constant 0)
                (
                  UseValue highestBid
                )
                (
                  UseValue secondHighestBid
                )
          in
            -- Check if the bid is highest or second highest so far.
            If (bidAmount `ValueGT` UseValue secondHighestBid)
              -- Require a deposit if the bid is highest or second highest.
              (
                When
                  [
                    -- Deposit the Lovelace for the bid.
                    Case (Deposit bidder bidder ada bidAmount)
                      -- Check if the bid is highest so far.
                      $ If (bidAmount `ValueGT` UseValue highestBid)
                          (
                            -- Record the new second highest amount.
                            secondHighestBid `Let` UseValue highestBid
                            -- Record the new highest amount.
                            $ highestBid `Let` bidAmount
                            -- Handle the remaining bids.
                            $ remaining
                            -- Make the payment for the asset.
                            $ Pay bidder (Party seller) ada winAmount
                            $ Pay seller (Party bidder) assetToken assetAmount
                              Close
                          )
                          (
                            -- Record the new second highest amount.
                            secondHighestBid `Let` bidAmount
                            -- Handle the remaining bids.
                              $ remaining continuation
                          )
                  ]
                  -- Ignore the bid if the deposit was not made.
                  deadline
                    $ remaining continuation
              )
              (
                -- Handle the remaining bids.
                remaining continuation
              )
    |
      bid@(ChoiceId _ bidder) : remainingBids <- permutations bids
    , let remaining = makeBids bounds assetToken remainingDeadlines remainingBids
    ]
    -- End the bidding if no one bids in this round.
    deadline
    continuation
