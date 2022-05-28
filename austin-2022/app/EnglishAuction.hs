-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- Copyright   :  (c) 2021 IOHK
-- License     :  Apache 2.0
--
-- Maintainer  :  Brian W Bush <brian.bush@iohk.io>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | An English auction contract for Marlowe.
--
-- Characteristic of this contract:
-- *  A seller auctions one unit of an asset.
-- *  Any number of bidders bid on the contract.
-- *  Bids may occur in any order.
-- *  There are a fixed number of bids (rounds of bidding) allowed.
-- *  A bid is rejected if it isn't higher than all previous bids.
-- *  A bid is rejected if it isn't immediately followed by a deposit of the Lovelace that was bid.
-- *  Funds are returned to unsuccessful bidders.
-- *  There is deadline for depositing the asset.
-- *  Each bidding round has a deadline.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Trustworthy        #-}


module EnglishAuction (
-- * Entry point
  main
-- * Contracts
, example
, makeContract
) where


import Language.Marlowe.Extended

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
    makeContract 3 3
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


-- | Create the Marlowe contract for an English auction.
makeContract :: Int       -- ^ The number of rounds of bidding.
             -> Int       -- ^ The number of bidders.
             -> Bound     -- ^ The range for valid bids, in Lovelace.
             -> Token     -- ^ The token representing the asset being bid upon.
             -> Contract  -- ^ The English auction.
makeContract nRounds nBidders bidBounds assetToken =
  let
    bids =
      [
        ChoiceId (fromString $ "Bid " <> show i) party
      |
        i <- [1..nBidders]
      , let party = fromString $ "Bidder " <> show i
      ]
    deadlines =
      [
        TimeParam . fromString $ "Bid Deadline " <> show i
      |
        i <- [1..nRounds]
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
          in
            -- Check if the bid is highest so far.
            If (bidAmount `ValueGT` UseValue highestBid)
              -- Require a deposit if the bid is highest.
              (
                When
                  [
                    -- Deposit the Lovelace for the bid.
                    Case (Deposit bidder bidder ada bidAmount)
                      -- Record the new highest amount.
                      $ Let highestBid bidAmount
                      -- Handle the remaining bids.
                      $ remaining
                      -- Make the payment for the asset.
                      $ Pay bidder (Party seller) ada bidAmount
                      $ Pay seller (Party bidder) assetToken assetAmount
                        Close
                  ]
                  deadline
                  -- Ignore the bid if the deposit was not made.
                  $ remaining continuation
              )
              -- Ignore the bid if it is not highest.
              (
                -- Handle the remaining bids and finalization.
                remaining continuation
              )
    |
      bid@(ChoiceId _ bidder) <- bids
    , let remaining = makeBids bounds assetToken remainingDeadlines bids
    ]
    deadline
    -- End the bidding if no one bids in this round.
    continuation
