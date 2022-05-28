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
-- | A reverse auction contract for Marlowe.
--
-- Characteristic of this contract:
-- *  Sellers auctions one unit of an asset.
-- *  Any number of sellers offer on the contract.
-- *  Bids may occur in any order.
-- *  There are a fixed number of bids (rounds of bidding) allowed.
-- *  A bid is rejected if it isn't lower than all previous bids.
-- *  A bid is rejected if it isn't immediately followed by a deposit of the asset, if the bidder didn't already deposit the asset.
-- *  Assets are returned to unsuccessful bidders.
-- *  Each bidding round has a deadline.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Trustworthy        #-}


module ReverseAuction (
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


-- | The party that buys the item at auction.
buyer :: Party
buyer = Role "Buyer"


-- | The quantity of items that is auctioned.
assetAmount :: Value
assetAmount = Constant 1


-- | The value of the highest bid.
lowestOffer :: ValueId
lowestOffer = "Lowest Offer"


-- | Create the Marlowe contract for a reverse auction.
makeContract :: Int       -- ^ The number of rounds of bidding.
             -> Int       -- ^ The number of bidders.
             -> Bound     -- ^ The range for valid bids, in Lovelace.
             -> Token     -- ^ The token representing the asset being bid upon.
             -> Contract  -- ^ The reverse auction.
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
   -- Make the bids, but close if no one bids.
   makeBids bidBounds assetToken deadlines bids
     Close


-- | Deposit the asset that is the subject of the bidding.
ensureAssetDeposit :: Token     -- ^ The token representing the asset being bid upon.
                   -> Timeout   -- ^ The deadline for the deposit.
                   -> Party     -- ^ The bidder.
                   -> Contract  -- ^ The contract to be executed after the asset is deposited.
                   -> Contract  -- ^ The contract to be executed if the asset is not deposited.
                   -> Contract  -- ^ The contract for the asset deposit and subsequent activity.
ensureAssetDeposit asset deadline bidder depositContinuation notDepositContinuation =
  If (AvailableMoney bidder asset `ValueEQ` assetAmount)
    -- No deposit is necessary.
    depositContinuation
    -- The bidder must deposit the asset.
    (
      When
        [
          Case (Deposit bidder bidder asset assetAmount)
            depositContinuation
        ]
        deadline
        notDepositContinuation
    )


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
            -- Check if the bid is lowest so far.
            If ((bidAmount `ValueLT` UseValue lowestOffer) `OrObs` (Constant 0 `ValueEQ` UseValue lowestOffer))
              -- Require a deposit if the bid is lowest.
              (
                ensureAssetDeposit assetToken deadline bidder
                  (
                    -- Record the new highest amount.
                    lowestOffer `Let` bidAmount
                      -- Let the buyer purchase the asset.
                      $ When
                          [
                            Case (Deposit bidder buyer ada bidAmount)
                              -- Deliver the asset to the buyer.
                              $ Pay bidder (Party buyer) assetToken assetAmount
                                Close
                          ]
                          -- Handle the remaining bids.
                          deadline
                            $ remaining continuation
                  )
                  (
                    -- Handle the remaining bids.
                    remaining continuation
                  )
              )
              -- Ignore the bid if it is not highest.
              (
                -- Handle the remaining bids.
                remaining continuation
              )
    |
      let remaining = makeBids bounds assetToken remainingDeadlines bids
    , bid@(ChoiceId _ bidder) <- bids
    ]
    -- End the bidding if no one bids in this round.
    deadline
    continuation
