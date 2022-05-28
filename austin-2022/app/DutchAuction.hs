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
-- | A Dutch auction contract for Marlowe.
--
-- Characteristic of this contract:
-- *  A seller auctions one unit of an asset.
-- *  Any number of bidders bid on the contract.
-- *  The selling price is dropped in equal steps.
-- *  The first bidder to deposit the price winds the asset.
-- *  There is deadline for depositing the asset.
-- *  Each bidding round has a deadline.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Trustworthy        #-}


module DutchAuction (
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
    makeContract 3
      (Bound 2_000_000 1_002_000_000)
      asset
      5


-- | The party that sells the item at auction.
seller :: Party
seller = Role "Seller"


-- | The quantity of items that is auctioned.
assetAmount :: Value
assetAmount = Constant 1


-- | Create the Marlowe contract for a Dutch auction.
makeContract :: Int       -- ^ The number of bidders.
             -> Bound     -- ^ The range for valid bids, in Lovelace.
             -> Token     -- ^ The token representing the asset being bid upon.
             -> Integer   -- ^ The number of steps of lowering the price from the top to the bottom of the range.
             -> Contract  -- ^ The Dutch auction.
makeContract n (Bound minimumPrice maximumPrice) assetToken steps =
  let
    bidders = fromString . ("Bidder " <>) . show <$> [1..n]
    (prices, deadlines) =
      unzip
        [
          (Constant price, deadline)
        |
          let delta = fromInteger (maximumPrice - minimumPrice) `div` (steps - 1)
        , i <- [1..steps]
        , let price = if i == 1 then maximumPrice else minimumPrice + (steps - i) * delta
              deadline = TimeParam . fromString $ "Bid Deadline " <> show i
        ]
  in
   -- Deposit the asset, then make the bids, but close if no one bids.
    makeAssetDeposit assetToken
      $ makeBids assetToken deadlines prices bidders
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
      -- Otherwise, the contract ends.
      assetDeadline
      Close


-- | Make the contract for bids.
makeBids :: Token       -- ^ The token representing the asset being bid upon.
         -> [Timeout]   -- ^ The deadlines for the rounds of bidding.
         -> [Value]     -- ^ The prices for the deadlines.
         -> [Party]     -- ^ The bidders.
         -> Contract    -- ^ The contract to be executed at the end of the bidding.
         -> Contract    -- ^ The bidding contract.
makeBids _ [] _ _ continuation = continuation
makeBids _ _ [] _ continuation = continuation
makeBids _ _ _ [] continuation = continuation
makeBids assetToken (deadline : remainingDeadlines) (price : remainingPrices) bidders continuation =
  When
    [
      -- The winning bidder makes their deposit.
      Case (Deposit bidder bidder ada price)
        $ Pay bidder (Party seller) ada price
        $ Pay seller (Party bidder) assetToken assetAmount
          Close
    |
      bidder <- bidders
    ]
    -- Continue the bidding if no one bids in this round.
    deadline
      $ makeBids assetToken remainingDeadlines remainingPrices bidders continuation
