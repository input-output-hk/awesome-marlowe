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
-- | Marlowe challenge problems for hackathons.
--
-----------------------------------------------------------------------------


module Main (
-- * Entry point
  main
) where


import Data.Aeson                (encodeFile)
import Language.Marlowe.Extended (prettyFragment)
import System.FilePath           ((</>), (<.>))

import qualified BiddingFee     (example)
import qualified DutchAuction   (example)
import qualified EnglishAuction (example)
import qualified FirstPriceBid  (example)
import qualified ReverseAuction (example)
import qualified SecondPriceBid (example)
import qualified StableCoin     (example)


-- | Generate the contracts.
main :: IO ()
main =
  do
    let
      examples =
        [
          ("EnglishAuction", EnglishAuction.example)
        , ("DutchAuction"  , DutchAuction.example  )
        , ("FirstPriceBid" , FirstPriceBid.example )
        , ("SecondPriceBid", SecondPriceBid.example)
        , ("ReverseAuction", ReverseAuction.example)
        , ("BiddingFee"    , BiddingFee.example    )
        , ("StableCoin"    , StableCoin.example    )
        ]
      writeMarlowe      name = writeFile  ("solutions" </> "marlowe"       </> name <.> "marlowe") . show . prettyFragment
      writeExtendedJson name = encodeFile ("solutions" </> "extended-json" </> name <.> "json"   )
    mapM_ (uncurry writeMarlowe     ) examples
    mapM_ (uncurry writeExtendedJson) examples
