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
-- | Marlowe challenge problems for hackathons.
--
-----------------------------------------------------------------------------


module Main (
-- * Entry point
  main
) where


import Data.Aeson (encodeFile)

import qualified EnglishAuction


-- | Generate the contracts.
main :: IO ()
main = encodeFile "solutions/EnglishAuction.json" EnglishAuction.example
