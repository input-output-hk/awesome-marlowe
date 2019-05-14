{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MyContract(contract) where

import           Control.Monad.State
import           Marlowe

-------------------------------------
-- Write your code below this line --
-------------------------------------

ops :: a -> (a -> a -> a) -> [a] -> a
ops x _  []       = x
ops _ op [y]      = y
ops x op (y : ys) = op y $ ops x op ys

ands :: [Observation] -> Observation
ands = ops TrueObs AndObs

ors :: [Observation] -> Observation
ors = ops FalseObs OrObs

boths :: [Contract] -> Contract
boths = ops Null Both

adds :: [Money] -> Money
adds = ops (ConstMoney 0) AddMoney

choices :: (a -> Observation) -> (a -> Contract) -> [a] -> Contract
choices _ _ []       = Null
choices p c (x : xs) = Choice (p x) (c x) $ choices p c xs

atTime :: Timeout -> Contract -> Contract
atTime when
    | when == 0 = id
    | otherwise = When FalseObs when Null

valueLE :: Money -> Money -> Observation
valueLE = flip ValueGE

valueGT :: Money -> Money -> Observation
valueGT x y = ValueGE x $ AddMoney y $ ConstMoney 1

valueLT :: Money -> Money -> Observation
valueLT = flip valueGT

newtype M a = M (State (Integer, Integer, Integer) a)
    deriving (Functor, Applicative, Monad)

runM :: M a -> a
runM (M m) = evalState m (0, 0, 0)

identCC :: M IdentCC
identCC = M $ do
    (x, y, z) <- get
    put (x + 1, y, z)
    return $ IdentCC x

identPay :: M IdentPay
identPay = M $ do
    (x, y, z) <- get
    put (x, y + 1, z)
    return $ IdentPay y

identChoice :: M IdentChoice
identChoice = M $ do
    (x, y, z) <- get
    put (x, y, z + 1)
    return $ IdentChoice z

choicesM :: (a -> Observation) -> (a -> M Contract) -> [a] -> M Contract
choicesM _ _ []       = return Null
choicesM p c (x : xs) = Choice (p x) <$> c x <*> choicesM p c xs

roundM :: Maybe (IdentChoice, Integer) -> Int -> Int -> Int -> Timeout -> Timeout -> Timeout -> M Contract
roundM mcp index personCount maxRounds bidTime commitTime payTime
    | index > maxRounds = return Null
    | otherwise         = atTime (beginTime + bidTime) <$> do
        xs <- replicateM personCount identChoice
        choicesM
            (hasLowestBid mcp xs)
            (cont mcp xs)
            ([1 .. fromIntegral personCount] ++ [0])
  where
    beginTime = (bidTime + commitTime) * (fromIntegral index - 1)

    choice xs p = xs !! (fromIntegral p - 1)

    bid xs p = MoneyFromChoice (choice xs p) p $ ConstMoney 0

    hasChosen xs p =
        let c = choice xs p
        in  AndObs
                (PersonChoseSomething c p)
                (valueGT (bid xs p) $ ConstMoney 0)

    hasBid Nothing         xs p = hasChosen xs p
    hasBid (Just (c', p')) xs p =
        AndObs
            (hasChosen xs p)
            (valueLT
                (bid xs p)
                (MoneyFromChoice c' p' $ ConstMoney 0))

    hasLowestBid _   _  0 = TrueObs
    hasLowestBid mcp xs p =
        AndObs
            (hasBid mcp xs p)
            (ands [NotObs (hasBid mcp xs q) `OrObs` valueLE (bid xs p) (bid xs q) | q <- [p + 1 .. fromIntegral personCount]])

    buyer = 1 + fromIntegral personCount

    cont mcp _  0 = roundM mcp (index + 1) personCount maxRounds bidTime commitTime payTime
    cont _ xs p   = do
        cc  <- identCC
        pay <- identPay
        c   <- roundM (Just (choice xs p, p)) (index + 1) personCount maxRounds bidTime commitTime payTime
        let end = beginTime + bidTime + commitTime + payTime

        return $ CommitCash
            cc
            buyer
            (bid xs p)
            end
            (beginTime + bidTime + commitTime)
            (Pay pay buyer p (AvailableMoney cc) end Null)
            c

mkAuction :: Int -> Int -> Timeout -> Timeout -> Timeout -> Contract
mkAuction personCount maxRounds bidTime commitTime = runM . roundM Nothing 1 personCount maxRounds bidTime commitTime

contract :: Contract
contract = mkAuction
      2 -- person count
      3 -- max rounds
      2 -- bid time
      2 -- commit time
      5 -- pay time
