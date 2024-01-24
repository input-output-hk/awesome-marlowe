{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MyContract(contract) where

import           Control.Monad.State
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
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

roundM :: Maybe (IdentChoice, Integer) -> Map (Int, Integer) IdentChoice -> Int -> Int -> Int -> Timeout -> Timeout -> Timeout -> M Contract
roundM mcp choices index personCount maxRounds bidTime commitTime payTime
    | index > maxRounds = return Null
    | otherwise         = atTime (beginTime + bidTime) <$> do
        choicesM
            (hasLowestBid mcp)
            (cont mcp)
            ([1 .. fromIntegral personCount] ++ [0])
  where
    beginTime = (bidTime + commitTime) * (fromIntegral index - 1)

    choice p = choices Map.! (index, p)

    bid p = MoneyFromChoice (choice p) p $ ConstMoney 0

    hasChosen p =
        let c = choice p
        in  AndObs
                (PersonChoseSomething c p)
                (valueGT (bid p) $ ConstMoney 0)

    hasBid Nothing         p = hasChosen p
    hasBid (Just (c', p')) p =
        AndObs
            (hasChosen p)
            (valueLT
                (bid p)
                (MoneyFromChoice c' p' $ ConstMoney 0))

    hasLowestBid _   0 = TrueObs
    hasLowestBid mcp p =
        AndObs
            (hasBid mcp p)
            (ands [NotObs (hasBid mcp q) `OrObs` valueLE (bid p) (bid q) | q <- [p + 1 .. fromIntegral personCount]])

    buyer = 1 + fromIntegral personCount

    cont mcp 0 = roundM mcp choices (index + 1) personCount maxRounds bidTime commitTime payTime
    cont _   p = do
        cc  <- identCC
        pay <- identPay
        c   <- roundM (Just (choice p, p)) choices (index + 1) personCount maxRounds bidTime commitTime payTime
        let end = beginTime + bidTime + commitTime + payTime

        return $ CommitCash
            cc
            buyer
            (bid p)
            (beginTime + bidTime + commitTime)
            end
            (Pay pay buyer p (AvailableMoney cc) end Null)
            c

mkAuction :: Int -> Int -> Timeout -> Timeout -> Timeout -> Contract
mkAuction personCount maxRounds bidTime commitTime payTime = runM $ do
    choices <- foldM f Map.empty [(r, p) | r <- [1 .. maxRounds], p <- [1 .. fromIntegral personCount]]
    roundM Nothing choices 1 personCount maxRounds bidTime commitTime payTime
  where
    f m (r, p) = do
        c <- identChoice
        return $ Map.insert (r, p) c m

contract :: Contract
contract = mkAuction
      3 -- person count
      2 -- max rounds
      2 -- bid time
      2 -- commit time
      5 -- pay time
