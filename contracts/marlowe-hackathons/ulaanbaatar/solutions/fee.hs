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

newtype M a = M (State (Integer, Integer, Integer) a)
    deriving (Functor, Applicative, Monad)

runM :: M a -> a
runM (M m) = evalState m (0, 0, 0)

identCC :: M IdentCC
identCC = M $ do
    (x, y, z) <- get
    put $ (x + 1, y, z)
    return $ IdentCC x

identPay :: M IdentPay
identPay = M $ do
    (x, y, z) <- get
    put $ (x, y + 1, z)
    return $ IdentPay y

identChoice :: M IdentChoice
identChoice = M $ do
    (x, y, z) <- get
    put $ (x, y, z + 1)
    return $ IdentChoice z

choicesM :: (a -> Observation) -> (a -> M Contract) -> [a] -> M Contract
choicesM _ _ []       = return Null
choicesM p c (x : xs) = Choice (p x) <$> c x <*> choicesM p c xs

stepM :: Cash -> Cash -> Int -> Int -> Timeout -> Timeout -> Int -> Maybe (Integer, Cash) -> M Contract
stepM fee increment personCount maxRounds roundLength endTime index mPrev
    | index > maxRounds = paySeller mPrev
    | otherwise = do
        xs <- forM bidders $ \p -> do
            cc <- identCC
            return (p, cc)
        c <- choicesM hasBid (cont xs) $ xs ++ [(0, IdentCC 0)]
        return $ Both
            (boths [CommitCash cc p (ConstMoney bid) end final Null Null | (p, cc) <- xs])
            (atTime end c)
  where
    start = (fromIntegral index - 1) * roundLength

    end = start + roundLength

    final = fromIntegral maxRounds * roundLength + endTime

    bidders = [1 .. fromIntegral personCount]

    bid = fee + fromIntegral index * increment

    hasBid (0, _)  = TrueObs
    hasBid (_, cc) = ValueGE (AvailableMoney cc) $ ConstMoney 1

    seller = 1 + fromIntegral personCount

    paySeller Nothing = return Null
    paySeller (Just (p, cash)) = do
        pay <- identPay
        return $ Pay pay p seller (ConstMoney cash) final Null

    cont _  (0, _) = paySeller mPrev
    cont xs (p, _) = case mPrev of
        Nothing        -> next
        Just (q, cash) -> do
            pay <- identPay
            let repay = Pay pay q q (ConstMoney cash) final Null
            next' <- next
            return $ Both repay next'
      where
        next = do
            step' <- stepM fee increment personCount maxRounds roundLength endTime (index + 1) $ Just (p, bid - fee)
            pay <- identPay
            let payFee = Pay pay p seller (ConstMoney fee) final Null
            return $ boths $ payFee : step' : [RedeemCC cc Null | (q, cc) <- xs, q > p]

mkAuction :: Cash -> Cash -> Int -> Int -> Timeout -> Timeout -> Contract
mkAuction fee increment personCount maxRounds roundLength endTime =
    runM $ stepM fee increment personCount maxRounds roundLength endTime 1 Nothing

contract :: Contract
contract = mkAuction
     10 -- fee
      1 -- increment
      3 -- person count
      3 -- max rounds
      2 -- round length
      5 -- end time
