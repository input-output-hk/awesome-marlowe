module MyContract(contract) where

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

boths :: [Contract] -> Contract
boths = ops Null Both

choices :: (a -> Observation) -> (a -> Contract) -> [a] -> Contract
choices _ _ []       = Null
choices p c (x : xs) = Choice (p x) (c x) $ choices p c xs

mkBid :: Timeout -> Timeout -> Timeout -> Cash -> Integer -> Contract
mkBid choiceTime bidTime maxTime minBid person =
    When
        (AndObs
            (PersonChoseSomething (IdentChoice person) person)
            (ValueGE
                (MoneyFromChoice (IdentChoice person) person (ConstMoney 0))
                (ConstMoney minBid')))
        choiceTime
        (CommitCash
            (IdentCC person)
            person
            (MoneyFromChoice (IdentChoice person) person (ConstMoney 0))
            bidTime
            maxTime
            Null
            Null)
        Null
  where
    minBid' = max minBid 1

mkBids :: Timeout -> Timeout -> Timeout -> Cash -> Int -> Contract
mkBids choiceTime bidTime maxTime minBid personCount =
    boths [mkBid choiceTime bidTime maxTime minBid $ fromIntegral p | p <- [1 .. personCount]]
bidValue :: Integer -> Money
bidValue person = AvailableMoney $ IdentCC person

hasBid :: Integer -> Observation
hasBid person = ValueGE (bidValue person) (ConstMoney 1)

hasHighestBid' :: Integer -> [Integer] -> Observation
hasHighestBid' person others =
    AndObs
        (hasBid person)
        (AndObs
            (ands [f q | q <- filter (< person) others])
            (ands [g q | q <- filter (> person) others]))
  where
    v = bidValue person
    f q = ValueGE v $ AddMoney (bidValue q) (ConstMoney 1)
    g q = ValueGE v $ bidValue q

hasHighestBid :: Int -> Integer -> Observation
hasHighestBid personCount person = hasHighestBid' person $ filter (/= person) [1 .. fromIntegral personCount]

hasSecondHighestBid :: Int -> Integer -> Integer -> Observation
hasSecondHighestBid personCount highest second = hasHighestBid' second $ filter (/= highest) [1 .. fromIntegral personCount]

finalizeAuction :: Timeout -> Timeout -> Int -> Contract
finalizeAuction bidTime maxTime personCount = choices (hasHighestBid personCount) f bidders
  where
    bidders = [1 .. fromIntegral personCount]

    f p = choices (hasSecondHighestBid personCount p) (g p) (filter (/= p) bidders)

    g p q =
        Pay
            (IdentPay $ p * fromIntegral personCount + q)
            p
            (1 + fromIntegral personCount)
            (AvailableMoney $ IdentCC q)
            maxTime
            (boths [RedeemCC (IdentCC r) Null | r <- bidders])

mkAuction :: Timeout -> Timeout -> Timeout -> Cash -> Int -> Contract
mkAuction choiceTime bidTime maxTime minBid personCount =
    Both
        (mkBids choiceTime bidTime maxTime minBid personCount)
        (When
            FalseObs
            bidTime
            Null
            (finalizeAuction bidTime maxTime personCount))

contract :: Contract
contract = mkAuction
      5 -- choice time
     10 -- bid time
     20 -- max time
    100 -- min bid
      3 -- person count
