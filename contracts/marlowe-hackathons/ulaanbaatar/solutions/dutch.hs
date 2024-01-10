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

ors :: [Observation] -> Observation
ors = ops FalseObs OrObs

boths :: [Contract] -> Contract
boths = ops Null Both

choices :: (a -> Observation) -> (a -> Contract) -> [a] -> Contract
choices _ _ []       = Null
choices p c (x : xs) = Choice (p x) (c x) $ choices p c xs

ident :: Int -> Int -> Integer -> Integer
ident personCount index person = person + fromIntegral ((index - 1) * personCount)

step :: Timeout -> Timeout -> Cash -> Int -> Int -> Contract -> Contract
step stepTime payTime bid index personCount cont =
    Both
        (boths $ f <$> bidders)
        (When
            FalseObs
            (offset + stepTime)
            Null
            (choices
                (\p -> if p > 0 then hasBid p else TrueObs)
                g
                (bidders ++ [0])))
  where

    offset = stepTime * (fromIntegral index - 1)

    bidders = [1 .. fromIntegral personCount]

    seller = 1 + fromIntegral personCount

    f p = CommitCash
        (cc p)
        p
        (ConstMoney bid)
        (offset + stepTime)
        (offset + stepTime + payTime)
        Null
        Null

    ident' = ident personCount index

    cc = IdentCC . ident'

    hasBid p = ValueGE (AvailableMoney $ cc p) (ConstMoney 1)

    g 0 = cont
    g p =
        Pay
            (IdentPay $ ident' p)
            p
            seller
            (AvailableMoney $ cc p)
            (offset + stepTime + payTime)
            Null

mkContract :: Timeout -> Timeout -> Cash -> Cash -> Cash -> Int -> Contract
mkContract stepTime payTime maxBid stepBid minBid personCount = go maxBid 1
  where
    go bid index
        | bid < minBid = Null
        | otherwise    = step stepTime payTime bid index personCount $ go (bid - stepBid) (index + 1)

contract :: Contract
contract = mkContract
      5 -- step time
      3 -- pay time
   1000 -- maximum bid
    100 -- step bid
    500 -- minimum bid
      3 -- person count
