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

adds :: [Money] -> Money
adds = ops (ConstMoney 0) AddMoney

choices :: (a -> Observation) -> (a -> Contract) -> [a] -> Contract
choices _ _ []       = Null
choices p c (x : xs) = Choice (p x) (c x) $ choices p c xs

ident :: Int -> Int -> Integer -> Integer
ident personCount index person = fromIntegral ((index - 1) * personCount) + person

personBid :: Int -> Int -> Integer -> Money
personBid personCount index person = adds [AvailableMoney $ IdentCC $ ident personCount i person | i <- [1 .. index]]

isWinner :: Int -> Int -> Integer -> Observation
isWinner personCount rounds person =
    AndObs
        (ands [ValueGE bid $ AddMoney (personBid' q) $ ConstMoney 1 | q <- [1 .. person - 1]])
        (ands [ValueGE bid $ personBid' q | q <- [person + 1 .. fromIntegral personCount]])
  where
    personBid' = personBid personCount rounds
    bid = personBid' person

personStep :: Timeout -> Int -> Int -> Integer -> Contract
personStep maxTime personCount index person =
    When
        FalseObs
        (index' - 1)
        Null
        ( When
            (AndObs
                (PersonChoseSomething (IdentChoice ident') person)
                (ValueGE bid $ ConstMoney 1))
            index'
            (CommitCash
                (IdentCC ident')
                person
                bid
                index'
                maxTime
                Null
                Null)
            Null)
  where
    ident' = ident personCount index person

    index' = fromIntegral index

    bid = MoneyFromChoice (IdentChoice ident') person $ ConstMoney 0

step :: Timeout -> Int -> Int -> Contract
step maxTime personCount index = boths [personStep maxTime personCount index p | p <- [1 .. fromIntegral personCount]]

getMaxTime :: Timeout -> Int -> Timeout
getMaxTime payTime rounds = payTime + fromIntegral rounds

steps :: Timeout -> Int -> Int -> Contract
steps payTime personCount rounds = boths [step (getMaxTime payTime rounds) personCount i | i <- [1 .. rounds]]

finalizeAuction :: Timeout -> Int -> Int -> Contract
finalizeAuction payTime personCount rounds =
    When
        FalseObs
        (fromIntegral rounds)
        Null
        (choices
            (isWinner personCount rounds)
            (\p ->
                Pay
                    (IdentPay p)
                    p
                    (1 + fromIntegral personCount)
                    (personBid personCount rounds p)
                    (getMaxTime payTime rounds)
                    Null)
            [1 .. fromIntegral personCount])

mkAuction :: Timeout -> Int -> Int -> Contract
mkAuction payTime personCount rounds =
    Both
        (steps payTime personCount rounds)
        (finalizeAuction payTime personCount rounds)

contract :: Contract
contract = mkAuction
                  3 -- pay time
                  3 -- person count
                  4 -- rounds
