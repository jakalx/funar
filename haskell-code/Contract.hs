{-# LANGUAGE InstanceSigs #-}
-- |
module Contract where

import Prelude hiding (negate)

{-
A contract is implicitly defined between bank and customer.
-}

type Amount = Double

data Currency = EUR | GBP | YEN | USD
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

newtype Date = Date String
  deriving (Eq, Ord, Read, Show)

{-
1. idea to model this is a sum type:
   - zero bond / zero-coupon bond
     "I get 100€ on 2024-12-24"
   - put ...
   - call ...
data Contract = ZeroCouponBond Amount Currency Date
              | Put ...
              | Call ...
              | ...
   contracts will be changed over time
-}

{-
2. devide the contract into atomic ideas, e.g.
   - Amount
   - Currency
   - Date

   mit Selbstbezug / Combinators

3. repeat with larger example
  Currency Swap
   - I'll get 100€ on ... AND
   - I'll pay 70 USD on ...

   mit Selbstbezug / Combinators
-}

-- |
--
-- $setup
-- zcb1 = AtDueDate (Date "2024-12-24") (Times 100 (One EUR))
-- c2 = AtDueDate (Date "2024-02-01") (AtDueDate (Date "2024-02-02") (Times 5 (One EUR)))
--
data Contract = One Currency                -- I'll get 1€ now
              | Times Amount Contract       -- I'll get x * xx
              | AtDueDate Date Contract     -- I'll get xx on 2024-12-24
              | And Contract Contract       -- combine two contracts
              | Negate Contract             -- changes the direction of money flow
              | Zero
              deriving (Eq, Ord, Show, Read)

instance Semigroup Contract where
  (<>) :: Contract -> Contract -> Contract
  (<>) = both

instance Monoid Contract where
  mempty = Zero

zeroCouponBond :: Amount -> Currency -> Date -> Contract
zeroCouponBond amt cur date = AtDueDate date $ Times amt $ One cur

-- denotational semantic

data Direction = Long   -- receive
               | Short  -- pay
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Payment = Payment Direction Date Amount Currency
  deriving (Eq, Ord, Show)

-- | calculate the payments until the given date and a new contract that represents the remaining payments\
--
-- >>> denotation (Date "2024-12-23") $ Times 100 (And (One EUR) (AtDueDate (Date "2024-12-24") (One EUR)))
-- ([Payment Long (Date "2024-12-23") 100.0 EUR],Times 100.0 (AtDueDate (Date "2024-12-24") (One EUR)))
--
-- >>> denotation (Date "2024-12-23") $ One EUR
-- ([Payment Long (Date "2024-12-23") 1.0 EUR],Zero)
denotation :: Date -> Contract -> ([Payment], Contract)
denotation now (One currency) =
  ([Payment Long now 1 currency], mempty)
denotation now (Times amount contract) =
  let
    (payments, remainingContract) = denotation now contract
  in (multiplyPayment amount <$> payments, times amount remainingContract)
denotation now contract@(AtDueDate date rest) =
  if now >= date
  then
    denotation now rest
  else
    ([], contract)
denotation now (And a b) = denotation now a <> denotation now b
denotation now (Negate contract) =
  let
    (payments, rest) = denotation now contract
  in (negatePayment <$> payments, negate rest)

denotation now Zero = mempty

both :: Contract -> Contract -> Contract
both Zero x = x
both x Zero = x
both x y = And x y

negate :: Contract -> Contract
negate Zero = Zero
negate x = Negate x

times :: Amount -> Contract -> Contract
times _ Zero = Zero
times amt x = Times amt x

at :: Date -> Contract -> Contract
at _ Zero = Zero
at date x = AtDueDate date x

one :: Currency -> Contract
one = One

zero :: Contract
zero = Zero

multiplyPayment :: Amount -> Payment -> Payment
multiplyPayment factor (Payment direction date previousAmount currency) =
  Payment direction date (factor * previousAmount) currency

negatePayment :: Payment -> Payment
negatePayment (Payment Long date amount currency) = Payment Short date amount currency
negatePayment (Payment Short date amount currency) = Payment Long date amount currency
