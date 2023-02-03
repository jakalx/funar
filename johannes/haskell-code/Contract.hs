module Contract where

{-

1. einfaches Beispiel:

-- Zero-Coupon-Bond:

"Ich bekomme am 24.12.2023 100 Euro."

Ziel: Simplifizieren der Domäne

Bestandteile:
- Aktion
- Datum/Zeitpunkt
- Person
- Währung
- Menge

-}

newtype Date = MkDate String
    deriving (Ord, Show, Eq)

data Currency = EUR | GBP | USD | YEN
    deriving (Eq, Show)

type Amount = Float