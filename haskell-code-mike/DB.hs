module DB where

import qualified Data.Map.Strict as Map 
import Data.Map (Map)

{-
put "Mike" 50
x = get "Mike"
put "Mike" (x+1)
y = get "Mike"
return (show (x + y))
-}

{-
data DBCommand a =
    Put String Integer 
  | Get String 
  | Return a

type DBProgram a = [DBCommand a]

p1 = [Put "Mike" 50, 
      Get "Mike"]
-}

data DB a =
    Get String         (Integer -> DB a) -- "callback" / "continuation"
  | Put String Integer (()      -> DB a)
  | Return a

p1 :: DB String
p1 = Put "Mike" 50 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x + y))))))

get :: String -> DB Integer 
get key = Get key Return -- (\ value -> Return value)

put :: String -> Integer -> DB ()
put key value = Put key value Return -- (\() -> Return ())

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key cont) next =
    Get key (\ value ->
               splice (cont value) next)
splice (Put key value cont) next =
    Put key value (\ () ->
                    splice (cont ()) next)
splice (Return result) next = next result


runDB :: DB a -> a