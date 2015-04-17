module MppException where
-- Exception monad.
--  Ok a     : a is a (partially or wholely complete) AST.
--  Except e : e is a string that records the location and source of
--                a parse error.
data E a = Ok a | Except String
bindE :: E a -> (a -> E b) -> E b
m `bindE` k =
    case m of
        Ok a -> k a
        Except e -> Except e
returnE :: a -> E a
returnE a = Ok a

-- Construct an instance of the Exception monad with the given message.
exceptE :: String -> E a
exceptE err = Except err

isOkay (Ok a)       = True
isOkay (Except e)   = False

tkVal (Ok a) = a
tkVal (Except e) = error "Can't take a value out of an exception"

err (Except e) = e

allOkay [] = True
allOkay ((Except _):rst) = False
allOkay ((Ok _):rst) = allOkay rst

firstErr :: [E a] -> String
firstErr [] = error "shouldn't happen"
firstErr ((Except e):rst) = e
firstErr ((Ok _):rst) = firstErr rst

allErr [] = ""
allErr ((Except e):rst) = e ++ "\n\n" ++ allErr rst
allErr ((Ok _):rst) = allErr rst

