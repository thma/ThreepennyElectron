{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} -- ensure that all possible Commands are covered in pattern matching
module Calc (
    State(..),
    populate, display, initialState, Operation(..), Command(..), Digit(..), toLabel 
    ) where

data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Show, Eq, Ord)

data Operation = Add | Sub | Mul | Div deriving (Show, Eq, Ord)

data Command = Digit Digit
             | Dot
             | Operation Operation
             | Flush | Clear | ClearError
             deriving (Show, Eq, Ord)

type Entering = (String, Bool) -- A tuple of the String representation of the entered digits and a flag signalling that **.** has already been pressed.

data State = EnteringA     Entering                      -- entering A
           | EnteredAandOp Double  Operation        -- A, Op
           | EnteringB     Double  Operation Entering    -- A, Op, entering B
           | Calculated    Double  Operation Double -- A, Op, B
           | Error         Double  String           -- A, Message
           deriving (Show, Eq)
  
initialState :: State
initialState = EnteringA ("0", False)

display :: State -> String
display s =
  case s of
    EnteringA     a     -> trim $ fst a
    EnteredAandOp a _   -> show a
    EnteringB     _ _ b -> trim $ fst b
    Calculated    a _ _ -> show a
    Error         _ msg -> msg

trim :: String -> String
trim "0"           = "0"
trim s@('0':'.':_) = s
trim ('0':tail)    = tail
trim x             = x
   
fromEntering :: Entering -> Double
fromEntering = read . fst

asEntering :: Double -> Entering
asEntering x = (show x, x /= (fromInteger . truncate) x) 

parseInput :: String -> Command
parseInput x = case x of
  "0"  -> Digit Zero
  "1"  -> Digit One
  "2"  -> Digit Two
  "3"  -> Digit Three
  "4"  -> Digit Four
  "5"  -> Digit Five
  "6"  -> Digit Six
  "7"  -> Digit Seven
  "8"  -> Digit Eight
  "9"  -> Digit Nine
  "."  -> Dot
  "+"  -> Operation Add
  "-"  -> Operation Sub
  "*"  -> Operation Mul
  "/"  -> Operation Div
  "="  -> Flush
  "C"  -> Clear
  "CE" -> ClearError
  _    -> undefined

toLabel :: Command -> String
toLabel c = case c of
  Digit Zero    -> "0"
  Digit One     -> "1"
  Digit Two     -> "2"
  Digit Three   -> "3"
  Digit Four    -> "4"
  Digit Five    -> "5"
  Digit Six     -> "6"
  Digit Seven   -> "7"
  Digit Eight   -> "8"
  Digit Nine    -> "9"
  Dot           -> "."
  Operation Add -> "+"
  Operation Sub -> "-"
  Operation Mul -> "*"
  Operation Div -> "/"
  Flush         -> "="
  Clear         -> "C"
  ClearError    -> "CE"

populate :: String -> State -> State
populate i =
  case parseInput i of
    Digit x      -> addDigit x
    Dot          -> addDot
    Operation op -> applyOp op
    cmd          -> applyCmd cmd

addDigit :: Digit -> State -> State
addDigit x s =
  case s of
    (EnteringA a)        -> EnteringA (update a)
    (EnteringB a op b)   -> EnteringB a op (update b)
    (EnteredAandOp a op) -> EnteringB a op (num x, False)
    Calculated {}        -> EnteringA (num x, False)
    _ -> s
  where
    update (a, False) = (ccc a (num x), False)
    update (a, True)  = (ccc a (num x), True)   
    num x = toLabel (Digit x)
    ccc "0" "0" = "0" -- avoid to create leading 0 digits
    ccc a b     = a ++ b

addDot :: State -> State
addDot s =
  case s of
    (EnteringA a)            -> EnteringA (dotted a)
    (EnteringB a op b)       -> EnteringB a op (dotted b)
    _                        -> s
  where
    dotted (a, False) = (a ++ ".", True)
    dotted (a, True) = (a, True)

tryToCalc :: Double -> Operation -> Double -- A op B
          -> (String -> a)                 -- error handler
          -> (Double -> a)                 -- result handler
          -> a 
tryToCalc _ Div b mkError _  | b == 0 = mkError "Division by Zero!"
tryToCalc a op  b _ mkResult =
  let f = case op of
            Add -> (+)
            Sub -> (-)
            Mul -> (*)
            Div -> (/)
  in mkResult $ f a b

applyOp :: Operation -> State -> State
applyOp op s =
  case s of
    (EnteringA a) -> EnteredAandOp (fromEntering a) op
    (EnteringB a op' b) -> tryToCalc a op' (fromEntering b)
                                     (Error a)
                                     (`EnteredAandOp` op)
    (EnteredAandOp a _) -> Error a "Can't do this!"
    (Calculated a _ _)  -> EnteredAandOp a op
    _ -> s

applyCmd :: Command -> State -> State
applyCmd cmd s =
  case (cmd, s) of
    (ClearError, Error a _)         -> EnteringA (asEntering a)
    (Clear,      _)                 -> initialState
    (_,          Error _ _)         -> s
    (Flush,      EnteringA _)       -> s
    (Flush,      EnteredAandOp a _) -> Error a "Can't do this!"
    (Flush,      EnteringB  a op b) -> calc a op (fromEntering b)
    (Flush,      Calculated a op b) -> calc a op b
    _                               -> s
  where
    calc a op b = tryToCalc a op b
                  (Error a)
                  (\a' -> Calculated a' op b)
