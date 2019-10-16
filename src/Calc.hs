{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} -- ensure that all possible Commands are covered in pattern matching
module Calc (
    State(..),
    populate, display, initialState, Operation(..), Command(..), Digit(..), toLabel -- , commandsToSymbols, symbolsToCommands, Symbol, sym
    ) where

data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Show, Eq, Ord)

data Operation = Add | Sub | Mul | Div deriving (Show, Eq, Ord)

data Command = Digit Digit
             | Dot
             | Operation Operation
             | Flush | Clear | ClearError | Pi
             deriving (Show, Eq, Ord)

type Raw = (String, Bool)

data State = EnteringA     Raw                      -- raw A
           | EnteredAandOp Double  Operation        -- A, Op
           | EnteringB     Double  Operation Raw    -- A, Op, raw B
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
trim "0"        = "0"
trim str@('0':'.':tail) = str
trim ('0':tail) = tail
trim x          = x

   
fromRaw :: Raw -> Double
fromRaw = read . fst

asRaw :: Double -> Raw
asRaw x = (show x, x /= (fromInteger . truncate) x) 

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
  "pi" -> Pi
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
  Pi            -> "pi"

populate :: String -> State -> State
populate i =
  case parseInput i of
    Digit x      -> addDigit x
    Dot          -> addDot
    Operation op -> applyOp op
    cmd          -> applyCmd cmd


addDigit :: Digit -> State -> State
addDigit x s =
  case s
    -- (EnteringA (pi, _))      -> s
        of
    (EnteringA a)        -> EnteringA (update a)
    -- (EnteringB a op (pi, _)) -> s
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
    -- (EnteringA (pi, _))      -> s
    (EnteringA a)            -> EnteringA (dotted a)
    --(EnteringB a op (pi, _)) -> s
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
    (EnteringA a) -> EnteredAandOp (fromRaw a) op
    (EnteringB a op' b) -> tryToCalc a op' (fromRaw b)
                                     (Error a)
                                     (`EnteredAandOp` op)
    (EnteredAandOp a _) -> Error a "Can't do this!"
    (Calculated a _ _)  -> EnteredAandOp a op
    _ -> s


applyCmd :: Command -> State -> State
applyCmd cmd s =
  case (cmd, s) of
    (ClearError, Error a _)         -> EnteringA (asRaw a)
    (Clear,      _)                 -> initialState
    (_,          Error _ _)         -> s
    (Flush,      EnteringA _)       -> s
    (Flush,      EnteredAandOp a _) -> Error a "Can't do this!"
    (Flush,      EnteringB  a op b) -> calc a op (fromRaw b)
    (Flush,      Calculated a op b) -> calc a op b
    --(Pi,         EnteringA _)       -> EnteringA (asRaw pi)
    --(Pi,         EnteredAandOp a op) -> EnteringB  a op (pi, False)
    _                               -> s
  where
    calc a op b = tryToCalc a op b
                  (Error a)
                  (\a' -> Calculated a' op b)
