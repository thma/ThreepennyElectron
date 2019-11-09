{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} -- ensure that all possible Commands are covered in pattern matching
module Calc (
    State(..),
    processCommand, parseInput, populate, toString, initialState, Operation(..), Command(..), Digit(..), lbl
    ) where

-- | a data type representing all possible Commands that can be executed by the Calculator
data Command = Digit Digit         -- ^ a Digit
             | Dot                 -- ^ a Dot
             | Operation Operation -- ^ an arithmetic operation
             | Flush               -- ^ evaluate the current operation (i.e. pressing '=')
             | Clear               -- ^ clear the current calculator state
             | ClearError          -- ^ clear the last error
             deriving (Show, Eq, Ord)

-- | a data type representing all digits
data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Show, Eq, Ord)

-- | a data type representing all possible arithmetic operations
data Operation = Add | Sub | Mul | Div deriving (Show, Eq, Ord)

-- | a data type representing all possible states of the calculator (see [calculator section](README.md#the-calculator))
data State = EnteringA     Entering                   -- ^ entering register A
           | EnteredAandOp Double  Operation          -- ^ A, Op
           | EnteringB     Double  Operation Entering -- ^ A, Op, entering register B
           | Calculated    Double  Operation Double   -- ^ A, Op, B
           | Error         Double  String             -- ^ A, Message
           deriving (Show, Eq)

-- | Entering is a tuple used while entering numbers. It consists of
type Entering = (String, Bool) -- A tuple of the String representation of the entered digits
                               -- and a flag signalling that Dot was already pressed.

-- | the initial state of the calculator
initialState :: State
initialState = EnteringA ("0", False)

-- | returns the String representation of a State
toString :: State -> String
toString s =
  case s of
    EnteringA     a     -> trim $ fst a
    EnteredAandOp a _   -> show a
    EnteringB     _ _ b -> trim $ fst b
    Calculated    a _ _ -> show a
    Error         _ msg -> msg

-- | handle leading zero characters
trim :: String -> String
trim "0"           = "0"
trim s@('0':'.':_) = s
trim ('0':tl)      = tl
trim x             = x
   
-- | convert an Entering tuple to a Double   
fromEntering :: Entering -> Double
fromEntering = read . fst

-- | convert a Double to an Entering tuple
asEntering :: Double -> Entering
asEntering x = (show x, x /= (fromInteger . truncate) x) 

-- | parse a Command from an input string
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

-- | return the button label for a calculator Command
lbl :: Command -> String
lbl c = case c of
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

-- | process a calculator command. That is: compute a calculator state transition    
processCommand :: Command -> State -> State
processCommand cmd = case cmd of
  Digit x      -> addDigit x
  Dot          -> addDot
  Operation op -> applyOp op
  command      -> applyCmd command

-- | a helper function that allows to parse a command from a string and then execute it
populate :: String -> State -> State
populate = processCommand . parseInput

-- | add a digit
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
    num i = lbl (Digit i)
    ccc "0" "0" = "0" -- avoid to create leading 0 digits
    ccc a b     = a ++ b

-- | add a dot
addDot :: State -> State
addDot s =
  case s of
    (EnteringA a)            -> EnteringA (dotted a)
    (EnteringB a op b)       -> EnteringB a op (dotted b)
    _                        -> s
  where
    dotted (a, False) = (a ++ ".", True)
    dotted (a, True) = (a, True)

-- | try to evaluate the current operation
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

-- | apply an arithmetic operation
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

-- | apply a unary command (C, CE, or =)
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
