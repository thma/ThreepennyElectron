module Calc (
    State(..),
    populate, display, initialState, Operation(..), Command(..), commandsToSymbols, symbolsToCommands, Symbol, sym
    ) where

import           Data.BigDecimal
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)

data Operation = Add | Sub | Mul | Div deriving (Show, Eq, Ord)

data Command = Digit Char
             | Dot
             | Operation Operation
             | Flush | Clear | ClearError | Pi
             deriving (Show, Eq, Ord)

allCommands :: [Command]
allCommands = [Digit '0', Digit '1', Digit '2', Digit '3', Digit '4', Digit '5', Digit '6', Digit '7', Digit '8', Digit '9',
               Operation Add, Operation Sub, Operation Mul, Operation Div, Dot, Flush, Clear, ClearError, Pi]

type Symbol = String
allSymbols :: [Symbol]
allSymbols = ["0","1","2","3","4","5","6","7","8","9","+","-","*","/",".","=","C","CE","pi"]

commandsToSymbols :: Map Command Symbol
commandsToSymbols = Map.fromList $ zip allCommands allSymbols

symbolsToCommands :: Map Symbol Command
symbolsToCommands = Map.fromList $ zip allSymbols allCommands

symFor :: Command -> Symbol
symFor c = fromMaybe (fail "invalid Command " ++ show c) (Map.lookup c commandsToSymbols)

comFor :: Symbol -> Maybe Command
comFor s = Map.lookup s symbolsToCommands

sym :: String -> Symbol
sym s = 
  case comFor s of
    Just c ->  symFor c
    Nothing -> fail "invalid symbol " ++ s


type Raw = (BigDecimal, Bool)

data State = EnteringA     Raw                      -- raw A
           | EnteredAandOp BigDecimal  Operation        -- A, Op
           | EnteringB     BigDecimal  Operation Raw    -- A, Op, raw B
           | Calculated    BigDecimal  Operation BigDecimal -- A, Op, B
           | Error         BigDecimal  String           -- A, Message
           deriving (Show, Eq)
  
initialState :: State
initialState = EnteringA (BigDecimal 0 0, False)


display :: State -> String
display s =
  case s of
    EnteringA     a     -> toString (fromRaw a)
    EnteredAandOp a _   -> toString a
    EnteringB     _ _ b -> toString (fromRaw b)
    Calculated    a _ _ -> toString a
    Error         _ msg -> msg


fromRaw :: Raw -> BigDecimal
fromRaw = fst

asRaw :: BigDecimal -> Raw
asRaw x = (x, x /= (fromInteger . truncate) x)

parseInput :: String -> Command
parseInput (x:_) | x `elem` "0123456789" = Digit x
parseInput x =
  case x of
    "."  -> Dot
    "+"  -> Operation Add
    "-"  -> Operation Sub
    "*"  -> Operation Mul
    "/"  -> Operation Div
    "="  -> Flush
    "C"  -> Clear
    "CE" -> ClearError
    "pi" -> Pi


populate :: String -> State -> State
populate i =
  case parseInput i of
    Digit x      -> addDigit x
    Dot          -> addDot
    Operation op -> applyOp op
    cmd          -> applyCmd cmd


addDigit :: Char -> State -> State
addDigit x s =
  case s
    -- (EnteringA (pi, _))      -> s
        of
    (EnteringA a)        -> EnteringA (update a)
    -- (EnteringB a op (pi, _)) -> s
    (EnteringB a op b)   -> EnteringB a op (update b)
    (EnteredAandOp a op) -> EnteringB a op (asRaw x')
    Calculated {}        -> EnteringA (asRaw x')
    _ -> s
  where
    update (a, False) = (fromString $ toString a ++ [x], False)
    update (a, True) =
      let BigDecimal intValue scale = a
          intValue' = 10*intValue + xInt
          scale'    = scale + 1
       in (BigDecimal intValue' scale', True)
    xInt = read [x] :: Integer
    x' = fromInteger xInt

addDot :: State -> State
addDot s =
  case s of
    -- (EnteringA (pi, _))      -> s
    (EnteringA a)            -> EnteringA (dotted a)
    --(EnteringB a op (pi, _)) -> s
    (EnteringB a op b)       -> EnteringB a op (dotted b)
    _                        -> s
  where
    dotted (a, _) = (a, True)


tryToCalc :: BigDecimal -> Operation -> BigDecimal -- A op B
          -> (String -> a)                 -- error handler
          -> (BigDecimal -> a)             -- result handler
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
