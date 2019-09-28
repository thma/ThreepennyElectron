module Calc (
    State(),
    populate, display
    ) where

import           Data.Default (Default (def))

data Operation = Add | Sub | Mul | Div deriving (Show, Eq)

data Command = Digit Char
             | Dot
             | Operation Operation
             | Flush | Clear | ClearError
             deriving (Show, Eq)

type Raw = (Double, Bool)

data State = EnteringA     Raw                      -- raw A
           | EnteredAandOp Double  Operation        -- A, Op
           | EnteringB     Double  Operation Raw    -- A, Op, raw B
           | Calculated    Double  Operation Double -- A, Op, B
           | Error         Double  String           -- A, Message
           deriving Show


instance Default State where
  def = EnteringA (0, False)


display :: State -> String
display s =
  case s of
    EnteringA     a     -> show (fromRaw a)
    EnteredAandOp a _   -> show a
    EnteringB     _ _ b -> show (fromRaw b)
    Calculated    a _ _ -> show a
    Error         _ msg -> msg


fromRaw :: Raw -> Double
fromRaw = fst

asRaw :: Double -> Raw
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


populate :: String -> State -> State
populate i =
  case parseInput i of
    Digit x      -> addDigit x
    Dot          -> addDot
    Operation op -> applyOp op
    cmd          -> applyCmd cmd


addDigit :: Char -> State -> State
addDigit x s =
  case s of
    (EnteringA a)        -> EnteringA (update a)
    (EnteringB a op b)   -> EnteringB a op (update b)
    (EnteredAandOp a op) -> EnteringB a op (asRaw x')
    Calculated {}        -> EnteringA (asRaw x')
    _                    -> s
  where
    update (a, False) = (a * 10 + x', False)
    update (a, True)  = let (a', b) = properFraction a
                        in (fromInteger a' + (x' + b / 10) / 10, True)

    x' = read [x] :: Double


addDot :: State -> State
addDot s =
  case s of
    (EnteringA a)      -> EnteringA (dotted a)
    (EnteringB a op b) -> EnteringB a op (dotted b)
    _                  -> s
  where
    dotted (a, _) = (a, True)


tryToCalc :: Double -> Operation -> Double -- A op B
          -> (String -> a)                 -- error handler
          -> (Double -> a)                 -- result handler
          -> a
tryToCalc _ Div b mkError _  | b == 0 = mkError "Dision by Zero!"
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
    (Clear,      _)                 -> def
    (_,          Error _ _)         -> s
    (Flush,      EnteringA _)       -> s
    (Flush,      EnteredAandOp a _) -> Error a "Can't do this!"
    (Flush,      EnteringB  a op b) -> calc a op (fromRaw b)
    (Flush,      Calculated a op b) -> calc a op b
    _                               -> s
  where
    calc a op b = tryToCalc a op b
                  (Error a)
                  (\a' -> Calculated a' op b)
