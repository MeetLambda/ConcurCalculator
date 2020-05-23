module Widgets.Calc where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Applicative (pure)
import Concur.React.DOM (button, text, table', tr', td', tbody', p')
import Concur.React.Props (onClick)
import Control.Bind (bind)
import Control.MultiAlternative (orr)
import Data.Array (fromFoldable)
import Data.EuclideanRing (div)
import Data.Function (($))
import Data.Functor ((<$), map)
import Data.List (List(..), uncons, (:), reverse)
import Data.Maybe (Maybe(..))
import Data.Ring((-))
import Data.Semiring((*), (+))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import React (ReactElement)

-- Possible actions emitted by the Calculator buttons
data CalculatorAction = Plus | Minus | Times | Div | Enter | Clear | Digit Int

-- Button pad widget
calcButtonsWidget :: Widget HTML CalculatorAction
calcButtonsWidget = table' $ pure $ tbody' $
  [ tr' [d 7, d 8, d 9, opDiv]
  , tr' [d 4, d 5, d 6, opTimes]
  , tr' [d 1, d 2, d 3, opMinus]
  , tr' [d 0, ent, cls, opPlus]
  ]
  where
    d n     = but (Digit n) (show n)
    ent     = but Enter "⏎"
    cls     = but Clear "C"
    opDiv   = but Div   "/"
    opTimes = but Times "*"
    opMinus = but Minus "-"
    opPlus  = but Plus  "+"
    but x s = x <$ td' [button [onClick] [text s]]

-- Postfix calculation
calc :: List Int -> CalculatorAction -> Tuple (List Int) Int
calc arr axn = case uncons arr, axn of
  Just {head: x, tail: xs}, Digit d -> new (x*10+d) xs
  Nothing                 , Digit d -> new d arr
  _                       , Clear   -> Tuple Nil 0
  _                       , Enter   -> Tuple (0:arr) 0
  Nothing                 , _       -> err
  Just {head: x, tail: xs}, _ -> case uncons xs, axn of
    Just {head: y, tail: ys}, Plus  -> new (x+y) ys
    Just {head: y, tail: ys}, Minus -> new (x-y) ys
    Just {head: y, tail: ys}, Times -> new (x*y) ys
    Just {head: y, tail: ys}, Div   -> new (y `div` x) ys
    _, _ -> err
  where
    err = Tuple arr 0
    new n s = Tuple (n:s) n

listStack :: forall a. List Int -> Widget (Array ReactElement) a
listStack xs = orr $ fromFoldable $ map (\i -> p' [text (show i)]) xs

widget :: forall a. Widget HTML a
widget = go 0 (Cons 0 Nil)
  where
    go :: forall a. Int -> List Int -> Widget (Array ReactElement) a
    go n s = do
    --   a <- orr [listStack s, p' [text (show n)], calcButtonsWidget]
      a <- orr [listStack $ reverse s, calcButtonsWidget]
      let Tuple s' n' = calc s a
      go n' s'