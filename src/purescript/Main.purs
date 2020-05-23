module Main where

import Concur.React.DOM (div_, h2_, hr', text, button)
import Concur.React.Props (onClick)
import Concur.React.Run (runWidgetInDom)
import Control.Bind (bind, discard)
import Control.MultiAlternative (orr)
import Data.Function (($))
import Data.Unit (Unit, unit)
import Effect (Effect)
import Concur.Core (Widget)
import Concur.React (HTML)

import Widgets.Calc as Calc

main :: Effect Unit
main = do
    runWidgetInDom "app" $ orr
        [ widget Calc.widget "Calcolatrice" ]
    where
        widget w s = orr
            [ hr'
            , h2_ [] $ text s
            , div_ [] w
            ]
