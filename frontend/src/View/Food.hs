module View.Food where

import           Data.Cookbook

import           Control.Lens
import           Control.Lens.MisoString

import           Data.Name

import           Miso
import           Miso.String

newtype Model = Model { _food :: Food }
  deriving (Eq)

food :: Lens' Model Food
food = lens _food (\x f -> x{_food = f})

data Action
  = UpdateName MisoString
  deriving (Eq)

model :: Model
model = Model (Food (Name "") [])

updateModel :: Action -> Model -> Effect Action Model
updateModel event m =
  case event of
       UpdateName nm -> noEff (m & food . name .~ (Name (fromMisoString nm)))

viewModel :: Model -> View Action
viewModel m = div_ [ class_ "columns" ] [ viewForm m ]

viewForm :: Model -> View Action
viewForm m = div_ [ class_ "column is-two-thirds" ] [
    field "text" UpdateName "Name" (m ^. food . name . _Name . from _MisoString)
  , p_ [] [text (m ^. food . name . _Name . from _MisoString)]
  ]
  where
    field fieldType act lbl  val = div_ [ class_ "field" ] [
        label_ [ class_ "label" ] [ text lbl ]
      , div_ [ class_ "control" ] [ input_ [ onInput act, class_ "input", type_ fieldType, value_ val] ]
      ]
