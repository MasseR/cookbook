{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module MyLib (defaultMain) where

import           Miso
import           Miso.String

import           Control.Lens  (Lens', lens, (+=), (-=), (^.), _Just, iso, Iso', from)
import           Data.Name     (name, _Name)

import           Data.Cookbook (Food)

_MisoString :: ToMisoString a => Iso' MisoString a
_MisoString = iso fromMisoString toMisoString

data Model
  = Model { _counter :: Int
          , _food    :: Maybe Food
          }
           deriving (Eq)

counter :: Lens' Model Int
counter = lens _counter (\m x -> m{_counter=x})

food :: Lens' Model (Maybe Food)
food = lens _food (\m x -> m{_food=x})

data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

defaultMain :: IO ()
defaultMain = startApp App {..}
  where
    initialAction = SayHelloWorld
    model = Model 0 Nothing
    update = fromTransition . updateModel
    view = viewModel
    events = defaultEvents
    subs = []
    mountPoint = Nothing

updateModel :: Action -> Transition Action Model ()
updateModel = \case
  AddOne -> counter += 1
  SubtractOne -> counter -= 1
  NoOp -> pure ()
  SayHelloWorld -> scheduleIO_ (consoleLog "Hello world")

viewModel :: Model -> View Action
viewModel x = section_ [class_ "section"] [
      link_ [ rel_ "stylesheet", type_ "text/css", href_ bulma ]
    , script_ [ defer_ "", src_ fontawesome ] []
    , div_ [class_ "container"] [
        h1_ [ class_ "title"] [ text "Hello world" ]
      , button_ [ onClick AddOne ] [ text "+" ]
      , text . ms $ x ^. counter
      , button_ [ onClick SubtractOne ] [ text "-" ]
      , div_ [class_ "columns"] [ viewFood (x ^. food) ]
      ]
  ]
  where
    bulma = "https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css"
    fontawesome = "https://use.fontawesome.com/releases/v5.3.1/js/all.js"

viewFood :: Maybe Food -> View Action
viewFood f = div_ [ class_ "column is-two-thirds" ] [
    textField "Name" (f ^. _Just . name . _Name . from _MisoString)
  ]
  where
    textField = field "text"
    field fieldType lbl  val = div_ [ class_ "field" ] [
        label_ [ class_ "label" ] [ text lbl ]
      , div_ [ class_ "control" ] [ input_ [ class_ "input", type_ fieldType, value_ val] ]
      ]
