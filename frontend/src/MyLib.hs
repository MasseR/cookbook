{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module MyLib (defaultMain) where

import           Miso
import           Miso.String

import           Control.Lens (Lens', lens, (+=), (-=), (^.))

data Model = Model { _counter :: Int }
           deriving (Eq)

counter :: Lens' Model Int
counter = lens _counter (\m x -> m{_counter=x})

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
    model = Model 0
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
      ]
  ]
  where
    bulma = "https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css"
    fontawesome = "https://use.fontawesome.com/releases/v5.3.1/js/all.js"
