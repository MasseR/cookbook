{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module MyLib (defaultMain) where

import           Miso
import           Miso.String

import           Control.Lens   (Lens', lens, (&), (+~), (-~), (.~), (^.))

import qualified View.Food      as Food

import           Data.Bifunctor (first)


data Model
  = Model { _counter :: Int
          , _food    :: Food.Model
          }
           deriving (Eq)

counter :: Lens' Model Int
counter = lens _counter (\m x -> m{_counter=x})

food :: Lens' Model Food.Model
food = lens _food (\m x -> m{_food=x})

data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  | FoodAction Food.Action
  deriving (Eq)

defaultMain :: IO ()
defaultMain = startApp App {..}
  where
    initialAction = SayHelloWorld
    model = Model 0 Food.model
    update = updateModel
    view = viewModel
    events = defaultEvents
    subs = []
    mountPoint = Nothing

-- From brick
handleEventLensed :: model -> Lens' model inner -> (inneraction -> action) -> (inneraction -> inner -> Effect inneraction inner) -> inneraction -> Effect action model
handleEventLensed v target evtarget handleEvent event = first evtarget $ do
  newB <- handleEvent event (v ^. target)
  noEff (v & target .~ newB)

viewLensed :: model -> Lens' model inner -> (inneraction -> action) -> (inner -> View inneraction) -> View action
viewLensed v target evtarget handleView = fmap evtarget (handleView (v ^. target))

updateModel :: Action -> Model -> Effect Action Model
updateModel event model =
  case event of
    AddOne -> noEff (model & counter +~ 1)
    SubtractOne -> noEff (model & counter -~ 1)
    NoOp -> noEff model
    SayHelloWorld -> model <# (consoleLog "Hello world" >> pure NoOp)
    FoodAction ev -> handleEventLensed model food FoodAction Food.updateModel ev

viewModel :: Model -> View Action
viewModel x = section_ [class_ "section"] [
      link_ [ rel_ "stylesheet", type_ "text/css", href_ bulma ]
    , script_ [ defer_ "", src_ fontawesome ] []
    , div_ [class_ "container"] [
        h1_ [ class_ "title"] [ text "Hello world" ]
      , button_ [ onClick AddOne ] [ text "+" ]
      , text . ms $ x ^. counter
      , button_ [ onClick SubtractOne ] [ text "-" ]
      , div_ [class_ "columns"] [ viewLensed x food FoodAction Food.viewModel ]
      ]
  ]
  where
    bulma = "https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css"
    fontawesome = "https://use.fontawesome.com/releases/v5.3.1/js/all.js"

