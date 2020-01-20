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


-- A way of having self-contained components / widgets
-- Each component has their own state which can't leak outside.
--
-- Idea taken from brick



-- | Handle an event with the help of a lens
--
-- Given a lens into a model and an action wrapper, this function can fully encapsulate an inner state
--
-- @
-- FoodAction event -> handleEventLensed model food FoodAction Food.updateModel event
-- @
handleEventLensed
  :: model -- ^ The model
  -> Lens' model inner -- ^ The lens into the component view of the model
  -> (inneraction -> action) -- ^ The wrapping function from the inner action into the outer action
  -> (inneraction -> inner -> Effect inneraction inner) -- ^ Event handler. It only sees the inner model and action
  -> inneraction -- ^ The inner action
  -> Effect action model -- ^ Fully wrapped effect
handleEventLensed v target evtarget handleEvent event = first evtarget $ do
  newB <- handleEvent event (v ^. target)
  noEff (v & target .~ newB)

-- | Incorporate a view with the help of a lens
--
-- Given a lens into a model and an action wrapper, this function can fully encapsulate an inner state
viewLensed
  :: model -- ^ The model
  -> Lens' model inner -- ^ The lens into the component view of the model
  -> (inneraction -> action) -- ^ The wrapping function from the inner action into the outer action
  -> (inner -> View inneraction) -- ^ The inner view function
  -> View action
viewLensed v target evtarget handleView = fmap evtarget (handleView (v ^. target))
