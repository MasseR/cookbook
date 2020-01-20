module View.Food where

import           Data.Cookbook
import           Data.Ingredient         (Amount (..), Unit (..), ingredient)
import           Data.Recipe

import           Control.Lens
import           Control.Lens.MisoString

import           Data.Name

import           Miso
import           Miso.String

data Model
  = Model { _food  :: Food
          , _entry :: MisoString
          , _diary :: Maybe Diary -- Maybe we should have a zipper into the diary instead?
          -- XXX: Maybe add a diary type here?
          -- How would you do the date which has an io action?
          }
  deriving (Eq)

food :: Lens' Model Food
food = lens _food (\x f -> x{_food = f})

entry :: Lens' Model MisoString
entry = lens _entry (\x e -> x{_entry = e})

diary :: Lens' Model (Maybe Diary)
diary = lens _diary (\x d -> x{_diary = d})

data Action
  = UpdateName MisoString
  | UpdateEntry MisoString
  | AddIngredient
  | NewDiary Day
  | New
  | NoOp
  deriving (Eq)

model :: Model
model = Model (Food (Name "") []) "" Nothing

updateModel :: Action -> Model -> Effect Action Model
updateModel event m =
  case event of
       UpdateName s -> noEff (m & food . name . _Name . from _MisoString .~ s)
       UpdateEntry s -> noEff (m & entry .~ s)
       -- XXX: Clean up. Right now it's good if I just see something outputed
       AddIngredient -> noEff $
        m & diary . _Just . recipe <>~ (Recipe (ingredient (m ^. entry . _MisoString . from _Name) (Amount 0) (Unit "tsp")) "")
          & entry .~ ""
       NewDiary day -> noEff (m & diary ?~ Diary day mempty)
       New -> m <# (liftIO (NewDiary . utctDay <$> getCurrentTime))
       NoOp -> pure m

viewModel :: Model -> View Action
viewModel m = div_ [ class_ "columns" ] [ viewForm m, viewResults m ]

viewForm :: Model -> View Action
viewForm m = div_ [ class_ "column" ] [
    button_ [ onClick New ] [ text "New diary entry" ]
  , field "text" UpdateName NoOp "Name" (m ^. food . name . _Name . from _MisoString)
  , field "text" UpdateEntry AddIngredient "Ingredient" (m ^. entry)
  ]
  where
    field fieldType act ret lbl  val = div_ [ class_ "field" ] [
        label_ [ class_ "label" ] [ text lbl ]
      , div_ [ class_ "control" ] [ input_ [ onInput act, class_ "input", type_ fieldType, value_ val, onEnter ret ] ]
      ]

onEnter :: Action -> Attribute Action
onEnter action = onKeyDown $ bool NoOp action . (== KeyCode 13)

viewResults :: Model -> View Action
viewResults m = div_ [ class_ "column" ] [
    p_ [] [text (m ^. food . name . _Name . from _MisoString)]
  , p_ [] [text (toMisoString $ show (m ^. diary . _Just . recipe . ingredients))]
  ]
