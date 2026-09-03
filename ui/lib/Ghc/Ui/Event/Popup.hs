module Ghc.Ui.Event.Popup where

import Brick (BrickEvent (..), EventM, zoom)
import Brick.Forms (Form, handleFormEvent)
import Brick.Widgets.List (GenericList, Splittable, handleListEvent, handleListEventVi)
import Control.Lens (Traversal', use, (.=))
import Control.Monad (unless)
import Ghc.Ui.Data.Main (MainState (..))
import Ghc.Ui.Data.Name (Name (..))
import Ghc.Ui.Monad (MonadUi)
import Ghc.Ui.OpLog (logOpDebug)
import Graphics.Vty (Event (..), Key (..))
import Types.Text (showText)

handleListEventOf ::
  (Foldable t, Splittable t, Ord n) =>
  Traversal' s (GenericList n t e) ->
  Event ->
  EventM n s ()
handleListEventOf lens =
  zoom lens . handleListEventVi handleListEvent

-- TODO make sure all consumers are safe
focus ::
  MonadUi m =>
  Name ->
  m ()
focus target = do
  logOpDebug ("Focusing " <> showText target)
  #currentFocus .= target

openPopup ::
  MonadUi m =>
  Name ->
  m ()
openPopup target = do
  current <- use #currentFocus
  #previousFocus .= current
  focus target

closePopup ::
  MonadUi m =>
  m ()
closePopup = do
  target <- use #previousFocus
  focus target

-- TODO when Esc is used in the initial screen's server form, the focus of the edit is correctly lost, but it is still
-- drawn as selected.
-- Not sure if this happens here.
staticDialog ::
  MonadUi m =>
  Event ->
  m ()
staticDialog = \case
  EvKey KEsc [] -> closePopup
  _ -> pure ()

popupKeyEvent ::
  MonadUi m =>
  Bool ->
  (Event -> m ()) ->
  m () ->
  Event ->
  m ()
popupKeyEvent enter fallback finalize = \case
  EvKey KEsc [] -> do
    closePopup
    unless enter do
      finalize
  EvKey KEnter [] | enter -> do
    closePopup
    finalize
  event -> fallback event

listKeyEvent ::
  Foldable t =>
  Splittable t =>
  Traversal' MainState (GenericList Name t e) ->
  Bool ->
  Event ->
  EventM Name MainState ()
listKeyEvent lens enter =
  popupKeyEvent enter (handleListEventOf lens) (pure ())

-- | @handleFormEvent@ updates the text in the lens immediately after each key press
handleForm ::
  Traversal' MainState (Form s e Name) ->
  Event ->
  EventM Name MainState ()
handleForm lens =
  zoom lens . handleFormEvent . VtyEvent
