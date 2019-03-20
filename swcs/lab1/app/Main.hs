module Main where

import Protolude hiding (on)
import Conduit
import Control.Monad.Reader (withReaderT)
import Data.Conduit.Lift
import Graphics.UI.Gtk hiding (Weight)
import Graphics.UI.Gtk.Builder
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph
import Control.Concurrent.STM
import IGE.Types
import IGE.UI hiding (runMainWindow)
import Lens.Micro.Platform
import Data.Conduit.TMChan
import Control.Concurrent.STM.TMChan

import IGE.Control
import IGE.Layout
import IGE.Render

import Lib

sampleGr :: Gr Weight Weight
sampleGr = mkGraph nodes edges
 where
  nodes :: [(Node, Weight)]
  nodes = second Weight <$> [(0, 10), (1, 20), (2, 30)]
  edges :: [(Node, Node, Weight)]
  edges = fmap (\(a, b, c) -> (a, b, Weight c))
               [(0, 1, 1), (1, 0, 4), (1, 2, 2), (0, 2, 3)]

initGr = sampleGr

initRM :: RM
initRM = RM (100 :+ 0) (100 :+ 100)

data EditorTab = TaskEditorTab

data LabEditor = LabEditor
  { taskEditor :: TVar (EditorState Weight Weight)
  , sysEditor :: TVar (EditorState Weight Weight)
  , editorTab :: TVar EditorTab
  }
newtype LabVar a = LabVar { unLabVar :: ReaderT LabEditor IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader  LabEditor)

makeLensesFor [
  ("taskEditor", "_taskEditor")
  , ("editorTab", "_editorTab")] ''LabEditor

renderLabEditor :: WidgetClass self => self -> LabEditor -> IO ()
renderLabEditor da les = do
  Just dw <- widgetGetWindow da
  dims    <- liftM2 (,) (drawWindowGetWidth dw) (drawWindowGetHeight dw)
  es      <- atomically $ readTVar $ taskEditor les
  renderWithDrawWindow dw $ renderEditorState es dims

keyProcessing :: ConduitT KeyVal RefreshType LabVar ()
keyProcessing = forever $ awaitOrFinish () $ \ch -> do
  te <- lift $ fmap taskEditor ask
  transPipe (LabVar . withReaderT (const te) . unIGEM) $ defaultKeyBinding ch

refresh :: WidgetClass self => self -> ConduitT RefreshType Void LabVar ()
refresh widget = awaitOrFinish () $ const $ do
  te <- taskEditor <$> ask
  liftIO $ atomically $ modifyTVar te $ \te ->
    te { esNodeMap = layoutGr (esGraph te) }
  liftIO $ widgetQueueDraw widget
  refresh widget

defaultES :: EditorState Weight Weight
defaultES = EditorState
  { esGraph   = initGr
  , esRM      = initRM
  , esNum     = noNodes initGr
  , esCmd     = ""
  , esPrompt  = ""
  , esLabels  = []
  , esNodeMap = layoutGr initGr
  }

runMainWindow :: IO ()
runMainWindow = do
  initGUI
  b <- builderNew
  b `builderAddFromFile` "window.glade"
  w  <- builderGetObject b castToWindow ("window" :: Text)
  da <- builderGetObject b castToDrawingArea ("view" :: Text)

  on w deleteEvent $ liftIO mainQuit >> return True

  task <- newTVarIO defaultES
  sys  <- newTVarIO defaultES
  tab  <- newTVarIO TaskEditorTab
  let les = LabEditor task sys tab

  keyPress <- newTMChanIO
  _        <-
    forkIO
    $  flip runReaderT les
    $  unLabVar
    $  runConduit
    $  sourceTMChan keyPress
    .| keyProcessing
    .| refresh w

  (w `on` keyPressEvent) $ do
    kv <- eventKeyVal
    liftIO $ atomically $ writeTMChan keyPress kv
    return True

  (da `on` draw) $ liftIO $ renderLabEditor da les

  widgetShowAll w
  mainGUI

main :: IO ()
main = runMainWindow
