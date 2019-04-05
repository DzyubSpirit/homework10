module Main where

import Protolude hiding (on)
import Conduit
import Control.Monad.Reader (withReaderT)
import Data.Conduit.Lift
import qualified Data.ByteString.Lazy as B (writeFile, readFile)
import Graphics.UI.Gtk hiding (Weight)
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Selectors.FileChooser
import Graphics.UI.Gtk.Selectors.FileChooserDialog
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph
import Control.Concurrent.STM
import IGE.Types
import IGE.UI hiding (runMainWindow)
import Lens.Micro.Platform
import Data.Conduit.TMChan
import Control.Concurrent.STM.TMChan
import qualified Data.Map.Strict as M

import IGE.Control
import IGE.Layout
import IGE.Render
import IGE.Keys
import IGE.Serialization

import Gant

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

data ChosenEditorTab = TaskEditorTabC | SysEditorTabC | GantTabC

data LabEditor = LabEditor
  { taskEditor :: TVar (EditorState Weight Weight)
  , sysEditor :: TVar (EditorState Weight Weight)
  , gantDiagram :: Maybe GantDiagram
  , editorTab :: TVar ChosenEditorTab
  }
newtype LabVar a = LabVar { unLabVar :: ReaderT LabEditor IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader  LabEditor)

makeLensesFor [
  ("taskEditor", "_taskEditor")
  , ("sysEditor", "_sysEditor")
  , ("gantDiagram", "_gantDiagram")
  , ("editorTab", "_editorTab")] ''LabEditor

data Tab = EditorTab (EditorState Weight Weight) | GantTab GantDiagram

instance DimsRenderable Tab where
  dimsRender dims (EditorTab t) = dimsRender dims t
  dimsRender dims (GantTab gd) = dimsRender dims gd

renderLabEditor :: WidgetClass self => self -> LabEditor -> IO ()
renderLabEditor da le = do
  Just dw <- widgetGetWindow da
  dims    <- liftM2 (,) (drawWindowGetWidth dw) (drawWindowGetHeight dw)
  es      <- atomically $ curTab le
  renderWithDrawWindow dw $ dimsRender dims es

matchingTab :: KeyVal -> Maybe ChosenEditorTab
matchingTab kv | kv == xK_1 = Just TaskEditorTabC
               | kv == xK_2 = Just SysEditorTabC
               | kv == xK_3 = Just GantTabC
               | otherwise  = Nothing

gdFromGraphs :: Gr Weight Weight -> Gr Weight Weight -> GantDiagram
gdFromGraphs tasks sys =
  GantDiagram $ M.fromList [(1, ([TimeRecord 1 2 4], [TimeRecord 0 1 (1, 3)]))]

curTab :: LabEditor -> STM Tab
curTab le = do
  et <- readTVar $ editorTab le
  case et of
    TaskEditorTabC -> fmap EditorTab $ readTVar $ taskEditor le
    SysEditorTabC  -> fmap EditorTab $ readTVar $ sysEditor le
    GantTabC       -> maybe gd (return . GantTab) $ gantDiagram le
     where
      gd = do
        tg <- fmap esGraph $ readTVar $ taskEditor le
        eg <- fmap esGraph $ readTVar $ sysEditor le
        return $ GantTab $ gdFromGraphs tg eg

curEditorTab :: LabEditor -> STM (Maybe (TVar (EditorState Weight Weight)))
curEditorTab le = do
  et <- readTVar $ editorTab le
  return $ case et of
    TaskEditorTabC -> Just $ taskEditor le
    SysEditorTabC  -> Just $ sysEditor le
    GantTabC       -> Nothing

keyProcessing :: ConduitT KeyVal RefreshType LabVar ()
keyProcessing = forever $ awaitOrFinish () $ \kv -> case matchingTab kv of
  Just t -> do
    lift $ do
      et <- editorTab <$> ask
      liftIO $ atomically $ modifyTVar et $ const t
    yield LayoutChange
  Nothing -> do
    ceM <- lift $ ask >>= liftIO . atomically . curEditorTab
    case ceM of
      Just ce -> transPipe (LabVar . withReaderT (const ce) . unIGEM)
        $ defaultKeyBinding kv
      Nothing -> return ()

refreshC :: WidgetClass self => self -> ConduitT RefreshType Void LabVar ()
refreshC widget = forever $ awaitOrFinish () $ const $ do
  le <- ask
  liftIO $ refresh widget le

refresh :: WidgetClass self => self -> LabEditor -> IO ()
refresh widget le = do
  liftIO $ atomically $ do
    ceM <- curEditorTab le
    case ceM of
      Just ce ->
        modifyTVar ce $ \ce -> ce { esNodeMap = layoutGr (esGraph ce) }
      Nothing -> return ()
  liftIO $ widgetQueueDraw widget

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

openDialog :: DialogClass self => self -> IO ResponseId
openDialog dialog = do
  widgetShow dialog
  response <- dialogRun dialog
  widgetHide dialog
  return response

openGraphUsingDialog :: Window -> TVar (EditorState Weight Weight) -> IO ()
openGraphUsingDialog parentWindow es = do
  dialog <- fileChooserDialogNew
    (Just "Open graph... " :: Maybe Text)
    (Just parentWindow)
    FileChooserActionOpen
    [("gtk-cancel", ResponseCancel), ("gtk-open", ResponseAccept)]
  response <- openDialog dialog
  when (response == ResponseAccept) $ do
    Just fname <- fileChooserGetFilename dialog
    Just gr    <- graphFromBS <$> B.readFile fname
    atomically $ modifyTVar es $ \ce -> ce { esGraph = gr }

saveGraphUsingDialog :: Window -> TVar (EditorState Weight Weight) -> IO ()
saveGraphUsingDialog parentWindow es = do
  dialog <- fileChooserDialogNew
    (Just "Save graph as... " :: Maybe Text)
    (Just parentWindow)
    FileChooserActionSave
    [("gtk-cancel", ResponseCancel), ("gtk-save", ResponseAccept)]
  response <- openDialog dialog
  when (response == ResponseAccept) $ do
    Just fname <- fileChooserGetFilename dialog
    es         <- atomically $ readTVar es
    B.writeFile fname $ graphToBS $ esGraph es

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
  tab  <- newTVarIO TaskEditorTabC
  let le = LabEditor task sys Nothing tab

  keyPress <- newTMChanIO
  _        <-
    forkIO
    $  flip runReaderT le
    $  unLabVar
    $  runConduit
    $  sourceTMChan keyPress
    .| keyProcessing
    .| refreshC w

  (w `on` keyPressEvent) $ do
    kv <- eventKeyVal
    liftIO $ atomically $ writeTMChan keyPress kv
    return False

  da `on` draw $ liftIO $ renderLabEditor da le

  taskSaveAs <- builderGetObject b castToImageMenuItem ("task_save" :: Text)
  taskSaveAs `on` menuItemActivated $ saveGraphUsingDialog w $ taskEditor le

  taskOpenAs <- builderGetObject b castToImageMenuItem ("task_open" :: Text)
  taskOpenAs `on` menuItemActivated $ do
    openGraphUsingDialog w $ taskEditor le
    atomically $ writeTVar (editorTab le) TaskEditorTabC
    refresh w le

  sysSaveAs <- builderGetObject b castToImageMenuItem ("sys_save" :: Text)
  sysSaveAs `on` menuItemActivated $ saveGraphUsingDialog w $ sysEditor le

  sysOpenAs <- builderGetObject b castToImageMenuItem ("sys_open" :: Text)
  sysOpenAs `on` menuItemActivated $ do
    openGraphUsingDialog w $ sysEditor le
    atomically $ writeTVar (editorTab le) SysEditorTabC
    refresh w le

  taskNewEditor <- builderGetObject b castToMenuItem ("task_new_editor" :: Text)
  taskNewEditor `on` menuItemActivated $ do
    atomically $ do
      writeTVar (taskEditor le) defaultES
      writeTVar (editorTab le)  TaskEditorTabC
    refresh w le

  sysNew <- builderGetObject b castToMenuItem ("sys_new" :: Text)
  sysNew `on` menuItemActivated $ do
    atomically $ do
      writeTVar (sysEditor le) defaultES
      writeTVar (editorTab le) SysEditorTabC
    refresh w le

  help <- builderGetObject b castToMenuItem ("help" :: Text)
  help `on` menuItemActivated $ do
    hw <- builderGetObject b castToWindow ("help_window" :: Text)
    widgetShowAll hw

  quit <- builderGetObject b castToMenuItem ("quit" :: Text)
  quit `on` menuItemActivated $ mainQuit

  widgetShowAll w
  mainGUI

main :: IO ()
main = runMainWindow
