module Main where

import GHC.Read (Read(..))
import Protolude hiding (on, Read)
import Conduit
import Control.Monad.Reader (withReaderT)
import Data.Conduit.Lift
import qualified Data.ByteString.Lazy as B (writeFile, readFile)
import Graphics.UI.Gtk hiding (Weight)
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Selectors.FileChooser
import Graphics.UI.Gtk.Selectors.FileChooserDialog
import Graphics.Rendering.Cairo
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph
import Control.Concurrent.STM
import IGE.Types
import IGE.UI hiding (runMainWindow)
import Lens.Micro.Platform
import Data.Conduit.TMChan
import Control.Concurrent.STM.TMChan
import qualified Data.Map.Strict as M
import System.Random (RandomGen, newStdGen)
import Data.Maybe (fromJust)
import Data.List (groupBy)
import Data.IORef
import qualified Data.Graph.Inductive as G

import IGE.Control
import IGE.Layout
import IGE.Render
import IGE.Keys
import IGE.Serialization

import Types
import Gant
import Algo
import Model
import Generator hiding (isAcyclic)

sampleGr :: Gr Weight Weight
sampleGr = mkGraph [] []
--sampleGr = mkGraph nodes edges
-- where
--  nodes :: [(Node, Weight)]
--  nodes = second Weight <$> [(0, 10), (1, 20), (2, 30)]
--  edges :: [(Node, Node, Weight)]
--  edges = fmap (\(a, b, c) -> (a, b, Weight c))
--               [(0, 1, 1), (1, 0, 4), (1, 2, 2), (0, 2, 3)]

initGr = sampleGr

initRM :: RM
initRM = RM (100 :+ 0) (100 :+ 100)

data ChosenEditorTab = TaskEditorTabC | SysEditorTabC | GantTabC
  deriving (Eq)

data EnchEditorState = EnchEditorState
  { esE :: TVar (EditorState Weight Weight)
  , compInfoE :: TVar CompInfo
  }

data CompInfo = CompInfo
  {  _isAcyclic :: Bool
  ,  _isConnected :: Bool
  }

data ModelInfo = ModelInfo
  { _taskQueue :: Maybe [Node]
  , _randomQueue :: Maybe [Node]
  , _gantDiagram :: Maybe GantDiagram
  }

data LabEditor = LabEditor
  { taskEditor :: EnchEditorState
  , sysEditor :: EnchEditorState
  , modelInfo :: TVar ModelInfo
  , cpuParams :: TVar CPUParams
  , editorTab :: TVar ChosenEditorTab
  }
newtype LabVar a = LabVar { unLabVar :: ReaderT LabEditor IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader  LabEditor)

makeLensesFor [
  ("taskEditor", "_taskEditor")
  , ("sysEditor", "_sysEditor")
  , ("gantDiagram", "_gantDiagram")
  , ("editorTab", "_editorTab")] ''LabEditor

data EnchEditorTab = EnchEditorTab
  { esT :: EditorState Weight Weight
  , compInfoT :: CompInfo
  }

data EditorView = EditorView
  { _tabV :: Tab
  , _modelInfoV :: ModelInfo
  }

data Tab = EditorTab EnchEditorTab | GantTab GantDiagram

instance DimsRenderable EditorView where
  dimsRender dims@(w,h) (EditorView (EditorTab et) mi) = do
    dimsRender dims (esT et)
    let aText  = "Acyclic:" <> if _isAcyclic (compInfoT et) then "Yes" else "No":: Text
        cText  = "Connected:" <> if _isConnected (compInfoT et) then "Yes" else "No" :: Text
    aExts <- textExtents aText
    cExts <- textExtents cText
    let h0 = 30
        h1 = h0 + textExtentsHeight aExts
    moveTo (fromIntegral w - 5 - textExtentsWidth aExts) h0
    showText aText
    moveTo (fromIntegral w - 5 - textExtentsWidth cExts) h1
    showText cText
    case _taskQueue mi of
      Just tq -> do
        let rq = fromJust $ _randomQueue mi
            tqText = "Task queue: " <> show tq :: Text
            rqText = "Random queue: " <> show rq :: Text
        tqExts <- textExtents tqText
        rqExts <- textExtents rqText
        moveTo 5 (fromIntegral h - 15 - textExtentsHeight tqExts - textExtentsHeight rqExts)
        showText tqText
        moveTo 5 (fromIntegral h - 15  - textExtentsHeight rqExts)
        showText rqText
      Nothing -> return ()
  dimsRender dims (EditorView (GantTab gd) mi) = dimsRender dims gd

renderLabEditor :: WidgetClass self => self -> LabEditor -> IO ()
renderLabEditor da le = do
  Just dw <- widgetGetWindow da
  dims    <- liftM2 (,) (drawWindowGetWidth dw) (drawWindowGetHeight dw)
  tab     <- atomically $ curTab le
  renderWithDrawWindow dw $ dimsRender dims tab

matchingTab :: KeyVal -> Maybe ChosenEditorTab
matchingTab kv | kv == xK_1 = Just TaskEditorTabC
               | kv == xK_2 = Just SysEditorTabC
               | kv == xK_3 = Just GantTabC
               | otherwise  = Nothing

gdFromGraphs :: GantDiagram
gdFromGraphs = GantDiagram
  $ M.fromList [(1, ([TimeRecord 1 2 4], [TimeRecord 0 1 (Transfer 1 3 4)]))]

curTab :: LabEditor -> STM EditorView
curTab le = do
  mi <- readTVar $ modelInfo le
  let eToTab (EnchEditorState aT ciT) = do
        a  <- readTVar aT
        ci <- readTVar ciT
        return $ EditorView (EditorTab $ EnchEditorTab a ci) mi
  et <- readTVar $ editorTab le
  case et of
    TaskEditorTabC -> eToTab $ taskEditor le
    SysEditorTabC  -> eToTab $ sysEditor le
    GantTabC       -> do
      gM <- fmap _gantDiagram $ readTVar $ modelInfo le
      return $ EditorView (GantTab $ fromMaybe gdFromGraphs gM) mi

      {-
      let
        gd = do
          tg   <- fmap esGraph $ readTVar $ esE $ taskEditor le
          sg   <- fmap esGraph $ readTVar $ esE $ sysEditor le
          cpup <- readTVar $ Main.cpuParams le
          return $ closestMatching (ModelParams tg sg cpup) <$> _taskQueue mi
      gant <- maybe (fromMaybe gdFromGraphs <$> gd) return gM
      return $ EditorView (GantTab gant) mi
-}

curEditorTab :: LabEditor -> STM (Maybe (TVar (EditorState Weight Weight)))
curEditorTab le = do
  et <- readTVar $ editorTab le
  return $ case et of
    TaskEditorTabC -> Just $ esE $ taskEditor le
    SysEditorTabC  -> Just $ esE $ sysEditor le
    GantTabC       -> Nothing

keyProcessing :: ConduitT KeyVal RefreshType LabVar ()
keyProcessing = forever $ awaitOrFinish () $ \kv -> case matchingTab kv of
  Just t -> do
    lift $ do
      et <- editorTab <$> ask
      liftIO $ atomically $ modifyTVar et $ const t
    yield GraphChange
  Nothing -> do
    le  <- ask
    ceM <- lift $ liftIO $ atomically $ do
      et <- readTVar $ editorTab le
      return $ case et of
        TaskEditorTabC -> Just (esE $ taskEditor le, defaultKeyBinding)
        SysEditorTabC  -> Just (esE $ sysEditor le, symKeyBinding)
        GantTabC       -> Nothing
    case ceM of
      Just (ce, kb) ->
        transPipe (LabVar . withReaderT (const ce) . unIGEM) $ kb kv
      Nothing -> return ()

changeGraph :: RandomGen g => g -> LabEditor -> STM ()
changeGraph g le = do
  et <- readTVar $ editorTab le
  let esM = case et of
        TaskEditorTabC -> Just $ taskEditor le
        SysEditorTabC  -> Just $ sysEditor le
        _              -> Nothing
  case esM of
    Nothing                      -> return ()
    Just (EnchEditorState es ci) -> do
      gr <- esGraph <$> readTVar es
      writeTVar ci $ evalCompInfo g gr
  tg <- fmap esGraph $ readTVar $ esE $ taskEditor le
  let tq' = if isAcyclic tg then Just (taskQueue tg) else Nothing
  let rq' = if isAcyclic tg then Just (randomQueue g tg) else Nothing
  sg    <- fmap esGraph $ readTVar $ esE $ sysEditor le
  gant' <- if isJust tq' && isConnected sg && G.order sg > 0
    then do
      mi   <- readTVar $ modelInfo le
      cpup <- readTVar $ Main.cpuParams le
      return $ Just $ closestMatching (ModelParams tg sg cpup) (fromJust tq')
    else return Nothing
  writeTVar (modelInfo le) (ModelInfo tq' rq' gant')


refreshC :: WidgetClass self => self -> ConduitT RefreshType Void LabVar ()
refreshC widget = forever $ awaitOrFinish () $ \rt -> do
  le <- ask
  g  <- liftIO newStdGen
  when (rt == GraphChange) $ liftIO $ atomically $ changeGraph g le
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

evalCompInfo :: (RandomGen g, Graph gr) => g -> gr Weight Weight -> CompInfo
evalCompInfo g gr = CompInfo (isAcyclic gr) (isConnected gr)
--  (if isAcyclic gr then Just (taskQueue gr) else Nothing)
--  (if isAcyclic gr then Just (randomQueue g gr) else Nothing)

defaultES :: RandomGen g => g -> EnchEditorTab
defaultES g = EnchEditorTab es $ evalCompInfo g initGr
 where
  es = EditorState
    { esGraph   = initGr
    , esRM      = initRM
    , esNum     = noNodes initGr
    , esCmd     = ""
    , esPrompt  = ""
    , esLabels  = []
    , esNodeMap = layoutGr initGr
    }

defaultCPUParams :: CPUParams
defaultCPUParams = CPUParams 1 True False 0

newEditorTab :: EnchEditorTab -> STM EnchEditorState
newEditorTab (EnchEditorTab es ci) =
  liftA2 EnchEditorState (newTVar es) (newTVar ci)

writeEditorTab :: EnchEditorState -> EnchEditorTab -> STM ()
writeEditorTab es (EnchEditorTab a ci) = do
  writeTVar (esE es)       a
  writeTVar (compInfoE es) ci

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

  w `on` deleteEvent $ liftIO mainQuit >> return True

  g    <- newStdGen
  task <- atomically $ newEditorTab $ defaultES g
  sys  <- atomically $ newEditorTab $ defaultES g
  mi   <- atomically $ newTVar $ ModelInfo Nothing Nothing Nothing
  cp   <- atomically $ newTVar defaultCPUParams
  tab  <- newTVarIO TaskEditorTabC
  let le = LabEditor task sys mi cp tab

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
  taskSaveAs `on` menuItemActivated $ saveGraphUsingDialog w $ esE $ taskEditor
    le

  taskOpenAs <- builderGetObject b castToImageMenuItem ("task_open" :: Text)
  taskOpenAs `on` menuItemActivated $ do
    openGraphUsingDialog w $ esE $ taskEditor le
    atomically $ writeTVar (editorTab le) TaskEditorTabC
    refresh w le

  sysSaveAs <- builderGetObject b castToImageMenuItem ("sys_save" :: Text)
  sysSaveAs `on` menuItemActivated $ saveGraphUsingDialog w $ esE $ sysEditor le

  sysOpenAs <- builderGetObject b castToImageMenuItem ("sys_open" :: Text)
  sysOpenAs `on` menuItemActivated $ do
    openGraphUsingDialog w $ esE $ sysEditor le
    atomically $ writeTVar (editorTab le) SysEditorTabC
    refresh w le

  taskNewEditor <- builderGetObject b castToMenuItem ("task_new_editor" :: Text)
  taskNewEditor `on` menuItemActivated $ do
    atomically $ do
      writeEditorTab (taskEditor le) $ defaultES g
      writeTVar (editorTab le) TaskEditorTabC
    refresh w le

  sysNew <- builderGetObject b castToMenuItem ("sys_new_editor" :: Text)
  sysNew `on` menuItemActivated $ do
    atomically $ do
      writeEditorTab (sysEditor le) $ defaultES g
      writeTVar (editorTab le) SysEditorTabC
    refresh w le

  hw <- builderGetObject b castToWindow ("help_window" :: Text)
  hw `on` deleteEvent $ liftIO (widgetHide hw) >> return True

  help <- builderGetObject b castToMenuItem ("help" :: Text)
  help `on` menuItemActivated $ widgetShowAll hw

  generateWindow  w b le
  cpuParamsWindow w b le

  quit <- builderGetObject b castToMenuItem ("quit" :: Text)
  quit `on` menuItemActivated $ mainQuit

  widgetShowAll w
  mainGUI

generateWindow :: Window -> Builder -> LabEditor -> IO ()
generateWindow w b le = do
  gen_win <- builderGetObject b castToWindow ("generate_window" :: Text)
  nodes_number <- builderGetObject b castToEntry ("nodes_number" :: Text)
  node_weight_range <- builderGetObject b
                                        castToEntry
                                        ("node_weight_range" :: Text)
  connectivity      <- builderGetObject b castToEntry ("connectivity" :: Text)
  edge_weight_range <- builderGetObject b
                                        castToEntry
                                        ("edge_weight_range" :: Text)
  let readRange str = do
        let [fStr, sStr] = groupBy (const (/= '-')) str
        f <- readMaybe fStr
        s <- readMaybe $ drop 1 sStr
        return (f, s)
      genParams = do
        nn    <- readMaybe <$> entryGetText nodes_number
        nwr   <- readRange <$> entryGetText node_weight_range
        cntvt <- readMaybe <$> entryGetText connectivity
        ewr   <- readRange <$> entryGetText edge_weight_range
        return $ liftM4 GenParams nn nwr cntvt (Just ewr)

  isTaskGenRef <- newIORef True

  gen_task     <- builderGetObject b castToMenuItem ("generate_task" :: Text)
  gen_task `on` menuItemActivated $ do
    writeIORef isTaskGenRef True
    widgetShowAll gen_win

  gen_sys <- builderGetObject b castToMenuItem ("generate_sys" :: Text)
  gen_sys `on` menuItemActivated $ do
    writeIORef isTaskGenRef False
    widgetShowAll gen_win

  initSeed <- newStdGen
  print initSeed
  seedVar <- atomically $ newTVar initSeed
  gen_btn <- builderGetObject b castToButton ("generate_btn" :: Text)
  gen_btn `on` buttonActivated $ do
    gpM    <- genParams
    isTask <- readIORef isTaskGenRef
    when (isJust gpM) $ atomically $ do
      seed <- readTVar seedVar
      let (grM, seed') = (if isTask then randomTaskGraph else randomSysGraph)
            seed
            (fromJust gpM)
      writeTVar seedVar seed'
      when (isJust grM) $ do
        modifyTVar (esE $ (if isTask then taskEditor else sysEditor) le)
          $ \te -> te { esGraph = fromJust grM }
        writeTVar (editorTab le)
                  (if isTask then TaskEditorTabC else SysEditorTabC)
        changeGraph seed le
    seed <- atomically $ readTVar seedVar
    print seed
    widgetHide gen_win
    refresh w le

  return ()

cpuParamsWindow :: Window -> Builder -> LabEditor -> IO ()
cpuParamsWindow w b le = do
  cpu_params_win <- builderGetObject b
                                     castToWindow
                                     ("cpu_params_window" :: Text)
  ph_links          <- builderGetObject b castToEntry ("ph_links" :: Text)
  in_out_processors <- builderGetObject b
                                        castToCheckButton
                                        ("in_out_processors" :: Text)
  duplex_links <- builderGetObject b castToCheckButton ("duplex_links" :: Text)
  packet_length  <- builderGetObject b castToEntry ("packet_length" :: Text)
  save           <- builderGetObject b castToButton ("cpu_params_save" :: Text)

  cpu_params_btn <- builderGetObject b castToMenuItem ("cpu_params_btn" :: Text)
  cpu_params_btn `on` menuItemActivated $ widgetShowAll cpu_params_win

  let readCPUParams = do
        phLinks         <- readMaybe <$> entryGetText ph_links
        inOutProcessors <- toggleButtonGetActive in_out_processors
        duplexLinks     <- toggleButtonGetActive duplex_links
        packetLength    <- (\s -> if s == "" then Just 0 else readMaybe s)
          <$> entryGetText packet_length
        return $ do
          phl <- phLinks
          pl  <- packetLength
          return $ CPUParams phl inOutProcessors duplexLinks pl

  save `on` buttonActivated $ do
    cpM <- readCPUParams
    case cpM of
      Nothing -> putText "Wrong format"
      Just cp -> do
        atomically $ writeTVar (Main.cpuParams le) cp
        widgetHide cpu_params_win
        refresh w le

  return ()

main :: IO ()
main = runMainWindow
