{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Lens           (at, traverse, filtered, makeLenses, contramap)
import           Control.Lens.Operators
import           Control.Monad          (when, void)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Maybe             (fromMaybe)
import qualified Data.Text as T
import           GHC.Generics           (Generic)
import           GHCJS.Types            (JSString, JSRef)
import           Ohm.Component
import           Ohm.HTML hiding        (title)
import           Pipes                  (MonadIO, liftIO, yield)
import           Prelude hiding         (div, id, span, map, filter)
import qualified Prelude as P


-- Models

data Visibility
  = All
  | Active
  | Completed
  deriving (Eq, Show, Generic)

data Task = Task
 { _title     :: String
 , _completed :: Bool
 , _editing   :: Bool
 , _edits     :: Maybe String
 , _id        :: Int
 } deriving (Show, Generic)

data ToDo = ToDo
  { _tasks      :: [Task]
  , _entry      :: String
  , _visibility :: Visibility
  , _uid        :: Int
  } deriving (Show, Generic)

makeLenses ''Task
makeLenses ''ToDo 

instance ToJSON Visibility
instance ToJSON ToDo
instance ToJSON Task

instance FromJSON Visibility
instance FromJSON ToDo
instance FromJSON Task

initialToDo :: ToDo
initialToDo = ToDo [] "" All 0


-- Update

data Action
  = CommitTask Int (Maybe String)
  | DeleteCompleted
  | UpdateEntry String
  | UpdateTask Int String

  -- TODO: separate into it's own component?
  | AddTask String
  | DoNothing
  | EditingTask Int Bool
  | RemoveTask Int
  | SetVisibility Visibility
  | ToggleAll Bool
  | ToggleCompleted Int
  deriving Show


update :: Action -> ToDo -> ToDo
update action todo =
  case action of
   AddTask str ->
     if isEmptyStr str then todo
     else todo &~ (do tasks <>= [Task (trimString str) False False Nothing (_uid todo)]
                      uid += 1
                      entry .= "")
   DeleteCompleted ->
     todo & tasks %~ P.filter (not . _completed)
   SetVisibility v ->
     todo & visibility .~ v
   ToggleAll isCompleted ->
     todo & tasks . traverse . completed .~ isCompleted
   UpdateEntry str ->
     todo & entry .~ str
   -- TODO: separate this into it's own component
   --       task Actions
   RemoveTask tid ->
     todo & tasks %~ P.filter ((/=) tid . _id)
   UpdateTask tid txt ->
     todo & tasks . traverseById tid . edits ?~ txt
   CommitTask tid _edits ->
     case _edits of
       Nothing  -> todo & tasks . traverseById tid . editing .~ False
       Just txt ->
         if isEmptyStr txt
            then todo & tasks %~ P.filter ((/=) tid . _id)
            else todo & tasks . traverseById tid . title .~ (trimString txt)
                      & tasks . traverseById tid . editing .~ False
                      & tasks . traverseById tid . edits   .~ Nothing
   ToggleCompleted tid ->
     todo & tasks . traverseById tid . completed %~ not
   EditingTask tid isEditing ->
     todo & tasks . traverseById tid . editing .~ isEditing
   _ -> todo

traverseById tid = traverse . filtered ((==) tid . _id)


-- Views

view :: DOMEvent Action -> ToDo -> HTML
view chan todo@(ToDo todos _txtEntry _visibility _id) =
  into div
       [ with div -- using section breaks events ?
              (attrs . at "id" ?= "todoapp")
              [ taskEntry chan _txtEntry
              , taskList chan _visibility todos
              , controls chan todo
              ]
       , info
       ]


taskView :: DOMEvent Action -> Task -> HTML
taskView chan (Task _title _isCompleted _isEditing _edits _tid) =
  with li
       (do classes .= (P.map fst . P.filter snd $ [ ("editing", _isEditing)
                                                  , ("completed", _isCompleted)
                                                  ])
           props   .  at "key" ?= (toJSString $ show _tid))
       [ with div
              (classes .= ["view"])
              [ with input
                     (do classes  .= [ "toggle" ]
                         attrs    .  at "type" ?= "checkbox"
                         -- there are booleans attributes, but for now this is a way to handle those
                         when _isCompleted (props .  at "checked" ?= "checked")
                         onChange $ contramap (const $ ToggleCompleted _tid) chan)
                     []
              , with label
                     (do classes .= ["description"]
                         onDoubleClick editTask)
                     [text _title]
              , with button
                     (do classes .= [ "destroy" ]
                         attrs . at "title" ?= "Remove Item"
                         onClick delete)
                     []
              ]
       , with input
              (do classes .= [ "edit" ]
                  attrs   . at "id" ?= todoId
                  -- props   . at "name" ?= "newTodo" -- breaks the input... perhaps it should be a prop?
                  props   . at "value" ?= (toJSString $ fromMaybe _title _edits)
                  -- when _isEditing (onFocus $ DOMEvent $ const $ channel chan $ DoNothing)
                  onBlur $ DOMEvent $ const $ channel chan $ CommitTask _tid _edits
                  onInput $ contramap (UpdateTask _tid) chan
                  onKeyDown $ contramap (\key -> if isEnter key -- TODO: gets called twice.. see if it can be avoided
                                                     then CommitTask _tid _edits
                                                     -- then EditingTask _tid False
                                                     else DoNothing) chan
              )
              []
       ]
  where
    isEnter = (==) 13
    todoId = toJSString $ "todo-" ++ show _tid
    editTask = DOMEvent $ const $ channel chan $ EditingTask _tid True
    delete = DOMEvent $ const $ channel chan $ RemoveTask _tid

taskEntry :: DOMEvent Action -> String -> HTML
taskEntry chan txt =
  with div
  (attrs . at "id" ?= "header")
  [ into h1 ["todos"]
  , with input
         (do attrs . at "id" ?= "new-todo"
             attrs . at "placeholder" ?= "What needs to be done?"
             attrs . at "autofocus" ?= "true"
             props . at "value" ?= (toJSString txt)
             onBlur $ DOMEvent $ const $ channel chan $ AddTask txt
             onInput $ contramap UpdateEntry chan
             onKeyDown $ contramap (\key -> if key == 13
                                                    then (AddTask txt)
                                                    else DoNothing) chan
         )
         []
  ]

taskList :: DOMEvent Action -> Visibility -> [Task] -> HTML
taskList chan v todos =
  with div
       (attrs . at "id" ?= "main")
       [ with input
              (do attrs . at "id" ?= "toggle-all"
                  attrs . at "type" ?= "checkbox"
                  when (allCompleted todos) (props .  at "checked" ?= "checked")
                  onChange $ contramap (const $ ToggleAll $ not $ allCompleted todos) chan)
              []
       , with label
              (attrs . at "for" ?= "toggle-all")
              ["Mark as complete"]
       , with ul
              (attrs . at "id" ?= "todo-list")
              (P.map (taskView chan) $ visibleTodos)
       ]
  where
    allCompleted :: [Task] -> Bool
    allCompleted [] = False
    allCompleted xs = all _completed xs
    visibleTodos = filterItems v todos

controls :: DOMEvent Action -> ToDo -> HTML
controls chan todo =
  with footer (do attrs . at "id" ?= "footer"
                  when (P.null $ _tasks todo) (attrs . at "hidden" ?= "hidden"))
  [ with span (attrs . at "id" ?= "todo-count")
    [ into strong [ text $ toJSString $ show tasksLeft ]
    , text $ toJSString $ itemText
    ]
  , with ul
    (attrs . at "id" ?= "filters")
    (renderFilter <$> [All, Active, Completed])
  , with button
    (do attrs . at "id" ?= "clear-completed"
        when (tasksCompleted == 0) (attrs . at "hidden" ?= "hidden")
        onClick $ DOMEvent $ const $ (channel chan) DeleteCompleted
    )
    [ text $ toJSString $ ("Clear completed (" ++ show tasksCompleted ++ ")" :: String)]
  ]
  where
    itemText = (if tasksLeft == 1 then " item" else " items") ++ " left" :: String
    tasksCompleted = P.length $ P.filter _completed $ _tasks todo
    tasksLeft = (P.length $ todo ^. tasks) - tasksCompleted
    currentFilter = (todo ^. visibility)
    renderFilter f =
      into li
      [ with a
        (do attrs . at "href" ?= (toJSString href)
            classes .= (if f == currentFilter then ["selected"] else [])
            onClick $ filterClick f)
        [text $ show f]
      ]
      where
        href = "#" ++ showFilter f
    filterClick f = DOMEvent $ const $ channel chan $ SetVisibility f

info :: HTML
info =
  with footer (attrs . at "id" ?= "info")
       [ into p [ "Double-click to edit a todo" ]
       , into p [ "Written by "
                , with a
                       (attrs . at "href" ?= "https://github.com/luigy")
                       [ "Luigy Leon" ]]
       , into p [ "Part of "
                , with a
                       (attrs . at "href" ?= "http://todomvc.com")
                       [ "TodoMVC" ]
                ]
       ]

--------------------------------------------------------------------------------

-- Utils

isEmptyStr :: String -> Bool
isEmptyStr = T.null . T.strip . T.pack

trimString :: String -> String
trimString = T.unpack . T.strip . T.pack

showFilter :: Visibility -> String
showFilter All       = "/"
showFilter Active    = "/active"
showFilter Completed = "/completed"

filterItems :: Visibility -> [Task] -> [Task]
filterItems All       = P.id
filterItems Active    = P.filter (not . _completed)
filterItems Completed = P.filter _completed

logMessage :: (Show a, MonadIO m) => String -> a -> m ()
logMessage msg obj = liftIO . putStrLn $ msg ++ ": " ++ (show obj)


-- Component

-- Log all DOM Events
idDebugProcessor :: (Show e, MonadIO m) => Processor m e e
idDebugProcessor =
  Processor (\x -> do
                liftIO $ print $ show x
                yield x)

modelComp :: Component () Action ToDo Action
modelComp = Component update view idDebugProcessor -- idProcessor

-- wire up the component
main :: IO ()
main = void $ runComponentDebug initialToDo () modelComp


-- localstorage
foreign import javascript unsafe
  "localStorage.setItem($1, JSON.stringify($2))"
  putLocalStorage :: JSString -> JSRef a -> IO ()

foreign import javascript unsafe
  "localStorage.getItem($1) || ''"
  getLocalStorage :: JSString -> IO JSString
