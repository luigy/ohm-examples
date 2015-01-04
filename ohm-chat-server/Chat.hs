{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Chat (server, ServerState (..), UsersConnected(..), UsersTyping(..)) where

import           Prelude hiding (mapM_)

import           ChatTypes
import           Control.Applicative
import qualified Control.Concurrent.STM as STM
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Foldable (mapM_)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Network.SocketIO as SocketIO
import qualified Snap.Core as Snap
--------------------------------------------------------------------------------

newtype UsersConnected = UsersConnected { uConnected :: Set Uname } deriving Show
newtype UsersTyping = UsersTyping { uTyping :: Set Uname } deriving Show

data ServerState = ServerState {
   ssConnected :: STM.TVar UsersConnected
 , ssTyping :: STM.TVar UsersTyping
 , ssMessages :: STM.TVar [Said]
 }

getState :: MonadIO m => ServerState -> m InitialState
getState state = liftIO $ STM.atomically $ do
  c <- uConnected <$> STM.readTVar (ssConnected state)
  t <- uTyping <$> STM.readTVar (ssTyping state)
  m <- STM.readTVar (ssMessages state)
  return $ InitialState c t m

server :: ServerState -> StateT SocketIO.RoutingTable (ReaderT SocketIO.Socket Snap.Snap) ()
server state = do
  userNameMVar <- liftIO STM.newEmptyTMVarIO
  let state' = getState state
  let forUserName m = liftIO (STM.atomically (STM.tryReadTMVar userNameMVar)) >>= mapM_ m

  SocketIO.on "new message" $ \(NewMessage message) ->
    forUserName $ \userName -> do
      liftIO $ print message
      SocketIO.broadcast "new message" (Said userName message)
  
  SocketIO.on_ "load state" $ SocketIO.emit "state" =<< state'

  SocketIO.on "add user" $ \m@(AddUser userName) -> do
    liftIO $ print m
    names <- liftIO $ STM.atomically $ do
      names <- (Set.insert userName) . uConnected <$> STM.readTVar (ssConnected state)
      STM.putTMVar userNameMVar userName
      STM.writeTVar (ssConnected state) (UsersConnected names)
      return names

    SocketIO.emit "login" (Loggedin userName)
    SocketIO.broadcast "user joined" (UserJoined userName)
    SocketIO.broadcast "currently connected" names

  SocketIO.appendDisconnectHandler $ do
    mUserName <- liftIO $ STM.atomically $ do
      mUserName <- STM.tryReadTMVar userNameMVar
      case mUserName of
        Just u -> do
          connected <- (Set.delete u) . uConnected <$> STM.readTVar (ssConnected state)
          typing <- (Set.delete u) . uTyping <$> STM.readTVar (ssTyping state)
          STM.writeTVar (ssConnected state) (UsersConnected connected) 
          STM.writeTVar (ssTyping state) (UsersTyping typing) 
        Nothing -> return ()
      return mUserName

    case mUserName of
      Nothing -> return ()
      Just userName ->
        SocketIO.broadcast "user left" (UserJoined userName)

  SocketIO.on_ "typing" $
    forUserName $ \userName -> do
      InitialState _ t _ <- state'
      SocketIO.broadcast "typing" (UserName userName)
      SocketIO.broadcast "currently typing" t

  SocketIO.on_ "stop typing" $
    forUserName $ \userName -> do
      InitialState _ t _ <- state'
      SocketIO.broadcast "stop typing" (UserName userName)
      SocketIO.broadcast "currently typing" t
