{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Network.Api where

import Control.Monad.Except
import Control.Monad.Reader         ( ReaderT, runReaderT )
import Control.Monad.Reader.Class
import Network.Wai                  ( Application )
import Database.Persist.Postgresql  ( selectList, Entity(..) )
import Servant

import Config    ( Config(..) )
import Database.Models
import Puzzle.Puzzle ( PuzzleType(..), Puzzle(..), PuzzleCategory(..) )
import qualified Puzzle.Generate as P

type PuzzleAPI =
  "puzzle"
  :> Capture "type" PuzzleType
  :> Capture "category" PuzzleCategory
  :> Get '[JSON] Puzzle

newtype App a
  = App
  { runApp :: ReaderT Config (ExceptT ServantErr IO) a
  } deriving (Functor, Applicative, Monad, MonadReader Config,
              MonadError ServantErr, MonadIO)

puzzleAPI :: Proxy PuzzleAPI
puzzleAPI = Proxy

app :: Config -> Application
app cfg = serve puzzleAPI (readerServer cfg)

readerServer :: Config -> Server PuzzleAPI
readerServer cfg = enter (convertApp cfg) server

convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

server :: ServerT PuzzleAPI App
server = genPuzzle

genPuzzle :: PuzzleType -> PuzzleCategory -> App Puzzle
genPuzzle ty cat = do
  countryRows <- runDb (selectList [] [])
  let countries = map (countryRowToCountry . entityVal) countryRows
  return $ P.generate ty cat 22 countries
