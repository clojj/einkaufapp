{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Data.Aeson (ToJSON)
import GHC.Generics
import Web.Scotty

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Data.Monoid ((<>))
import Data.Text (Text, concat)

import Control.Monad.IO.Class

import Network.Wai.Middleware.Static


-- MODEL
data Answer = Answer
  { err :: Maybe String
  , word :: Text
  } deriving (Show, Generic)

instance ToJSON Answer

data User = User
  { email :: Text
  }

instance FromRow User where
  fromRow = User <$> field

-- APP
main :: IO ()
main = do
  conn <- connectPostgreSQL ("host='127.0.0.1' dbname='haskell' user='test' password='test'")
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    server conn

server :: Connection -> ScottyM ()
server conn =
  get "/:word" $ do
    wordParam <- param "word"
    userEmails <- liftIO (query_ conn "select email from users" :: IO [User])
    json Answer {err = Nothing, word = wordParam <> Data.Text.concat (email <$> userEmails)}