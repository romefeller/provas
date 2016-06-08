{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
import Yesod.Static
import Database.Persist.Sqlite
import Data.Text
import Control.Monad
--------------- TIPO e INSTANCIA DE PersistField ----------
data Arquivo = Arquivo {nome :: Text, arq :: Maybe FileInfo} deriving Show

instance Show FileInfo where
    show f = unpack $ fileName f

instance PersistField Arquivo where
  toPersistValue (Arquivo t (Just f)) = PersistText (fileName f)
  toPersistValue (Arquivo t Nothing) = PersistText (pack "")
  fromPersistValue (PersistText t) = Right $ Arquivo t Nothing
  fromPersistValue _ = Left "FileInfo values must be converted from PersistText"
-------------- DEFINE QUE Arquivo eh Varchar --------------
instance PersistFieldSql Arquivo where
  sqlType _ = SqlString
