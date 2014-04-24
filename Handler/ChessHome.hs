{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.ChessHome where

import Import
import Yesod.Auth
import Data.List
import Text.Julius

notLoggedIn = "Must be logged in!"
notYourGame = "This is not your game!"
invalidArguments = "Invalid arguments."

xor :: Bool -> Bool -> Bool -- GHC.7-8, base-4.7.0.0 implements this
x `xor` y = (x || y) && (not (x && y))

getChessHomeR :: Handler Html
getChessHomeR = do
    defaultLayout $ do
      setTitle "Dark Chess - home"
      $(widgetFile "chesshome")

getGameListR :: Handler Html
getGameListR = do
    mauth <- maybeAuth
    case mauth of
      Nothing -> setMessage notLoggedIn >> redirect ChessHomeR
      Just (Entity k u) -> do
        games <- runDB $ selectList ([GameUserA ==. k] ||. [GameUserB ==. Just k]) []
        moves <- runDB $ mapM ((\k -> selectList [MoveGame ==. k] []) . entityKey) games
        users <- runDB $ selectList [] []
        let gameMoves = zip3 [1..] games moves
            findUserName mu = head . map (userIdent . entityVal) $ filter ((== mu) . Just . entityKey) users
        defaultLayout $ do
          setTitle "Dark Chess - game list"
          $(widgetFile "gamelist")

getBoardR :: Key Game -> Handler Html
getBoardR gid = do
    mauth <- maybeAuth
    case mauth of
      Nothing -> setMessage notLoggedIn >> redirect ChessHomeR
      Just (Entity k u) -> do
        Just move <- runDB $ selectFirst [MoveGame ==. gid] [Desc MoveId] -- FIXME: unsafe
        liftIO $ print $ entityKey move
        mgame <- runDB $ selectFirst ([GameId ==. gid, GameUserA ==. k] ||. [GameId ==. gid, GameUserB ==. Just k]) []
        case mgame of
          Nothing -> setMessage notYourGame >> redirect GameListR
          Just (Entity _ game) -> do
            let color = if gameUserA game == k then "white" else "black" :: Text
            defaultLayout $ do
              setTitle "Dark Chess - board"
              addScriptRemote "//code.jquery.com/jquery-1.11.0.min.js"
              addScript $ StaticR js_chessboard_0_3_0_min_js
              addScript $ StaticR js_chess_min_js
              addScript $ StaticR js_underscore_min_js
              addStylesheet $ StaticR css_chessboard_0_3_0_min_css
              $(widgetFile "board")

postBoardR :: Key Game -> Handler ()
postBoardR gid = do
    mauth <- maybeAuth
    case mauth of
      Nothing -> setMessage notLoggedIn >> redirect ChessHomeR
      Just (Entity k u) -> do
        mgame <- runDB $ selectFirst ([GameId ==. gid, GameUserA ==. k] ||. [GameId ==. gid, GameUserB ==. Just k]) []
        case mgame of
          Nothing -> setMessage notYourGame >> redirect GameListR
          Just game -> do
            mfen <- lookupPostParam "fen"
            case mfen of
              Nothing -> setMessage invalidArguments >> redirect (BoardR gid)
              Just fen -> do
                runDB . insert_ $ Move gid fen
                redirect $ BoardR gid

