{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices  #-}

module LexerState where
import Control.Lens
import Tokens

data AlexUserState = AlexUserState { _curString :: !String,
                                     _commentsLevel :: !Int,
                                     _startCode :: !Int,
                                     _stringStart :: !Pos }
                     deriving Show

$(makeLenses ''AlexUserState)

nonePos :: Pos
nonePos = Pos 0 0

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "" 0 0 nonePos
