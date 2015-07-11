{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices  #-}

module LexerState where
import Control.Lens

data AlexUserState = AlexUserState { _curString :: !String,
                                     _commentsLevel :: !Int,
                                     _startCode :: !Int }
                     deriving Show

$(makeLenses ''AlexUserState)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "" 0 0
