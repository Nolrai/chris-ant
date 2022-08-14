{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module ParseInput (parseExample, Parser, CustomError, buildExample) where

import Example
import Lib
import Relude
import Text.Megaparsec as M
import Text.Megaparsec.Char (eol, char, hspace1)
import Text.Megaparsec.Char.Lexer
import Data.Vector as V
import Control.Lens

type CustomError = Void

type Parser = Parsec CustomError Text

eol' :: Parser Text
eol' = eol

lexeme' :: Parser a -> Parser a
lexeme' = lexeme (space hspace1 M.empty M.empty)

word64 :: Parser Word64
word64 = lexeme' decimal

bit :: Parser IsOpen
bit = lexeme' $ (char '0' *> pure False <|> char '1' *> pure True)

lineParsers :: Int -> Vector (Parser (Word64, Word64, Bool))
lineParsers size = V.replicate size $ ((,,) <$> word64 <*> word64 <*> bit) <* eol'

parseExample :: Parser Example
parseExample = do
    size <- decimal <* eol'
    v <- sequenceA $ lineParsers size
    -- trace ("v = " <> show v) $ pure ()
    expected' <- optional (decimal <* eol')
    eof
    pure $ buildExample v expected'

buildExample :: Vector (Word64, Word64, IsOpen) -> Maybe Word64 -> Example
buildExample v expected' =
    let _antPos = 0
        _doorStates = (^. _3) <$> v
        _startState = AntState {..}
        _doorSpecs = (\ (_src, _dest, _) -> Door {..}) <$> v
        _lineLength = V.maximum $ V.map (^. src) _doorSpecs
        _env = AntEnv {..}
        _expected = expected'
    in Example {..}