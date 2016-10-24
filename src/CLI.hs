{-# LANGUAGE DataKinds #-}

module CLI (ski) where


import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString, pack)
import Control.Monad.Trans   (liftIO)
import Options.Declarative
import Text.Parsec           (parse)
import Text.Parsec.Error     (errorMessages, messageString)

import Parser (context, expr)
import Expr   (Context, emptyContext, compile, unlambda)
import PPrint (pp)


ski :: Flag "l" '["load"] "FilePath" "path to source file" (Maybe String)
    -> Flag "" '["no-subst"] "Bool" "only unlambda not substitute defined variable" Bool
    -> Arg "Code" String
    -> Cmd "convert \"Lambda Abstraction\" to \"SKI Combinator\"" ()
ski src nosubst code = do
  context <- liftIO $ loadContext $ get src
  let result = case (parse expr "" . pack $ get code, get nosubst) of
                    (Left  parseError, _)     -> show parseError
                    (Right parsedCode, True)  -> pp $ unlambda parsedCode
                    (Right parsedCode, False) -> pp $ compile context parsedCode
  liftIO $ putStrLn result


loadContext :: Maybe String -> IO Context
loadContext Nothing         = return emptyContext
loadContext (Just filepath) = do
  Right context <- parse context "" <$> BS.readFile filepath
  return context
