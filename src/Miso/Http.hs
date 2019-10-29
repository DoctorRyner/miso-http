{-# LANGUAGE CPP #-}

module Miso.Http where

import           Control.Exception           (SomeException (..))
#ifdef ghcjs_HOST_OS
#else
import           Control.Monad.IO.Class      (liftIO)
#endif
import           Data.Aeson
import qualified Data.ByteString.Char8       as BS
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as TLEncoding
import           JSDOM.Custom.XMLHttpRequest as JSDOM hiding (error)
import           JSDOM.Generated.Enums
import           Language.Javascript.JSaddle hiding (JSM)
import           Miso
import           Miso.SPA.Types
import           Miso.SPA.Utils              (try)
import           Miso.String

data Request payload
    = GET
        { url     :: MisoString
        , headers :: [(MisoString, MisoString)]
        }
    | POST
        { url     :: MisoString
        , headers :: [(MisoString, MisoString)]
        , payload :: Maybe payload
        }

get :: Request ()
get = GET
    { url     = ""
    , headers = [("Content-Type", "application/json")]
    }

post :: Request payload
post = POST
    { url     = ""
    , headers = [("Content-Type", "application/json")]
    , payload = Nothing
    }

getLocalFile :: MisoString -> JSM (Response MisoString)
getLocalFile path = Miso.Http.sendPlain $ get { url = path, headers = [] }

send :: (FromJSON response, ToJSON response, ToJSON payload) => Request payload -> JSM (Response response)
send r = newXMLHttpRequest >>= \req -> case r of
    GET url headers -> do
        openSimple req ("GET" :: MisoString) url
        mapM_ (uncurry $ setRequestHeader req) headers
        eitherXhrError <- try $ JSDOM.send req
        case eitherXhrError of
            Right _ -> do
                resRaw' <- valToStr =<< getResponse req
                statusNum <- getStatus req
                let resBS = TLEncoding.encodeUtf8 $ TL.pack $ unpack resRaw'
                case eitherDecode resBS of
                    Right res -> do
                        let isOk = Prelude.take 1 (show statusNum) == "2"
                        if isOk
                            then pure $ Ok res
                            else pure $ HttpError (ms $ encode res) (fromEnum statusNum)
                    Left err  -> pure $ HttpError (ms err) (fromEnum statusNum)
            Left (SomeException _) -> pure $ HttpError "XHRError" 404
    POST url headers maybePayload -> do
        openSimple req ("POST" :: MisoString) url
        mapM_ (uncurry $ setRequestHeader req) headers
        eitherXhrError <- try $ case maybePayload of
            Just payload -> JSDOM.sendString req $ ms $ encode payload
            Nothing      -> JSDOM.send req
        case eitherXhrError of
            Right _ -> do
                resRaw <- valToStr =<< getResponse req
                statusNum <- fromEnum <$> getStatus req
                let resBS = TLEncoding.encodeUtf8 $ TL.pack $ unpack resRaw
                case eitherDecode resBS of
                    Right res -> do
                        let isOk = Prelude.take 1 (show statusNum) == "2"
                        if isOk
                            then pure $ Ok res
                            else pure $ HttpError (ms $ encode res) statusNum
                    Left err  -> pure $ HttpError (ms err) statusNum
            Left (SomeException _) -> pure $ HttpError "XHRError" 404

sendPlain :: ToJSON payload => Request payload -> JSM (Response MisoString)
sendPlain = \case
    GET url headers -> do
        req <- newXMLHttpRequest
        openSimple req ("GET" :: MisoString) url
        mapM_ (uncurry $ setRequestHeader req) headers
        eitherXhrError <- try $ JSDOM.send req
        case eitherXhrError of
            Right _ -> do
                res       <- valToStr =<< getResponse req
                statusNum <- getStatus req
                let isOk = Prelude.take 1 (show statusNum) == "2"
                if isOk
                    then pure $ Ok res
                    else pure $ HttpError res (fromEnum statusNum)
            Left (SomeException _) -> pure $ HttpError "XHRError" 404
    POST url headers maybePayload -> do
        req <- newXMLHttpRequest
        openSimple req ("POST" :: MisoString) url
        mapM_ (uncurry $ setRequestHeader req) headers
        eitherXhrError <- try $ case maybePayload of
            Just payload -> JSDOM.sendString req $ ms $ encode payload
            Nothing      -> JSDOM.send req
        case eitherXhrError of
            Right _ -> do
                res <- valToStr =<< getResponse req
                statusNum <- fromEnum <$> getStatus req
                let isOk = Prelude.take 1 (show statusNum) == "2"
                if isOk
                    then pure $ Ok res
                    else pure $ HttpError res statusNum
            Left (SomeException _) -> pure $ HttpError "XHRError" 404
