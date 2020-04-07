module HEP.Data.SUSHI.THDM.BranchingRatio where

import HEP.Data.THDM                    (BRH2, InputParam, paramToArgs)
import HEP.Data.THDM.Parser             (parseBRH2)

import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString.Char8            (pack)
import Pipes
import System.Process                   (readProcessWithExitCode)

import Control.Monad                    (forever)

runh2decays :: MonadIO m
            => FilePath -> Pipe InputParam (Maybe (InputParam, BRH2)) m ()
runh2decays h2decaysExe = forever $ do
    inputArgs <- paramToArgs <$> await
    (_, brs0, _) <- liftIO (readProcessWithExitCode h2decaysExe inputArgs "")
    case parseOnly parseBRH2 (pack brs0) of
        Left _    -> do liftIO . putStrLn $ "---- error from h2decays!"
                        yield Nothing
        Right brs -> yield (Just brs)
