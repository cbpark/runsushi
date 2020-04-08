{-# LANGUAGE RecordWildCards #-}

module HEP.Data.SUSHI.THDM.BranchingRatio where

import HEP.Data.SUSHI.THDM.CrossSection (getXSH2, runSushi)
import HEP.Data.SUSHI.THDM.Model        (BRH2 (..), InputParam, mkModelFiles,
                                         paramToArgs)

import HEP.Data.THDM.Parser             (parseBRH2)

import Control.Monad.Trans.Reader       (ReaderT, ask)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString.Char8            (pack)
import Data.Text.Lazy.Builder           (Builder)
import Pipes
import System.Process                   (readProcessWithExitCode)

-- import Control.Monad                    (forever)

-- runh2decays :: MonadIO m
--             => FilePath -> Pipe InputParam (Maybe BRH2) m ()
-- runh2decays h2decaysExe = forever $ do
--     inputArgs <- paramToArgs <$> await
--     (_, brs0, _) <- liftIO (readProcessWithExitCode h2decaysExe inputArgs "")
--     case parseOnly parseBRH2 (pack brs0) of
--         Left _         -> do liftIO . putStrLn $ "---- error from h2decays!"
--                              yield Nothing
--         Right (_, brs) -> yield (Just brs)

runh2decays :: MonadIO m
             => FilePath -> Producer (Maybe BRH2) (ReaderT InputParam m) ()
runh2decays h2decaysExe = do
    inputArgs <- paramToArgs <$> lift ask
    (_, brs0, _) <- liftIO (readProcessWithExitCode h2decaysExe inputArgs "")
    case parseOnly parseBRH2 (pack brs0) of
        Left _         -> do liftIO . putStrLn $ "---- error from h2decays!"
                             yield Nothing
        Right (_, brs) -> yield (Just brs)

-- getBRHpW :: MonadIO m => Pipe (Maybe BRH2) (Maybe Double) m ()
-- getBRHpW = forever $ do
--     brs <- await
--     yield $ case brs of
--                 Nothing        -> Nothing
--                 Just BRH2 {..} -> Just _h2HpmW

getBRHpW :: Monad m => Pipe (Maybe BRH2) (Maybe Double) (ReaderT InputParam m) ()
getBRHpW = do
    brs <- await
    yield $ case brs of
                Nothing        -> Nothing
                Just BRH2 {..} -> Just _h2HpmW

runSushiHpW :: MonadIO m
            => Double
            -> FilePath
            -> FilePath
            -> FilePath  -- ^ the executable path of SusHi
            -> Pipe (Maybe Double) (Maybe Builder) (ReaderT InputParam m) ()
runSushiHpW sqrtS workDir inpTmpF sushiexe = do
    brHpW <- await
    case brHpW of
        Nothing -> yield Nothing
        Just br -> if br < 1.0e-8
                   then yield Nothing
                   else do -- liftIO $ print br
                           inp <- lift ask
                           yield inp
                               >-> mkModelFiles sqrtS workDir inpTmpF
                               >-> runSushi sushiexe
                               >-> getXSH2 sqrtS br
