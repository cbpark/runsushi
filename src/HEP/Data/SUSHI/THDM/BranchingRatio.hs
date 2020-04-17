module HEP.Data.SUSHI.THDM.BranchingRatio
    (
      runh2decays
    , runSushiWithBR
    , getBRHpW
    ) where

import HEP.Data.SUSHI.THDM.CrossSection (getXSH2, runSushi)
import HEP.Data.SUSHI.THDM.Model        (BRH2 (..), InputParam, mkModelFiles,
                                         paramToArgsH2)

import HEP.Data.THDM.Parser             (parseBRH2)

import Control.Monad.Trans.Reader       (ReaderT, ask)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString.Char8            (pack)
import Data.Text.Lazy.Builder           (Builder)
import Pipes
import System.Process                   (readProcessWithExitCode)

runh2decays :: MonadIO m
             => FilePath -> Producer (Maybe BRH2) (ReaderT InputParam m) ()
runh2decays h2decaysExe = do
    inputArgs <- paramToArgsH2 <$> lift ask
    (_, brs0, _) <- liftIO (readProcessWithExitCode h2decaysExe inputArgs "")
    case parseOnly parseBRH2 (pack brs0) of
        Left _         -> do liftIO . putStrLn $ "---- error from h2decays!"
                             yield Nothing
        Right (_, brs) -> yield (Just brs)

getBR :: Monad m
      => (BRH2 -> Double)
      -> Pipe (Maybe BRH2) (Maybe Double) (ReaderT InputParam m) ()
getBR select = do
    brs0 <- await
    yield $ case brs0 of
                Nothing  -> Nothing
                Just brs -> Just (select brs)

getBRHpW :: Monad m => Pipe (Maybe BRH2) (Maybe Double) (ReaderT InputParam m) ()
getBRHpW = getBR _h2HpmW

runSushiWithBR :: MonadIO m
               => Double
               -> FilePath
               -> FilePath
               -> FilePath  -- ^ the executable path of SusHi
               -> Pipe (Maybe Double) (Maybe Builder) (ReaderT InputParam m) ()
runSushiWithBR sqrtS workDir inpTmpF sushiexe = do
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
