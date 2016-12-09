#!/usr/bin/env runhaskell

import Control.Monad
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory as D
import System.FilePath.Posix
import System.Random



kubeadmTokenFile :: IO String
kubeadmTokenFile = do
  h <- getHomeDirectory
  return $ joinPath [h, ".google/kubeadm_token"]


kubeadmToken :: Action String
kubeadmToken = do
  tokenFile <- liftIO kubeadmTokenFile
  need [tokenFile]
  liftIO $ readFile tokenFile


main :: IO ()
main = shakeArgs shakeOptions $ do
  tokenFile <- liftIO kubeadmTokenFile

  want [tokenFile]

  tokenFile %> \out -> do
    token <- liftIO $ do
      g <- newStdGen
      let (pre, post) = splitAt 6 $ take 16 $ randomRs ('!','~') g
      return $ pre ++ "." ++ post
    unit $ writeFile' out token
    unit $ cmd "chmod 400" out

  "clobber" ~> do
    removeFileIfExists tokenFile


removeFileIfExists :: String -> Action ()
removeFileIfExists filename = liftIO $ do
  e <- D.doesFileExist filename
  when e $ removeFile filename
