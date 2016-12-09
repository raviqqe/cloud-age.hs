#!/usr/bin/env runhaskell

import Control.Monad
import Control.Monad.IO.Class
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory as D
import System.FilePath.Posix
import System.Random



kubeadmTokenFile :: MonadIO m => m String
kubeadmTokenFile = liftIO $ do
  h <- getHomeDirectory
  return $ joinPath [h, ".google/kubeadm_token"]


kubeadmToken :: Action String
kubeadmToken = do
  tokenFile <- kubeadmTokenFile
  need [tokenFile]
  liftIO $ readFile tokenFile


main :: IO ()
main = shakeArgs shakeOptions $ do
  tokenFile <- kubeadmTokenFile

  want [tokenFile]

  tokenFile %> \out -> do
    token <- liftIO $ do
      g <- newStdGen
      let (pre, post) = splitAt 6 $ take 16 $ randomRs ('!','~') g
      return $ pre ++ "." ++ post
    unit $ writeFile' out token
    unit $ cmd "chmod 400" out

  "clobber" ~> do
    need ["destroy"]
    removeFileIfExists tokenFile

  "apply" ~> do
    token <- kubeadmToken
    cmd $ "terraform apply -var token='\"" ++ token ++ "\"'"

  "destroy" ~> cmd Shell "echo yes | terraform destroy"
  "show" ~> cmd "terraform show"


removeFileIfExists :: String -> Action ()
removeFileIfExists filename =
  liftIO $ flip when (removeFile filename) =<< D.doesFileExist filename
