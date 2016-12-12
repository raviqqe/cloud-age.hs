import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as C
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory as D
import System.FilePath.Posix
import System.Process
import System.Random



adminFile = "etc/admin.conf"


kubeadmTokenFile :: MonadIO m => m String
kubeadmTokenFile = liftIO $ do
  h <- getHomeDirectory
  return $ joinPath [h, ".google/kubeadm_token"]


kubeadmToken :: Action String
kubeadmToken = do
  tokenFile <- kubeadmTokenFile
  need [tokenFile]
  liftIO $ readFile tokenFile


base64 :: String -> String
base64 = C.unpack . encode . C.pack


randomString :: Int -> IO String
randomString n = do
  g <- newStdGen
  return $ take n $ base64 $ take (n * 2) (randoms g :: String)


main :: IO ()
main = shakeArgs shakeOptions $ do
  tokenFile <- kubeadmTokenFile

  tokenFile %> \out -> do
    token <- do
      pre <- liftIO $ randomString 6
      post <- liftIO $ randomString 16
      return $ pre ++ "." ++ post
    unit $ writeFile' out token
    unit $ cmd "chmod 400" out

  "clobber" ~> do
    need ["destroy"]
    removeFileIfExists tokenFile

  forM_ ["apply", "plan"] $ \target -> do
    target ~> do
      token <- kubeadmToken
      cmd $ "terraform " ++ target ++ " -var token='\"" ++ token ++ "\"'"

  adminFile %> \out -> do
    need ["apply"]

  "destroy" ~> do
    removeFileIfExists adminFile
    cmd "terraform destroy -force"
  "show" ~> cmd "terraform show"

  "proxy" ~> do
    need [adminFile]
    liftIO $ void $ system $ "sh -c \"kubectl --kubeconfig " ++ adminFile ++ " proxy --port 8080 &\""


removeFileIfExists :: String -> Action ()
removeFileIfExists filename =
  liftIO $ flip when (removeFile filename) =<< D.doesFileExist filename
