-- Copyright (c) 2020 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module DA.Daml.Compiler.Fetch (
  LedgerArgs(..), runWithLedgerArgs,
  createDarFile,
  fetchDar
  ) where

import Control.Lens (toListOf)
import Data.List.Extra (nubSort)
import Data.String (fromString)
import System.Directory.Extra (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import qualified "zip" Codec.Archive.Zip as Zip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import DA.Daml.Compiler.Dar (PackageConfigFields(..),PackageSdkVersion(..),createArchive)
--import Development.IDE.Types.Location (toNormalizedFilePath)
import qualified DA.Daml.LF.Ast as LF
import qualified DA.Daml.LF.Ast.Optics as LF (packageRefs)
import qualified DA.Daml.LF.Proto3.Archive as LFArchive
import qualified DA.Ledger as L
import qualified SdkVersion

data LedgerArgs = LedgerArgs
  { host :: String
  , port :: Int
  , tokM :: Maybe L.Token
  , sslConfigM :: Maybe L.ClientSSLConfig
  }

instance Show LedgerArgs where
  show LedgerArgs{host,port} = host <> ":" <> show port

-- | Create a DAR file by running a ZipArchive action.
createDarFile :: FilePath -> Zip.ZipArchive () -> IO ()
createDarFile fp dar = do
    createDirectoryIfMissing True $ takeDirectory fp
    Zip.createArchive fp dar
    putStrLn $ "Created " <> fp

-- | Reconstruct a DAR file by downloading packages from a ledger
fetchDar :: LedgerArgs -> LF.PackageId -> FilePath -> IO ()
fetchDar ledgerArgs rootPid saveAs = do
  xs <- downloadAllReachablePackages ledgerArgs rootPid
  [pkg] <- pure [ pkg | (pid,pkg) <- xs, pid == rootPid ]
  let (dalf,pkgId) = LFArchive.encodeArchiveAndHash pkg

  let dalfDependencies :: [(T.Text,BS.ByteString,LF.PackageId)] =
        [ (txt,bs,pkgId)
        | (pid,pkg) <- xs, pid /= rootPid
        , let txt = T.pack ("dep-" <> T.unpack (LF.unPackageId pid))
        , let (bsl,pkgId) = LFArchive.encodeArchiveAndHash pkg
        , let bs = BSL.toStrict bsl
        ]

  let pName :: LF.PackageName = LF.PackageName $ T.pack "reconstructed"
  let pSrc :: String = undefined -- "SRC"
  let srcRoot = undefined -- toNormalizedFilePath "DOT"
  let pkgConf =
        PackageConfigFields
        { pName
        , pSrc
        , pExposedModules = Nothing
        , pVersion = Nothing
        , pDependencies = []
        , pDataDependencies = []
        , pSdkVersion = PackageSdkVersion SdkVersion.sdkVersion
        }
  let za = createArchive pkgConf pkgId dalf dalfDependencies srcRoot [] [] []
  createDarFile saveAs za


-- | Download all Packages reachable from a PackageId; fail if any don't exist or can't be decoded.
downloadAllReachablePackages :: LedgerArgs -> LF.PackageId -> IO [(LF.PackageId,LF.Package)]
downloadAllReachablePackages ledgerArgs pid = loop [] [pid]
  where
    loop :: [(LF.PackageId,LF.Package)] -> [LF.PackageId] -> IO [(LF.PackageId,LF.Package)]
    loop acc = \case
      [] -> return acc
      pid:morePids ->
        if pid `elem` [ pid | (pid,_) <- acc ]
        then loop acc morePids
        else do
          putStrLn $ "Downloading: " <> show (LF.unPackageId pid)
          pkg <- downloadPackage ledgerArgs pid
          loop ((pid,pkg):acc) (packageRefs pkg ++ morePids)

packageRefs :: LF.Package -> [LF.PackageId]
packageRefs pkg = nubSort [ pid | LF.PRImport pid <- toListOf LF.packageRefs pkg ]

-- | Download the Package identified by a PackageId; fail if it doesn't exist or can't be decoded.
downloadPackage :: LedgerArgs -> LF.PackageId -> IO LF.Package
downloadPackage ledgerArgs pid = do
  let ls :: L.LedgerService (Maybe L.Package) = do
        lid <- L.getLedgerIdentity
        L.getPackage lid $ convPid pid
  runWithLedgerArgs ledgerArgs ls >>= \case
    Nothing -> error $ "Unable to download package with identity: " <> show pid
    Just (L.Package bs) -> do
      let mode = LFArchive.DecodeAsMain
      case LFArchive.decodePackage mode pid bs of
        Left err -> error $ show err
        Right pkg -> return pkg
  where
    convPid :: LF.PackageId -> L.PackageId
    convPid (LF.PackageId text) = L.PackageId $ TL.fromStrict text

runWithLedgerArgs :: LedgerArgs -> L.LedgerService a -> IO a
runWithLedgerArgs args ls = do
    let LedgerArgs{host,port,tokM} = args
    let ls' = case tokM of Nothing -> ls; Just tok -> L.setToken tok ls
    let timeout = 30 :: L.TimeoutSeconds
    let ledgerClientConfig =
            L.configOfHostAndPort
                (L.Host $ fromString host)
                (L.Port port)
                (sslConfigM args)
    L.runLedgerService ls' timeout ledgerClientConfig
