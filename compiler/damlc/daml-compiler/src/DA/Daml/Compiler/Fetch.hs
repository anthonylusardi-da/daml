-- Copyright (c) 2020 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module DA.Daml.Compiler.Fetch (
  LedgerArgs(..), runWithLedgerArgs,
  fetchDar
  ) where

import Control.Lens (toListOf)
import Data.List.Extra (nubSort)
import Data.String (fromString)
import qualified Data.Text.Lazy as TL

import qualified DA.Ledger as L (
  ClientSSLConfig(..),
  Host(..),
  LedgerService,
  Package(..),
  PackageId(..),
  Port(..),
  TimeoutSeconds,
  Token,
  configOfHostAndPort,
  getLedgerIdentity,
  getPackage,
  runLedgerService,
  setToken,
  )

import qualified DA.Daml.LF.Ast as LF
import qualified DA.Daml.LF.Ast.Optics as LF (packageRefs)
import qualified DA.Daml.LF.Proto3.Archive as LFArchive

data LedgerArgs = LedgerArgs
  { host :: String
  , port :: Int
  , tokM :: Maybe L.Token
  , sslConfigM :: Maybe L.ClientSSLConfig
  }

instance Show LedgerArgs where
  show LedgerArgs{host,port} = host <> ":" <> show port

data Dar -- TODO

fetchDar :: LedgerArgs -> LF.PackageId -> FilePath -> IO ()
fetchDar ledgerArgs pid saveAs = do
  xs <- downloadAllReachablePackages ledgerArgs pid
  let _ = undefined xs saveAs writeDarToFile encodeAsDar -- TODO
  return ()

writeDarToFile :: Dar -> FilePath -> IO ()
writeDarToFile = undefined

encodeAsDar :: LF.PackageId -> [(LF.PackageId,LF.Package)] -> IO Dar
encodeAsDar = undefined

-- | Download all Packages reachable from a PackageId; fail if any don't exist or can't be decoded.
downloadAllReachablePackages :: LedgerArgs -> LF.PackageId -> IO [(LF.PackageId,LF.Package)]
downloadAllReachablePackages ledgerArgs pid = loop [] [pid]
  where
    loop :: [(LF.PackageId,LF.Package)] -> [LF.PackageId] -> IO [(LF.PackageId,LF.Package)]
    loop acc = \case
      [] -> return acc
      pid:morePids ->
        if pid `elem` [ pid | (pid,_) <- acc ]
        then do
          putStrLn $ "Already Got: " <> show (LF.unPackageId pid)
          loop acc morePids
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
    Just pkg -> return $ decodePackageExpect pid pkg
  where
    convPid :: LF.PackageId -> L.PackageId
    convPid (LF.PackageId text) = L.PackageId $ TL.fromStrict text

decodePackageExpect :: LF.PackageId -> L.Package -> LF.Package
decodePackageExpect pid (L.Package bs) = do
  let mode = LFArchive.DecodeAsMain -- DecodeAsDependency -- ???
  let either = LFArchive.decodePackage mode pid bs
  case either of
    Left err -> error $ show err
    Right pkg -> pkg

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
