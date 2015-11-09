module Language.Haskell.Config where


data Config = Config { topDir :: FilePath
                     , pkgDb  :: PackageDB
                     } deriving Show


data PackageDB = PackageDB { local  :: FilePath
                           , global :: FilePath
                           } deriving Show
