{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Haskell.Session.Instances (
    module Control.Monad.Ghc,
    module Exception,
    module DynFlags
) where

import Control.Monad.Ghc
import Control.Monad.IO.Class     (MonadIO)
import Control.Monad.Trans.Except
import DynFlags (HasDynFlags (getDynFlags))
import Exception (ExceptionMonad (gcatch, gmask))
import Control.Monad.Trans.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.State.Lazy as StateL
import qualified Control.Monad.Catch as Catch
import  Control.Monad.Catch



instance (GhcMonad m,  MonadIO m) => GhcMonad (IdentityT m) where
    getSession = lift $ getSession
    setSession = lift . setSession

instance (GhcMonad m,  MonadIO m) => GhcMonad (ExceptT e m) where
    getSession = lift $ getSession
    setSession = lift . setSession

instance (GhcMonad m,  MonadIO m) => GhcMonad (StateT s m) where
    getSession = lift $ getSession
    setSession = lift . setSession

instance (MonadIO m, ExceptionMonad m)  => ExceptionMonad (ExceptT e m) where
    gcatch (ExceptT m) f = ExceptT $ gcatch m (runExceptT . f)
    gmask f = ExceptT $ gmask f' where
        f' g = runExceptT $ f g' where
            g' (ExceptT m)= ExceptT $ g m

instance (MonadIO m, ExceptionMonad m)  => ExceptionMonad (IdentityT m) where
    gcatch (IdentityT m) f = IdentityT $ gcatch m (runIdentityT . f)
    gmask f = IdentityT $ gmask f' where
        f' g = runIdentityT $ f g' where
            g' (IdentityT m)= IdentityT $ g m

instance (MonadIO m, ExceptionMonad m)  => ExceptionMonad (StateT s m) where
    gcatch m h = StateT $ \ s -> runStateT m s `gcatch` \ e -> runStateT (h e) s
    gmask a = StateT $ \s -> gmask $ \u -> runStateT (a $ q u) s
        where q :: (m (a, s) -> m (a, s)) -> StateT s m a -> StateT s m a
              q u (StateT b) = StateT (u . b)

instance (MonadIO m, HasDynFlags m) => HasDynFlags (IdentityT m) where
    getDynFlags = lift $ getDynFlags

instance (MonadIO m, HasDynFlags m) => HasDynFlags (ExceptT e m) where
    getDynFlags = lift $ getDynFlags

instance (MonadIO m, HasDynFlags m) => HasDynFlags (StateT s m) where
    getDynFlags = lift $ getDynFlags
