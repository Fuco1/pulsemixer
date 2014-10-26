module Pulse.Monad.Monad
       ( PulseT(..)
       , Pulse
       , getContext
       , getData
       ) where

import Control.Applicative ((<$>))

import Control.Concurrent.STM.TVar (TVar(..))

import Control.Monad (MonadPlus(..))
import Control.Monad.CatchIO (MonadCatchIO(..))
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.RWS.Strict (RWST(..), runRWST, mapRWST, asks, get)
import Control.Monad.Trans.Class (MonadTrans(..))

import Data.Monoid (Monoid(..))

import Pulse.Internal.Context (RawContextPtr)

import Pulse.Monad.Data

newtype PulseT m n = PulseT { unPulseT :: RWST Env () (TVar State) m n }
type Pulse = PulseT IO

instance Functor m => Functor (PulseT m) where
  fmap f (PulseT x) = PulseT $ fmap f x

instance Monad m => Monad (PulseT m) where
  (PulseT f) >>= g = PulseT $ f >>= unPulseT . g
  (PulseT f) >> (PulseT g) = PulseT $ f >>  g
  return = PulseT . return

instance MonadIO m => MonadIO (PulseT m) where
  liftIO = PulseT . liftIO

instance MonadPlus m => MonadPlus (PulseT m) where
  mzero = PulseT mzero
  mplus (PulseT x) (PulseT y) = PulseT $ mplus x y

instance MonadTrans PulseT where
  lift = PulseT . lift

-- why do I need this? It's in the Control.Monad.CatchIO
instance (Monoid w, MonadCatchIO m) => MonadCatchIO (RWST r w s m) where
  m `catch` f = RWST $ \r s -> runRWST m r s `catch` \e -> runRWST (f e) r s
  block       = mapRWST block
  unblock     = mapRWST unblock

instance MonadCatchIO m => MonadCatchIO (PulseT m) where
  catch (PulseT code) handler = PulseT $ catch code (unPulseT . handler)
  block (PulseT code) = PulseT $ block code
  unblock (PulseT code) = PulseT $ unblock code

getContext :: Monad m => PulseT m RawContextPtr
getContext = PulseT (asks envContext)

getData :: Monad m => PulseT m (TVar State)
getData = PulseT get
