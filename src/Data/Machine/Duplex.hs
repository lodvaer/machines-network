{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}

-- |
-- Module      :  Data.Machine.Duplex
-- Copyright   :  Lodvær 2015
-- License     :  BSD3
--
-- Maintainer  :  Lodvær <lodvaer@gmail.com>
-- Stability   :  provisional
-- Portability :  unknown
--
-- Half duplexed machines.
module Data.Machine.Duplex
    ( -- * Types
      Channel(..)
    , HalfDuplexT
    , ServerT
    , ClientT
      -- * Operations
    , send
    , listen
    , weakListen
    , recv
    , weakRecv
    , (>~>), (<~<)
    , flipDuplex
      -- * Useful duplex machines
    , duplexEcho
    , reflect
      -- * Process interface
    , (~<), (>~)
    , toSimplex
    , toReversedSimplex
    , (~<~>)
    ) where

import Control.Applicative ((<|>))
import Data.Machine
import Data.Void


-- $setup
-- >>> import Control.Monad.Trans.Class
-- >>> :set -XScopedTypeVariables

infixr 7 <~<
infixl 7 >~>
infixr 8 ~<
infixl 9 >~
infixl 9 ~<~>

data Channel a a' b' b where
    Send       :: a' -> Channel a a' b' ()
    WeakRecv   ::       Channel a a' b' b'
    Recv       ::       Channel a a' b' b'
    WeakListen ::       Channel a a' b' a
    Listen     ::       Channel a a' b' a

-- | A half-duplex stream transducer.
--
-- @
--        Upstream | Downstream
--            +---------+
--            |         |
-- send   a' <==       <== b' recv
--            |    m    |
-- listen a  ==>       ==> b  yield
--            |         |
--            +---------+
-- @
--
-- 'recv' and 'yield' will always succeed, while 'send' and 'listen'
-- may fail if there's a collision on the other end.
--
-- Additionally there's 'weakRecv' that fails if downstream listens,
-- and 'weakListen' that fails if upstream either 'recv's or 'weakRecv's.
type HalfDuplexT m a a' b' b = MachineT m (Channel a a' b') b

-- | A duplex server, receiving a @b'@, yielding @b@.
type ServerT m b' b = HalfDuplexT m Void Void b' b

-- | A duplex client, listening for an @a@, yielding @a'@.
type ClientT m a a' = HalfDuplexT m a a' Void Void

-- | Send something upstream.
--
-- Blocks awaiting upstream action. Fails if upstream yields.
send :: a' -> PlanT (Channel a a' b') b m ()
send a = awaits (Send a)

-- | Listen for an upstream yield.
--
-- Blocks awaiting upstream action. Fails if upstream requires a send.
listen :: PlanT (Channel a a' b') b m a
listen = awaits Listen

-- | Listen for an upstream value, and fail if it's possible for upstream to
-- receive a send.
weakListen :: PlanT (Channel a a' b') b m a
weakListen = awaits WeakListen

-- | Blocks for a downstream send. Makes downstream listen fail.
-- Cannot fail.
recv :: PlanT (Channel a a' b') b m b'
recv = awaits Recv

-- | Blocks for a downstream action and fails if downstream requires a yield.
weakRecv :: PlanT (Channel a a' b') b m b'
weakRecv = awaits WeakRecv

-- | Repeat what's sent.
duplexEcho :: Monad m => HalfDuplexT m a b b a
duplexEcho = repeatedly $ (listen >>= yield) <|> (recv >>= send)

-- | Fit two 'HalfDuplexT's together.
(>~>) :: Monad m
      => HalfDuplexT m a a' b' b
      -> HalfDuplexT m b b' c' c
      -> HalfDuplexT m a a' c' c
ma >~> mb = MachineT $ runMachineT mb >>= \b -> case b of
  Stop                 -> return Stop
  Yield o f            -> return $ Yield o (ma >~> f)
  Await f Recv ff      -> return $ Await (\x -> ma >~> f x) Recv (ma >~> ff)
  Await f WeakRecv ff -> return $ Await (\x -> ma >~> f x) WeakRecv (ma >~> ff)
  Await f Listen ff    -> runMachineT ma >>= \a -> case a of
    Stop                     -> runMachineT $ stopped >~> ff
    Yield o f'               -> runMachineT $ f' >~> f o
    Await _ Recv _           -> runMachineT $ encased a >~> ff
    Await _ WeakRecv ff'     -> runMachineT $ ff' >~> encased b
    Await f' (Send i) ff'    -> return $ Await (\x -> f' x >~> encased b) (Send i) (ff' >~> encased b)
    Await f' Listen ff'      -> return $ Await (\x -> f' x >~> encased b) Listen (ff' >~> encased b)
    Await f' WeakListen ff'  -> return $ Await (\x -> f' x >~> encased b) WeakListen (ff' >~> encased b)
  Await f WeakListen ff   -> runMachineT ma >>= \a -> case a of
    Stop                     -> runMachineT $ stopped >~> ff
    Yield o f'               -> runMachineT $ f' >~> f o
    Await _ Recv _           -> runMachineT $ encased a >~> ff
    Await _ WeakRecv _       -> runMachineT  $ encased a >~> ff
    Await f' (Send i) ff'    -> return $ Await (\x -> f' x >~> encased b) (Send i) (ff' >~> encased b)
    Await f' Listen ff'      -> return $ Await (\x -> f' x >~> encased b) Listen (ff' >~> encased b)
    Await f' WeakListen ff'  -> return $ Await (\x -> f' x >~> encased b) WeakListen (ff' >~> encased b)
  Await f (Send i) ff -> runMachineT ma >>= \a -> case a of
    Stop                     -> runMachineT $ stopped >~> ff
    Yield _ _                -> runMachineT $ encased a >~> ff
    Await f' Recv _          -> runMachineT (f' i) >>= \x -> runMachineT (encased x >~> f ())
    Await f' WeakRecv _      -> runMachineT (f' i) >>= \x -> runMachineT (encased x >~> f ())
    Await f' (Send i') ff'   -> return $ Await (\x -> f' x >~> encased b) (Send i') (ff' >~> encased b)
    Await f' Listen ff'      -> return $ Await (\x -> f' x >~> encased b) Listen (ff' >~> encased b)
    Await f' WeakListen ff'  -> return $ Await (\x -> f' x >~> encased b) WeakListen (ff' >~> encased b)

-- | Fit two 'HalfDuplexT's together. Flipped '>~>'.
(<~<) :: Monad m
      => HalfDuplexT m b b' c' c
      -> HalfDuplexT m a a' b' b
      -> HalfDuplexT m a a' c' c
(<~<) = flip (>~>)

-- | Flip a machine such that downstream becomes upstream.
--
-- 'listen' becomes 'recv', 'weakListen' becomes 'weakRecv' and vice versa.
-- 'yield' becomes @('send' \<|\> return ())@, 'send' becomes 'yield'.
--
-- >>> runT $ flipDuplex (construct $ listen >>= send) >~> (construct $ send "test" >> listen >>= yield)
-- ["test"]
flipDuplex :: Monad m => HalfDuplexT m a a' b' b -> HalfDuplexT m b' b a a'
flipDuplex m = MachineT $ runMachineT m >>= \case
  Stop                  -> return Stop
  Yield o f             -> return $ Await (const $ flipDuplex f) (Send o) (flipDuplex f)
  Await f (Send o) _    -> return $ Yield o (flipDuplex $ f ())
  Await f Listen ff     -> return $ Await (flipDuplex . f) Recv (flipDuplex ff)
  Await f WeakListen ff -> return $ Await (flipDuplex . f) WeakRecv (flipDuplex ff)
  Await f Recv ff       -> return $ Await (flipDuplex . f) Listen (flipDuplex ff)
  Await f WeakRecv ff   -> return $ Await (flipDuplex . f) WeakListen (flipDuplex ff)

-- | Send something coming upstream back down, or something coming downstream
-- back up. Transfers control upstream first, then downstream when upstream
-- stops.
--
-- >>> runT $ reflect >~> (construct $ send 0 >> listen >>= yield)
-- [0]
-- >>> runT_ $ (construct $ yield 5 >> recv >>= lift . print) >~> reflect >~> (construct $ send 0 >> listen >>= lift . print)
-- 5
-- 0
reflect :: Monad m => HalfDuplexT m a a b b
reflect = repeatedly $ (listen >>= send) <|> (recv >>= yield)

-- | Transform a HalfDuplex that never sends or receives into a process.
toSimplex :: Monad m => HalfDuplexT m a Void Void b -> ProcessT m a b
toSimplex m = MachineT $ runMachineT m >>= \case
  Stop                   -> return Stop
  Yield o f              -> return $ Yield o (toSimplex f)
  Await f Listen ff      -> return $ Await (toSimplex . f) Refl (toSimplex ff)
  Await f WeakListen ff  -> return $ Await (toSimplex . f) Refl (toSimplex ff)
  _                      -> error "Data.Machine.Duplex.toSimplex: impossible send/recv"

-- | Transform a HalfDuplex that never yields or listens into a process.
toReversedSimplex :: Monad m => HalfDuplexT m Void a' b' Void -> ProcessT m b' a'
toReversedSimplex m = MachineT $ runMachineT m >>= \case
  Stop                 -> return Stop
  Await f (Send i) _   -> return $ Yield i (toReversedSimplex $ f ())
  Await f Recv ff      -> return $ Await (toReversedSimplex . f) Refl (toReversedSimplex ff)
  Await f WeakRecv ff -> return $ Await (toReversedSimplex . f) Refl (toReversedSimplex ff)
  _                    -> error "Data.Machine.Duplex.toReversedSimplex: impossible yield/listen"

-- | Fit a process on the sending channel on a HalfDuplex.
--
-- Note: the process is run first.
--
-- >>> runT $ reflect >~> auto (+1) ~< (repeatedly $ send 0 >> listen >>= yield) ~> taking 3
-- [1,1,1]
(~<) :: Monad m => ProcessT m a' c -> HalfDuplexT m a a' b' b -> HalfDuplexT m a c b' b
mp ~< ma = MachineT $ runMachineT mp >>= \v -> case v of
  Stop            -> return Stop
  Yield o f       -> return $ Await (\() -> f ~< ma) (Send o) (f ~< ma)
  Await f Refl ff -> runMachineT ma >>= \case
    Stop                     -> runMachineT $ ff ~< stopped
    Yield o f'               -> return $ Yield o (encased v ~< f')
    Await f' (Send o) _      -> runMachineT $ f o ~< f' ()
    Await f' Recv ff'        -> return $ Await (\b -> encased v ~< f' b) Recv (encased v ~< ff')
    Await f' WeakRecv ff'    -> return $ Await (\b -> encased v ~< f' b) WeakRecv (encased v ~< ff')
    Await f' Listen ff'      -> return $ Await (\b -> encased v ~< f' b) Listen (encased v ~< ff')
    Await f' WeakListen ff'  -> return $ Await (\b -> encased v ~< f' b) WeakListen (encased v ~< ff')


-- | Fit a process on the sending channel on a HalfDuplex. Flipped '~<'.
(>~) :: Monad m =>  HalfDuplexT m a a' b' b -> ProcessT m a' c -> HalfDuplexT m a c b' b
(>~) = flip (~<)

-- | Turn two processes into a HalfDuplexT.
--
-- >>> runT $ reflect >~> auto show ~<~> auto read >~> (repeatedly $ send (5::Int) >> listen >>= \(x::Float) -> yield x) ~> taking 3
-- [5.0,5.0,5.0]
(~<~>) :: Monad m => ProcessT m b' a' -> ProcessT m a b -> HalfDuplexT m a a' b' b
a ~<~> b = a ~< duplexEcho ~> b
