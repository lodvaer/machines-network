{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      :  Data.Machine.AltWye
-- Copyright   :  Lodvær 2015
-- License     :  BSD3
--
-- Maintainer  :  Lodvær <lodvaer@gmail.com>
-- Stability   :  provisional
-- Portability :  unknown
--
-- Alternative combinators for 'Wye' for monads that implement 'Alternative'.
module Data.Machine.AltWye
    ( -- * Wyes over instances of 'Alternative'.
      Wye, WyeT
    , Y(..)
    , altWye
    , altAddX, altAddY
    , altCapX, altCapY
    ) where

import Control.Applicative
import Data.Machine

-- | Alternative implementation of 'wye' for monads that can fail.
--
-- Different in 'Z' in that if the left fails it is kept unchanged as the left
-- input while the right is attempted.
--
-- Useful e.g. for implementing select behaviour for machines over STM.
altWye :: (Monad m, Alternative m)
       => ProcessT m a a'
       -> ProcessT m b b'
       -> WyeT m a' b' c
       -> WyeT m a b c
altWye ma mb m = MachineT $ runMachineT m >>= \v -> case v of
    Yield o k ->
        return $ Yield o (altWye ma mb k)
    Stop ->
        return Stop
    Await f X ff -> runMachineT ma >>= \case
        Yield a k ->
            runMachineT . altWye k mb $ f a
        Stop ->
            runMachineT $ wye stopped mb ff
        Await g Refl fg ->
            return . Await (\a -> altWye (g a) mb $ encased v) X
                   . altWye fg mb $ encased v
    Await f Y ff -> runMachineT mb >>= \case
        Yield b k ->
            runMachineT . altWye ma k $ f b
        Stop ->
            runMachineT $ wye ma stopped ff
        Await g Refl fg ->
            return . Await (\b -> altWye ma (g b) $ encased v) Y
                   . altWye ma fg $ encased v
    Await f Z ff -> optional (runMachineT ma) >>= \case
        Nothing ->
            runMachineT mb >>= \case
                Yield b k ->
                    runMachineT . altWye ma k $ f $ Right b
                Stop -> runMachineT ma >>= \x -> case x of
                    Yield b k ->
                        runMachineT . wye k stopped . f $ Left b
                    Stop ->
                        runMachineT $ wye stopped stopped ff
                    Await g Refl fg ->
                        return . Await (\b -> wye (g b) stopped $ encased v) X
                               . wye fg stopped $ encased v
                Await g Refl fg ->
                    return . Await (\b -> altWye ma (g b) $ encased v) Y
                           . altWye ma fg $ encased v
        Just (Yield a k) ->
            runMachineT . altWye k mb . f $ Left a
        Just Stop ->
            runMachineT mb >>= \case
                Yield b k ->
                    runMachineT . wye stopped k . f $ Right b
                Stop ->
                    runMachineT $ wye stopped stopped ff
                Await g Refl fg ->
                    return . Await (\b -> wye stopped (g b) $ encased v) Y
                           . wye stopped fg $ encased v
        Just u@(Await g Refl fg) ->
            runMachineT mb >>= \w -> case w of
                Yield b k ->
                    runMachineT . altWye (encased u) k . f $ Right b
                Stop ->
                    return . Await (\a -> wye (g a) stopped $ encased v) X
                           . wye fg stopped $ encased v
                Await h Refl fh ->
                    return . Await (\case
                                      Left a ->
                                          altWye (g a) (encased w) $
                                              encased v
                                      Right b ->
                                          altWye (encased u) (h b) $
                                              encased v)
                                   Z
                           . altWye fg fh $ encased v

-- Copy/paste job with slight alterations:

-- | Precompose a pipe onto the left input of a wye.
altAddX :: (Alternative m, Monad m)
        => ProcessT m a b -> WyeT m b c d -> WyeT m a c d
altAddX p = altWye p echo
{-# INLINE altAddX #-}

-- | Precompose a pipe onto the right input of a tee.
altAddY :: (Alternative m, Monad m)
        => ProcessT m b c -> WyeT m a c d -> WyeT m a b d
altAddY = altWye echo
{-# INLINE altAddY #-}

-- | Tie off one input of a tee by connecting it to a known source.
altCapX :: (Alternative m, Monad m)
        => SourceT m a -> WyeT m a b c -> ProcessT m b c
altCapX s t = process (capped Right) (altAddX s t)
{-# INLINE altCapX #-}

-- | Tie off one input of a tee by connecting it to a known source.
altCapY :: (Alternative m, Monad m)
        => SourceT m b -> WyeT m a b c -> ProcessT m a c
altCapY s t = process (capped Left) (altAddY s t)
{-# INLINE altCapY #-}

-- | Natural transformation used by 'altCapX' and 'altCapY'
capped :: (a -> Either a a) -> Y a a b -> a -> b
capped _ X = id
capped _ Y = id
capped f Z = f
{-# INLINE capped #-}
