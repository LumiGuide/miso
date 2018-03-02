{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Window
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.Window where

import GHCJS.Foreign.Callback
import GHCJS.Marshal

import JavaScript.Object
import JavaScript.Object.Internal
import Miso.FFI
import Miso.Types

-- | Captures window coordinates changes as they occur and writes them to
-- an event sink
windowSub :: ((Int, Int) -> action) -> Transition action model ()
windowSub f = scheduleIOWithSink $ \sink -> do
  sink . f =<< (,) <$> windowInnerHeight <*> windowInnerWidth
  windowAddEventListener "resize" =<< do
    asyncCallback1 $ \windowEvent -> do
      target <- getProp "target" (Object windowEvent)
      Just w <- fromJSVal =<< getProp "innerWidth" (Object target)
      Just h <- fromJSVal =<< getProp "innerHeight" (Object target)
      sink $ f (h, w)

