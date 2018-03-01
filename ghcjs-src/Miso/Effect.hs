-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Effect
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Effect (
  module Miso.Effect.Storage
, module Miso.Effect.XHR
, module Miso.Effect.DOM
, Effect (..), Sub, Sink
, noEff
, (<#)
, (#>)
) where

import Miso.Effect.Storage
import Miso.Effect.XHR
import Miso.Effect.DOM

-- | An effect represents the results of an update action.
--
-- It consists of the updated model and a list of IO computations. Each IO
-- computation is run in a new thread so there is no risk of accidentally
-- blocking the application. The IO computation is given a 'Sink' callback which
-- can be used to dispatch actions which are fed back to the @update@ function
data Effect action model
  = Effect model [Sub action model]

-- | Type synonym for constructing event subscriptions.
--
-- The first argument passed to a subscription provides a way to
-- access the current value of the model (without blocking). The 'Sink'
-- callback is used to dispatch actions which are then fed back to the
-- @update@ function.
type Sub action model = IO model -> Sink action -> IO ()

-- | Function to pass actions into the 'update' function.
--
-- It can be called as many times as desired.
type Sink action = action -> IO ()

-- | Smart constructor for an 'Effect' with no actions.
noEff :: model -> Effect action model
noEff m = Effect m []

-- | Smart constructor for an 'Effect' with exactly one action.
(<#) :: model -> IO action -> Effect action model
(<#) m a = Effect m [\_getModel sink -> a >>= sink]

-- | `Effect` smart constructor, flipped
(#>) :: IO action -> model -> Effect action model
(#>) = flip (<#)
