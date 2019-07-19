{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module ConnectedComponent
    ( ComponentId
    , connectedComponents
    ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.ST
import qualified Data.Vector.Indexed as VI
import qualified Data.Vector.Indexed.Mutable as VIM
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Deriving

import Dijkstra
import DenseMapping

newtype ComponentId = ComponentId Int
                    deriving (Ord, Eq, Show)

$(derivingUnbox "Distance"
     [t| ComponentId -> Int |]
     [| \(ComponentId n) -> n |]
     [| ComponentId |]
 )

connectedComponents :: DenseMapping n
                    -> [(DenseId n, DenseId n, Distance e)]
                    -> VI.Vector VU.Vector (DenseId n) ComponentId
connectedComponents mapping xs = VI.create $ do
    accum <- VIM.replicate (denseRange mapping) noComponent
    evalStateT (mapM_ (doSource accum) xs) (ComponentId 1)
    return accum
  where
    noComponent = ComponentId 0

    nextComponent :: Monad m => StateT ComponentId m ComponentId
    nextComponent = do
        ComponentId n <- get
        put $ ComponentId (succ n)
        return (ComponentId n)

    readComponent :: VIM.MVector VU.MVector s (DenseId n) ComponentId
                  -> DenseId n
                  -> StateT ComponentId (ST s) (Maybe ComponentId)
    readComponent accum n = do
        c <- lift $ VIM.read accum n
        if c == noComponent
           then return Nothing
           else return $ Just c

    doSource :: VIM.MVector VU.MVector s (DenseId n) ComponentId
             -> (DenseId n, DenseId n, Distance e)
             -> StateT ComponentId (ST s) ()
    doSource accum (src, dst, Infinite) = do
        c <- nextComponent
        lift $ VIM.write accum src c

    doSource accum (src, dst, Finite _) = do
        mcSrc <- readComponent accum src
        mcDst <- readComponent accum dst
        case (mcSrc, mcDst) of
          (Nothing, Nothing) -> do
              c <- nextComponent
              lift $ VIM.write accum src c
              lift $ VIM.write accum dst c
          (Just cSrc, Nothing) -> do
              lift $ VIM.write accum dst cSrc
          (Nothing, Just cDst) -> do
              lift $ VIM.write accum src cDst
          (Just cSrc, Just cDst)
            | cSrc > cDst -> do
              lift $ VIM.write accum dst cSrc
            | otherwise -> do
              lift $ VIM.write accum src cDst
