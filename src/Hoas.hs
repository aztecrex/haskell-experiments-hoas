{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Hoas where

import Control.Monad.IO.Class
import Data.IORef

data IntT
data a :-> b
infixr 5 :->


class EDSL exp where
  lam :: (exp a -> exp b) -> exp (a :-> b)
  app :: exp (a :-> b) -> exp a -> exp b

  int :: Int -> exp IntT
  add :: exp IntT -> exp IntT -> exp IntT
  sub :: exp IntT -> exp IntT -> exp IntT

let_ :: EDSL exp => exp a -> (exp a -> exp b) -> exp b
let_ x y = (lam y) `app` x

t2 :: EDSL exp => exp IntT
t2 = (lam $ \z -> lam $ \x -> let_ (x `add` x)
                                 $ \y -> y `add` y)
      `app` (int 100 `sub` int 10)
      `app` (int 5 `add` int 5)

type family Sem (m :: * -> *) a :: *
type instance Sem m IntT = Int
type instance Sem m (a :-> b) = m (Sem m a) -> m (Sem m b)

newtype S l m a = S { unS :: m (Sem m a) }

data Name
instance MonadIO m => EDSL (S Name m) where
  int = S . return
  add x y = S $ do a <- unS x
                   b <- unS y
                   liftIO $ putStrLn "Adding"
                   return (a + b)
  sub x y = S $ do a <- unS x
                   b <- unS y
                   liftIO $ putStrLn "Subtracting"
                   return (a - b)
  lam f = S . return $ (unS . f . S)
  app x y = S $ unS x >>= ($ (unS y))

runName :: S Name m a -> m (Sem m a)
runName x = unS x

t2SN = runName t2 >>= print

data Value
instance MonadIO m => EDSL (S Value m) where
  int = S . return
  add x y = S $ do a <- unS x
                   b <- unS y
                   liftIO $ putStrLn "Adding"
                   return (a + b)
  sub x y = S $ do a <- unS x
                   b <- unS y
                   liftIO $ putStrLn "Subtracting"
                   return (a - b)
  lam f = S . return $ (\x -> x >>= unS . f . S . return)
  app x y = S $ unS x >>= ($ (unS y))

runValue :: S Value m a -> m (Sem m a)
runValue x = unS x

t2SV = runValue t2 >>= print

share :: MonadIO m => m a -> m (m a)
share m = do
          r <- liftIO $ newIORef (False,m)
          let ac = do
                   (f,m) <- liftIO $ readIORef r
                   if f then m
                      else do
                           v <- m
                           liftIO $ writeIORef r (True,return v)
                           return v
          return ac

data Need
instance MonadIO m => EDSL (S Need m) where
  int = S . return
  add x y = S $ do a <- unS x
                   b <- unS y
                   liftIO $ putStrLn "Adding"
                   return (a + b)
  sub x y = S $ do a <- unS x
                   b <- unS y
                   liftIO $ putStrLn "Subtracting"
                   return (a - b)
  lam f = S . return $ (\x -> share x >>= unS . f . S)
  app x y = S $ unS x >>= ($ (unS y))

runNeed :: S Need m a -> m (Sem m a)
runNeed x = unS x

t2SND = runNeed t2 >>= print
