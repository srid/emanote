{-# LANGUAGE DeriveAnyClass #-}

module Emanote.Source.Util where

-- | Monadic version of `chain`
chainM :: Monad m => [b] -> (b -> m (a -> a)) -> m (a -> a)
chainM xs =
  fmap chain . forM xs
  where
    -- Apply the list of actions in the given order to an initial argument.
    --
    -- chain [f1, f2, ...] x = ... (f2 (f1 x))
    chain :: [a -> a] -> a -> a
    chain = flip (foldl' $ flip ($))

newtype BadInput = BadInput Text
  deriving (Show, Exception)
