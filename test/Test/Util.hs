{-# LANGUAGE TupleSections #-}
module Test.Util where

iterM :: Monad m => m Int -> a -> (a -> m (a, b)) -> m [b]
iterM n x f = n >>= \n' -> iterM_ (return []) n' x f where
    iterM_ :: Monad m => m [b] -> Int -> a -> (a -> m (a, b)) -> m [b]
    iterM_ ys 0 _ _ = reverse <$> ys
    iterM_ ys n x f = do
        (x', y) <- f x
        ys'     <- ys
        iterM_ (return (y:ys')) (n - 1) x' f

replM :: Monad m => m Int -> m a -> m[a]
replM mn mx = iterM mn () (\a -> (a, ) <$> mx)