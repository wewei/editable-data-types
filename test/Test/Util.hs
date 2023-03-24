{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Test.Util where
import Editable.Core ( Editable (apply) )

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

generateOps :: (Editable d o, Monad m) => m Int -> (d -> m o) -> d -> m [o]
generateOps mn generateOp d = do
    iterM mn (Just d) $ \case
        Nothing -> do
            o <- generateOp d
            return (apply o d, o)
        Just e -> do
            r <- generateOp e
            return (apply r e, r)

