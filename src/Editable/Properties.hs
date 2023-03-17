{-# LANGUAGE MonoLocalBinds #-}
module Editable.Properties where
import Editable.Core ( Editable((~)), Rebasable (rebase, (+>)) )
import Control.Monad ( foldM, forM_ )
import Control.Monad.Writer.Lazy ( Writer, MonadWriter (tell) )
import Text.Printf (printf)

testCP1 :: (Eq d, Show d, Show o, Rebasable o, Editable d o) =>
           o -> o -> d -> Writer ShowS Bool
testCP1 o1 o2 d = do
    let (o1', o2') = rebase o1 o2
        d1         = Just d ~ o1 ~ o2'
        d2         = Just d ~ o2 ~ o1'

    tell $ showString "Base:\n"
    tell $ showChar '\t' . shows d . showChar '\n'

    tell $ showString "Editing Path #1:\n"
    forM_ [o1, o2'] $ \o -> tell $ showChar '\t' . shows o . showChar '\n'
    tell $ showString "Result #1:\n"
    tell $ showChar '\t' . shows d1 . showChar '\n'

    tell $ showString "Editing Path #2:\n"
    forM_ [o2, o1'] $ \o -> tell $ showChar '\t' . shows o . showChar '\n'
    tell $ showString "Result #2:\n"
    tell $ showChar '\t' . shows d2 . showChar '\n'

    return (d1 == d2)

testCP2 :: (Eq o, Show o, Rebasable o) => o -> o -> o -> Writer ShowS Bool
testCP2 o1 o2 t = do
    let (o1', o2') = rebase o1 o2
        t1         = t +> o1 +> o2'
        t2         = t +> o2 +> o1'

    tell $ showString "Base:\n"
    tell $ showChar '\t' . shows t . showChar '\n'

    tell $ showString "Editing Path #1:\n"
    forM_ [o1, o2'] $ \o -> tell $ showChar '\t' . shows o . showChar '\n'
    tell $ showString "Result #1:\n"
    tell $ showChar '\t' . shows t1 . showChar '\n'

    tell $ showString "Editing Path #2:\n"
    forM_ [o2, o1'] $ \o -> tell $ showChar '\t' . shows o . showChar '\n'
    tell $ showString "Result #2:\n"
    tell $ showChar '\t' . shows t2 . showChar '\n'

    return (t1 == t2)