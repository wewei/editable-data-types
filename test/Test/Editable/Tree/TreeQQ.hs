module Test.Editable.Tree.TreeQQ where
import Data.Tree (Tree (Node))
import Text.Parsec ( parse, ParseError, char, spaces, option, runParser, Parsec, getPosition, setSourceName, setSourceColumn, setSourceLine, setPosition )
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import Numeric (showInt)
import Data.List (intersperse)
import GHC.Show (showCommaSpace)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote ( QuasiQuoter(QuasiQuoter, quoteExp, quotePat, quoteDec, quoteType), dataToExpQ, dataToPatQ )

tree :: QuasiQuoter
tree = QuasiQuoter
    { quoteExp = quoteTreeExp
    , quotePat = quoteTreePat
    , quoteDec = undefined
    , quoteType = undefined }

quoteTreeExp :: String -> TH.ExpQ
quoteTreeExp s = do
    loc <- TH.location
    let pos = ( TH.loc_filename loc
              , fst (TH.loc_start loc)
              , snd (TH.loc_start loc))
    tr  <- parseTree pos s
    dataToExpQ (const Nothing) tr


quoteTreePat :: String -> TH.PatQ
quoteTreePat s = do
    loc <- TH.location
    let pos = ( TH.loc_filename loc
              , fst (TH.loc_start loc)
              , snd (TH.loc_start loc))
    tr <- parseTree pos s
    dataToPatQ (const Nothing) tr

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right b) = Just b
rightToMaybe _         = Nothing

parseTree :: (Monad m, MonadFail m) => (String, Int, Int) -> String -> m (Tree Int)
parseTree (file, line, col) s =
    case runParser p () "" s of
        Left err -> fail $ show err
        Right tr -> return tr
    where
        p = do
            pos <- getPosition
            setPosition $
                flip setSourceName file $
                flip setSourceLine line $
                setSourceColumn pos col
            pTree
        pTree = withLeadingSpaces . withTailingSpaces . parens $ do
            n  <- fromInteger <$> withTailingSpaces integer
            ch <- option [] $ do
                withTailingSpaces . char $ ','
                commaSep1 pTree
            return $ Node n ch

        withLeadingSpaces :: Parsec [Char] u a -> Parsec [Char] u a
        withLeadingSpaces = (>>) spaces

        withTailingSpaces :: Parsec [Char] u a -> Parsec [Char] u a
        withTailingSpaces p = do { t <- p; spaces; return t }
 
        integer = T.integer L.haskell
        commaSep1 = T.commaSep1 L.haskell
        parens = T.parens L.haskell
