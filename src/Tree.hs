module Tree where

import Data.List (isPrefixOf)
import Data.Text (Text, pack)
import System.Posix (FileOffset, fileSize, getFileStatus, isDirectory, isSymbolicLink)
import System.Posix.Files (FileStatus)

{-
cabal repl
:l TreeTr

./manage/reload_repl_on_change
-}

data Entry = Entry
    { name :: !String
    , isDir :: !Bool
    , isHidden :: !Bool
    , isLink :: !Bool
    , size :: !FileOffset
    , parentDir :: !String
    , children :: ![Entry]
    }
    deriving (Show)

toText2 :: [FilePath] -> Text
toText2 = foldMap pack

toText :: FilePath -> Text
toText = pack

-- toName :: FilePath -> String
-- toName path =
--     encodeString path

-- toEntry :: FilePath -> IO Entry
toEntry :: FilePath -> FileStatus -> Entry
toEntry path status =
    let basename_ = encodeString $ basename (decodeString path)
        parentDir_ = encodeString $ dirname (decodeString path)
        isHidden_ = isPrefixOf "." basename_
     in Entry
            { name = path
            , isDir = isDirectory status
            , isHidden = isHidden_
            , isLink = isSymbolicLink status
            , size = fileSize status
            , parentDir = parentDir_
            , children = []
            }

main :: IO ()
main = do
    -- paths <- listDirectory "/home/benjamin/code/explore/elm/turtle/"
    let path = "/home/benjamin/code/explore/elm/turtle/elm.json"
    status <- getFileStatus (encodeString (decodeString path))
    print (toEntry path status)

hello :: [Char]
hello = "world3"