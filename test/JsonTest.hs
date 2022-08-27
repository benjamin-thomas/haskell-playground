{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonTest where

import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON (toJSON), Value (Null), defaultOptions, eitherDecode, encode, fromJSON, genericToEncoding, object, withObject, (.:))
import Data.Aeson.Types (FromJSON (parseJSON), Result (..), ToJSON (toEncoding))
import GHC.Generics (Generic)
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Text.RawString.QQ (r)

{-
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.ByteString.Lazy.Internal as BSL (ByteString)
toLBS :: String -> BSL.ByteString
toLBS raw = toLazyByteString (stringUtf8 raw)
-}

myObject :: Value
myObject =
    object
        [ "id" .= (123 :: Int)
        , "name" .= ("Benjamin" :: String)
        , "hobbies" .= (["Programming", "Piano"] :: [String])
        , "country" .= Null
        ]

-- START: Person example definition from the docs...
data Person = Person
    { name :: String
    , age :: Int
    }
    deriving (Generic, Show, Eq)

instance ToJSON Person where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Person

-- No need to provide a parseJSON implementation.
-- END: Person example definition from the docs...

-- START: Provide an explicit encode/decode implementation
data User = User
    { ident :: Int
    , firstName :: String
    , occupations :: [String]
    }
    deriving (Show, Eq)

instance ToJSON User where
    toJSON (User uId fn occs) =
        object
            [ "id" .= uId
            , "name" .= fn
            , "hobbies" .= occs
            ]

{- ORMOLU_DISABLE -}
instance FromJSON User where
    parseJSON = withObject "User" $ \v ->
        User <$> v .: "id"
             <*> v .: "name"
             <*> v .: "hobbies"
{- ORMOLU_ENABLE -}
-- END: Provide an explicit encode/decode implementation

spec :: Spec
spec =
    describe "JSON parsing" $ do
        it "can encode a maybe type" $ do
            encode (Just "hello" :: Maybe String) `shouldBe` "\"hello\""
            encode (Nothing :: Maybe String) `shouldBe` "null"
        it "can encode arbitrary data" $ do
            let data_ = (1, "two", 3.3) :: (Int, String, Float)
            encode data_ `shouldBe` [r|[1,"two",3.3]|]
        it "can encode a manually constructed value object " $
            encode myObject `shouldBe` [r|{"country":null,"hobbies":["Programming","Piano"],"id":123,"name":"Benjamin"}|]
        it "can decode a value object" $ do
            let j = toJSON (1 :: Int)
            fromJSON j `shouldBe` Success (1 :: Int)
        context "with a a custom data type" $ do
            it "can encode/decode by deriving a generic encoder/decoder" $ do
                encode
                    Person{name = "Benjamin", age = 41}
                    `shouldBe` [r|{"name":"Benjamin","age":41}|]

                eitherDecode
                    [r|{"name":"Benjamin","age":41}|]
                    `shouldBe` Right Person{name = "Benjamin", age = 41}

                (eitherDecode [r|{"firstName":"Benjamin","age":41}|] :: Either String Person)
                    `shouldBe` Left "Error in $: parsing JsonTest.Person(Person) failed, key \"name\" not found"
            it "can provide a specialized encoder/decoder" $ do
                let encoded = encode $ User{ident = 1, firstName = "Benjamin", occupations = ["Programming", "Piano"]}
                encoded `shouldBe` [r|{"hobbies":["Programming","Piano"],"id":1,"name":"Benjamin"}|]
                eitherDecode
                    [r|{"id":1,"name":"Benjamin","hobbies":["Programming","Piano"]}|]
                    `shouldBe` Right (User 1 "Benjamin" ["Programming", "Piano"])

                eitherDecode [r|{"ident":1,"name":"Benjamin","hobbies":["Programming","Piano"]}|]
                    `shouldBe` (Left "Error in $: key \"id\" not found" :: Either String User)