module TimeTest where

import Data.Time (
    UTCTime (UTCTime),
    defaultTimeLocale,
    diffUTCTime,
    getCurrentTime,
    iso8601DateFormat,
    parseTimeM,
    zonedTimeToUTC,
 )
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Lens (
    ZonedTime,
    day,
    getL,
    hours,
    minutes,
    modL,
    month,
    seconds,
    setL,
    year,
 )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "Working with time" $ do
        describe "time lib" $ do
            let fmt = iso8601DateFormat (Just "%H:%M:%S%Q%z")
            let parseToUTC = parseTimeM True defaultTimeLocale fmt :: String -> Maybe UTCTime
            let parseToZoned = parseTimeM True defaultTimeLocale fmt :: String -> Maybe ZonedTime

            let nthDayOfTheYear = 233
            let secondsFromMidnight = 43200

            -- The constructor API is a little quirky
            it "parses timestamps to UTC" $ do
                let day_ = fromOrdinalDate 2022 nthDayOfTheYear
                let expectedTime = UTCTime day_ secondsFromMidnight
                parseToUTC "2022-08-21T14:00:00+0200" `shouldBe` Just expectedTime
                parseToUTC "2022-08-21T14:00:00+0200" `shouldBe` Just (posixSecondsToUTCTime 1661083200)
                parseToUTC "2022-08-21T13:00:00+0100" `shouldBe` Just (posixSecondsToUTCTime 1661083200)
                parseToUTC "2022-08-21T12:00:00+0000" `shouldBe` Just (posixSecondsToUTCTime 1661083200)

            it "parses timestamps to zoned time" $ do
                let parsed = parseToZoned "2022-08-21T14:00:00+0200" -- no Eq
                let toUtc = fmap zonedTimeToUTC parsed
                toUtc `shouldBe` Just (posixSecondsToUTCTime 1661083200)
            it "can only compare time in UTC" $ do
                now <- getCurrentTime -- I can also get the current time with: `getZonedTime`
                let expected =
                        [ posixSecondsToUTCTime 0 < posixSecondsToUTCTime 1
                        , posixSecondsToUTCTime 1 > posixSecondsToUTCTime 0
                        , posixSecondsToUTCTime 0 == posixSecondsToUTCTime 0
                        , posixSecondsToUTCTime 0 < posixSecondsToUTCTime 1661083200
                        , posixSecondsToUTCTime 0 < now
                        ]
                expected `shouldBe` [True, True, True, True, True]
            it "can compute a the difference of time between 2 timestamps" $ do
                let expected =
                        [ show (diffUTCTime (posixSecondsToUTCTime 1) (posixSecondsToUTCTime 0))
                        , show (diffUTCTime (posixSecondsToUTCTime 0) (posixSecondsToUTCTime 1))
                        ]
                expected `shouldBe` ["1s", "-1s"]
            it "can apply math operations from a time diff" $ do
                let diff = diffUTCTime (posixSecondsToUTCTime 1) (posixSecondsToUTCTime 0)
                let expected =
                        [ show $ diff * 10
                        , show $ diff / 2
                        ]
                expected `shouldBe` ["10s", "0.5s"]
        describe "time-lens lib" $ do
            it "can extract the components of a time stamp" $ do
                let extract t =
                        ( getL year t
                        , getL month t
                        , getL day t
                        , getL hours t
                        , getL minutes t
                        , getL seconds t
                        )
                extract (posixSecondsToUTCTime 1661083200) `shouldBe` (2022, 8, 21, 12, 0, 0)

            it "can alter timestamps" $ do
                let ts = posixSecondsToUTCTime 1661083200
                show ts `shouldBe` "2022-08-21 12:00:00 UTC"
                show (setL year 2000 . modL day (subtract 1) $ ts) `shouldBe` "2000-08-20 12:00:00 UTC"
            it "will alter using a non-crashing (but sometimes surprising) strategy" $ do
                let ts = posixSecondsToUTCTime 0
                show (setL month 2 . setL day 28 $ ts) `shouldBe` "1970-02-28 00:00:00 UTC"
                show (setL month 2 . setL day 29 $ ts) `shouldBe` "1970-03-01 00:00:00 UTC"
                show (setL month 2 . setL day 30 $ ts) `shouldBe` "1970-03-02 00:00:00 UTC"
                show (setL month 2 . setL day 31 $ ts) `shouldBe` "1970-03-03 00:00:00 UTC"
                show (setL month 2 . setL day 32 $ ts) `shouldBe` "1970-02-01 00:00:00 UTC" -- !!
