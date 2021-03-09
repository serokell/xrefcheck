{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.IgnoreAnnotationsSpec where

import qualified Data.ByteString.Lazy as BSL
import Test.Hspec (Spec, describe, it, shouldBe)

import Xrefcheck.Core
import Xrefcheck.Scanners.Markdown

spec :: Spec
spec = do
    describe "Parsing failures" $ do
        it "Check if parsing incorrect markdowns produce exceptions" $ do
            areIncorrect <- mapM isIncorrectMD failPaths
            or areIncorrect `shouldBe` True
    describe "\"ignore link\" mode" $ do
        it "Check \"ignore link\" performance" $ do
            fi <- getFI "tests/markdowns/with-annotations/ignore_link.md"
            (getRefs fi) `shouldBe` ["team", "team", "team", "hire-us", "how-we-work", "privacy"]
    describe "\"ignore paragraph\" mode" $ do
        it "Check \"ignore paragraph\" performance" $ do
            fi <- getFI "tests/markdowns/with-annotations/ignore_paragraph.md"
            (getRefs fi) `shouldBe` ["blog", "contacts"]
    describe "\"ignore file\" mode" $ do
        it "Check \"ignore file\" performance" $ do
            fi <- getFI "tests/markdowns/with-annotations/ignore_file.md"
            (getRefs fi) `shouldBe` []
    where
        failPaths :: [FilePath]
        failPaths =
            [ "tests/markdowns/with-annotations/no_link.md"
            , "tests/markdowns/with-annotations/no_paragraph.md"
            , "tests/markdowns/with-annotations/unexpected_ignore_file.md"
            , "tests/markdowns/with-annotations/unrecognised_option.md"
            ]
        
        parse :: FilePath -> IO (Either Text FileInfo)
        parse path = parseFileInfo . decodeUtf8 <$> BSL.readFile path

        getFI :: FilePath -> IO FileInfo
        getFI path =
            let errOrFI = parse path
            in either error id <$> errOrFI
        
        getRefs :: FileInfo -> [Text]
        getRefs fi = map rName $ fi ^. fiReferences 

        isIncorrectMD :: FilePath -> IO Bool
        isIncorrectMD path = do
            errOrInfo <- parse path
            return $ isLeft errOrInfo
