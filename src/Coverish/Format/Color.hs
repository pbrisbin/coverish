{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- Construct and render terminal-escaped @'Text'@ values.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Data.Text.Escaped
-- >
-- > import qualified Data.Text.IO as T
-- >
-- > main :: IO ()
-- > main = T.putStrLn
-- >     $ render
-- >     $ "This text is " <> FG Red <> "Red" <> Reset <> "."
--
module Coverish.Format.Color
    ( Color(..)
    , Escaped(..)
    , renderIfTerminal
    , render
    , plain
    , visibleLength
    ) where

import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Supported colors
data Color
    = Default
    | Custom Int
    | Black
    | Blue
    | Cyan
    | DarkGray
    | Green
    | LightBlue
    | LightCyan
    | LightGray
    | LightGreen
    | LightMagenta
    | LightRed
    | LightYellow
    | Magenta
    | Red
    | White
    | Yellow

-- | Bits of escaped text
data Escaped
    = Plain Text
    | Reset
    | Bold
    | Dim
    | Underlined
    | Blink
    | Reverse
    | Hidden
    | FG Color
    | BG Color
    | Many [Escaped]

instance IsString Escaped where
    fromString = Plain . T.pack

instance Monoid Escaped where
    mempty = Plain ""
    mappend (Many a) (Many b) = Many $ a ++ b
    mappend (Many a) b = Many $ a ++ [b]
    mappend a (Many b) = Many $ a:b
    mappend a b = Many [a, b]

-- | Use @'render'@ if @'stdout'@ is a terminal device, otherwise @'plain'@
renderIfTerminal :: Escaped -> IO Text
renderIfTerminal e = do
    isTerminal <- undefined

    return $ (if isTerminal then render else plain) e

-- | Render an @'Escaped'@ to actually-escaped @'Text'@
render :: Escaped -> Text
render (Plain t) = t
render (Many es) = T.concat $ map render es
render Reset = "\ESC[0m"
render Bold = "\ESC[1m"
render Dim = "\ESC[2m"
render Underlined = "\ESC[3m"
render Blink = "\ESC[5m"
render Reverse = "\ESC[7m"
render Hidden = "\ESC[8m"
render (FG c) = "\ESC[" <> fgColorCode c <> "m"
render (BG c) = "\ESC[" <> bgColorCode c <> "m"

-- | Render only the @'Text'@ parts
plain :: Escaped -> Text
plain (Plain t) = t
plain (Many es) = T.concat $ map plain es
plain _ = ""

-- | Calculate the /visible/ length of an @'Escaped'@
visibleLength :: Escaped -> Int
visibleLength (Plain t) = T.length t
visibleLength (Many es) = sum $ map visibleLength es
visibleLength _ = 0

fgColorCode :: Color -> Text
fgColorCode Default = "39"
fgColorCode (Custom n) = "38;5;" <> T.pack (show n)
fgColorCode Black = "30"
fgColorCode Blue = "34"
fgColorCode Cyan = "36"
fgColorCode DarkGray = "90"
fgColorCode Green = "32"
fgColorCode LightBlue = "94"
fgColorCode LightCyan = "96"
fgColorCode LightGray = "37"
fgColorCode LightGreen = "92"
fgColorCode LightMagenta = "95"
fgColorCode LightRed = "91"
fgColorCode LightYellow = "93"
fgColorCode Magenta = "35"
fgColorCode Red = "31"
fgColorCode White = "97"
fgColorCode Yellow = "33"

bgColorCode :: Color -> Text
bgColorCode Default = "49"
bgColorCode (Custom n) = "48;5;" <> T.pack (show n)
bgColorCode Black = "40"
bgColorCode Blue = "44"
bgColorCode Cyan = "46"
bgColorCode DarkGray = "100"
bgColorCode Green = "42"
bgColorCode LightBlue = "104"
bgColorCode LightCyan = "106"
bgColorCode LightGray = "100"
bgColorCode LightGreen = "102"
bgColorCode LightMagenta = "105"
bgColorCode LightRed = "101"
bgColorCode LightYellow = "103"
bgColorCode Magenta = "45"
bgColorCode Red = "41"
bgColorCode White = "107"
bgColorCode Yellow = "103"
