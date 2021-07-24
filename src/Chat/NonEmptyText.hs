module Chat.NonEmptyText (
  NonEmptyText,
  fromText,
  toText,
) where

import Data.Text (Text)
import qualified Data.Text as Text

newtype NonEmptyText = NonEmptyText {fromNonEmptyText :: Text}
  deriving (Show, Eq, Ord, Semigroup) via Text

fromText :: Text -> Maybe NonEmptyText
fromText (Text.strip -> text)
  | text == Text.empty = Nothing
  | otherwise = Just $ NonEmptyText text

toText :: NonEmptyText -> Text
toText = fromNonEmptyText
