module Types where

-- command data type
data Command
  = Rect Int Int
  | RowRotate Int Int
  | ColumnRotate Int Int
  deriving (Show)

type Grid = [[Char]]
