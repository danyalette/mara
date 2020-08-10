{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}

module Stringable where 

mkStringList :: (Stringable a) => [a] -> String
mkStringList [] = ""
mkStringList [x] = mkString x 
mkStringList (x:xs) = (mkString x) ++ ", " ++ (mkStringList xs)

class Stringable a where 
  mkString :: a -> String
instance Stringable Char where 
  mkString c = [c]
instance Stringable Int where 
  mkString i = show i
instance Stringable [Int] where 
  mkString l = "(" ++ (mkStringList l) ++ ")"
instance Stringable [String] where 
  mkString l = "(" ++ (mkStringList l) ++ ")"
instance Stringable String where 
  mkString s = s
instance (Stringable a, Stringable b) => Stringable (a, b) where 
  mkString (x, y) = "(" ++ mkString x ++ ", " ++ mkString y ++ ")"