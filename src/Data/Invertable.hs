{-# LANGUAGE InstanceSigs #-}

module Data.Invertable where

class Invertable o where
    invert :: o -> o

instance (Invertable o) => Invertable [o] where
    invert :: Invertable o => [o] -> [o]
    invert = reverse . map invert

instance (Invertable o1, Invertable o2) => Invertable (Either o1 o2) where
    invert :: (Invertable o1, Invertable o2) => Either o1 o2 -> Either o1 o2
    invert (Left o)  = Left (invert o)
    invert (Right o) = Right (invert o)