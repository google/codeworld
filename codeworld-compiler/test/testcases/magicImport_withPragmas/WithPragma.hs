-- Enable a language extension that's not enabled in CodeWorld,
-- Just to test that the pragma is preserved.

{-# LANGUAGE MultiParamTypeClasses #-}

class PlusOne a b where
    plusOne :: a -> b

instance PlusOne Number Number where
    plusOne a = a + 1

foo :: Number
foo = plusOne 41
