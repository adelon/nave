module Report.Region where

https://gitlab.haskell.org/ghc/ghc/-/merge_requests/2626


https://hackage.haskell.org/package/pretty-simple-3.2.2.0/docs/Text-Pretty-Simple.html

import Base


-- `Loc` wraps a value with location information specified by a `Region`.
-- If the `Region` of `la :: Loc a` is `Nowhere`, we call `la` pure, since
-- `la = pure a` for some `a :: a`.
--
data Loc a = At {unLoc :: a, locRegion :: Region} deriving (Show)

-- Equality tests ignore the location information, so that we can match
-- located values from the token stream against pure values with functions
-- such as `token`. The same holds for `Ord`.
--
instance Eq a => Eq (Loc a) where
   (==) = (==) `on` unLoc

instance Ord a => Ord (Loc a) where
   compare = compare `on` unLoc

instance Functor Loc where
   fmap :: (a -> b) -> Loc a -> Loc b
   fmap f (a `At` loc) = f a `At` loc

instance Applicative Loc where
   pure :: a -> Loc a
   pure a = a `At` mempty

   (<*>) :: Loc (a -> b) -> Loc a -> Loc b
   f `At` r1 <*> a `At` r2 = f a `At` (r1 <> r2)


data Position = Position
   { positionLine :: Int   -- Line of the position, starting at 1.
   , positionColumn :: Int -- Column of the position, starting at 1.
   , positionOffset :: Int -- Index of the position, starting at 0.
   } deriving (Show, Eq, Ord)

compareLine :: Position -> Position -> Ordering
compareLine = compare `on` positionLine


data Region
   = Region Position Position Indicator
--          ^^^^^^^^ ^^^^^^^^ ^^^^^^^^^
--          Start    End
--
-- Source regions are indicated by start and end position.
-- The start is inclusive, the end is exclusive. Thus below
--
--    1 3 5 7 9
--    -----------
-- 1| Hey Hello
-- 2|
--
-- the word 'Hello' has the region `1:5-1:10`. This has the nice property
-- that the length of the region is equal to the difference of the columns
-- if the region is on the same line. The start position must be strictly
-- less than the end position of the region.
--
   | Nowhere
--
-- `Nowhere` is intended for errors without source regions or
-- for lifting pure values into a located value.
--
   deriving (Show, Eq, Ord)

regionOffset :: Region -> Maybe Int
regionOffset (Region s _e _i) = Just (positionOffset s)
regionOffset Nowhere = Nothing

-- Regions form a commutative monoid under taking the convex hull of
-- their unions. The empty region `Nowhere` acts is the neutral element.
--
convexUnion :: Region -> Region -> Region
convexUnion r1 r2 = case (r1, r2) of
   (Region s1 e1 i1, Region s2 e2 i2) -> Region s e i
      where
         s = min s1 s2
         e = max e1 e2
         i = case (i1, i2) of
            (_, MultiLine)                         -> MultiLine
            (MultiLine, _)                         -> MultiLine
            _ | positionLine s1 == positionLine e2 -> SingleLine (positionColumn e - positionColumn s)
            _                                      -> MultiLine
   (_, Nowhere)                       -> r1
   (Nowhere, _)                       -> r2

instance Semigroup Region where
   (<>) :: Region -> Region -> Region
   (<>) = convexUnion

instance Monoid Region where
   mempty :: Region
   mempty = Nowhere



data Indicator
   = MultiLine
   | SingleLine Int
--              ^^^
--              Length of the region within that line.
--
   deriving (Show, Eq, Ord)

-- length :: Region -> Length
-- length = \case
--    Nowhere -> WasNowhere
--    Region s e -> case compareLine s e of
--       EQ -> SameLine (positionColumn e - positionColumn s)
--       _  -> DifferentLine