{-# LANGUAGE DeriveFunctor #-}
module Category (
  Category,
  union,
  unionAll,
  intersect,
  implementCategory,
  optimize,
  overlap,
  --
  RarityLevel (..),
  SocketColor (..),
  ColorList (..),
  ItemClass (..),
  --
  nothing,
  everything,
  itemLevel,
  dropLevel,
  quality,
  rarity,
  itemClass,
  baseType,
  baseTypes,
  sockets,
  linkedSockets,
  socketGroup,
  width,
  height
  ) where

import           Data.List                    (isInfixOf, nub, (\\))

import           Text.ParserCombinators.ReadP as ReadP
import           Text.Read                    as Read

import           Data.Functor.Classes
import           Data.Functor.Foldable

-- primitive categories
data PrimCat = ItemLevel Ordering Int
             | DropLevel Ordering Int
             | Quality Ordering Int
             | Rarity Ordering RarityLevel
             | Class ItemClass
             | BaseType [String]
             | Sockets Ordering Int
             | LinkedSockets Ordering Int
             | SocketGroup ColorList
             | Height Ordering Int
             | Width Ordering Int
             deriving (Eq,Show)

-- TODO: check for valid values

data RarityLevel = Normal | Magic | Rare | Unique
                 deriving (Eq,Show,Read,Enum,Ord)

data SocketColor = Red | Green | Blue | White
                 deriving (Eq,Enum)

instance Show SocketColor where
  show Red   = "R"
  show Green = "G"
  show Blue  = "B"
  show White = "W"

newtype ColorList = ColorList [SocketColor]
                  deriving Eq

instance Show ColorList where
  show (ColorList xs) = concatMap show xs

data ItemClass = ActiveSkillGems
               | Amulets
               | Belts
               | BodyArmour
               | Boots
               | Bows
               | Claws
               | Daggers
               | DivinationCard
               | FishingRods
               | Gloves
               | Helmets
               | HybridFlasks
               | Jewel
               | LifeFlasks
               | ManaFlasks
               | MapFragments
               | Maps
               | OneHandAxes
               | OneHandMaces
               | OneHandSwords
               | QuestItems
               | Quivers
               | Rings
               | Sceptres
               | Shields
               | StackableCurrency
               | Staves
               | SupportSkillGems
               | ThrustingOneHandSwords
               | TwoHandAxes
               | TwoHandMaces
               | TwoHandSwords
               | UtilityFlasks
               | Wands
               deriving (Eq, Enum)

instance Show ItemClass where
  show ActiveSkillGems        = "Active Skill Gems"
  show Amulets                = "Amulets"
  show Belts                  = "Belts"
  show BodyArmour             = "Body Armour"
  show Boots                  = "Boots"
  show Bows                   = "Bows"
  show Claws                  = "Claws"
  show Daggers                = "Daggers"
  show DivinationCard         = "Divination Card"
  show FishingRods            = "Fishing Rods"
  show Gloves                 = "Gloves"
  show Helmets                = "Helmets"
  show HybridFlasks           = "Hybrid Flasks"
  show Jewel                  = "Jewel"
  show LifeFlasks             = "Life Flasks"
  show ManaFlasks             = "Mana Flasks"
  show MapFragments           = "Map Fragments"
  show Maps                   = "Maps"
  show OneHandAxes            = "One Hand Axes"
  show OneHandMaces           = "One Hand Maces"
  show OneHandSwords          = "One Hand Sword"
  show QuestItems             = "Quest Items"
  show Quivers                = "Quivers"
  show Rings                  = "Rings"
  show Sceptres               = "Sceptres"
  show Shields                = "Shields"
  show StackableCurrency      = "Currency"
  show Staves                 = "Staves"
  show SupportSkillGems       = "Support Skill Gems"
  show ThrustingOneHandSwords = "Thrusting One Hand Swords"
  show TwoHandAxes            = "Two Hand Axes"
  show TwoHandMaces           = "Two Hand Maces"
  show TwoHandSwords          = "Two Hand Swords"
  show UtilityFlasks          = "Utility Flasks"
  show Wands                  = "Wands"

instance Read ItemClass where
  readPrec = Read.choice $ map (read'.show) [ActiveSkillGems .. Wands]

read' :: String -> ReadPrec ItemClass
read' x = lift $ do
  _ <- ReadP.string x
  return $ readItemClass x

readItemClass :: String -> ItemClass
readItemClass "Active Skill Gems"         = ActiveSkillGems
readItemClass "Amulets"                   = Amulets
readItemClass "Belts"                     = Belts
readItemClass "Body Armour"               = BodyArmour
readItemClass "Boots"                     = Boots
readItemClass "Bows"                      = Bows
readItemClass "Claws"                     = Claws
readItemClass "Daggers"                   = Daggers
readItemClass "Divination Card"           = DivinationCard
readItemClass "Fishing Rods"              = FishingRods
readItemClass "Gloves"                    = Gloves
readItemClass "Helmets"                   = Helmets
readItemClass "Hybrid Flasks"             = HybridFlasks
readItemClass "Jewel"                     = Jewel
readItemClass "Life Flasks"               = LifeFlasks
readItemClass "Mana Flasks"               = ManaFlasks
readItemClass "Map Fragments"             = MapFragments
readItemClass "Maps"                      = Maps
readItemClass "One Hand Axes"             = OneHandAxes
readItemClass "One Hand Maces"            = OneHandMaces
readItemClass "One Hand Sword"            = OneHandSwords
readItemClass "Quest Items"               = QuestItems
readItemClass "Quivers"                   = Quivers
readItemClass "Rings"                     = Rings
readItemClass "Sceptres"                  = Sceptres
readItemClass "Shields"                   = Shields
readItemClass "Currency"                  = StackableCurrency
readItemClass "Staves"                    = Staves
readItemClass "Support Skill Gems"        = SupportSkillGems
readItemClass "Thrusting One Hand Swords" = ThrustingOneHandSwords
readItemClass "Two Hand Axes"             = TwoHandAxes
readItemClass "Two Hand Maces"            = TwoHandMaces
readItemClass "Two Hand Swords"           = TwoHandSwords
readItemClass "Utility Flasks"            = UtilityFlasks
readItemClass "Wands"                     = Wands
readItemClass _                           = undefined

-- "real" categories
data FormulaF a f
  = Base a
  | And f f
  | Or f f
  | EmptyFormula
  deriving Functor

type Formula a = Fix (FormulaF a)
data Category = MkC { formula :: Formula PrimCat
                    , dnf     :: CategoryDNF }
-- the dnf for a Category is only evaluated once, and only if needed

instance Show a => Show1 (FormulaF a) where
    --liftShowsPrec :: (Int -> b -> ShowS) -> ([b] -> ShowS) -> Int -> FormulaF a -> (String -> String)
    liftShowsPrec _ _ _ EmptyFormula = showString "EmptyFormula "
    liftShowsPrec _ _ d (Base x) = showString "Base " . showsPrec d x
    liftShowsPrec _ g _ (And x1 x2) = showString "And " . g [x1, x2]
    liftShowsPrec _ g _ (Or x1 x2) = showString "Or " . g [x1, x2]

instance Show Category where
    show (MkC f _) = show f

newtype DNF a = DNF {conditions :: [[a]]}
type CategoryDNF = DNF PrimCat

makeCategory :: Formula PrimCat -> Category
makeCategory f = MkC f (computeDnf f)

emptyCategory :: Category
emptyCategory = MkC (Fix EmptyFormula) (DNF [])

computeDnf :: Formula a -> DNF a
computeDnf = cata phi where
  phi (Base x)                  = DNF [[x]]
  phi (And (DNF xss) (DNF yss)) = DNF [a ++ b | a <- xss, b <- yss]
  phi (Or (DNF xss) (DNF yss))  = DNF $ xss ++ yss
  phi EmptyFormula              = DNF []

union :: Category -> Category -> Category
union x (MkC (Fix EmptyFormula) _) = x
union (MkC (Fix EmptyFormula) _) y = y
union (MkC f _) (MkC f' _)         = makeCategory (Fix $ Or f f')

intersect :: Category -> Category -> Category
intersect _ (MkC (Fix EmptyFormula) _) = emptyCategory
intersect (MkC (Fix EmptyFormula) _) _ = emptyCategory
intersect (MkC f _) (MkC f' _)      = makeCategory (Fix $ And f f')

-- utility
unionAll :: [Category] -> Category
unionAll = foldl union nothing

primitive :: PrimCat -> Category
primitive = makeCategory . Fix . Base

-- optimization/simplification functions
-- optimize tries to find obviously empty conditions and removes them
{-# DEPRECATED optimize "optimization is now the default behaviour" #-}
optimize :: Category -> Category
optimize = id

optimize' :: CategoryDNF -> CategoryDNF
optimize' = filterClassConflicts

filterClassConflicts :: CategoryDNF -> CategoryDNF
filterClassConflicts (DNF xss) = DNF $ filter (not.hasClassConflict) xss

hasClassConflict :: [PrimCat] -> Bool
hasClassConflict = (>1).length.nub.filter isClassCond
  where isClassCond (Class _) = True
        isClassCond _         = False

-- check two categories for overlap
overlap :: Category -> Category -> Bool
overlap c c' = or $ overlapPrimCats <$> xss <*> yss where
    (DNF xss, DNF yss) = (dnf c, dnf c')

overlapPrimCats :: [PrimCat] -> [PrimCat] -> Bool
overlapPrimCats xs ys = or $ primOverlap <$> xs <*> ys

primOverlap :: PrimCat -> PrimCat -> Bool
primOverlap (ItemLevel o n) (ItemLevel o' n')
    = satisfiable [(o,n),(o',n')] [1..100]
primOverlap (DropLevel o n) (DropLevel o' n')
    = satisfiable [(o,n),(o',n')] [1..100]
primOverlap (Quality o n) (Quality o' n')
    = satisfiable [(o,n),(o',n')] [0..20]
primOverlap (Rarity o r) (Rarity o' r')
    = satisfiable [(o,r),(o',r')] [Normal .. Unique]
primOverlap (Class c) (Class c')
    = c == c'
primOverlap (BaseType ss) (BaseType ss')
    = any (uncurry isInfixOf) [(s, s') | s <- ss ++ ss', s' <- ss ++ ss' ]
primOverlap (Sockets o n) (Sockets o' n')
    = satisfiable [(o,n),(o',n')] [0..6]
primOverlap (LinkedSockets o n) (LinkedSockets o' n')
    = satisfiable [(o,n),(o',n')] [0..6]
primOverlap (SocketGroup (ColorList cs)) (SocketGroup (ColorList cs'))
    = not . null $ cs \\ cs'
primOverlap (Height o n) (Height o' n')
    = satisfiable [(o,n),(o',n')] [1..5]
primOverlap (Width o n) (Width o' n')
    = satisfiable [(o,n),(o',n')] [1..5]
primOverlap _ _ = True -- two different PrimCats always overlap

satisfiable :: Ord a => [(Ordering, a)] -> [a] -> Bool
satisfiable = check or and

check :: Ord a => ([Bool] -> Bool) -- outer collapse func.
                -> ([Bool] -> Bool) -- inner collapse func.
                -> [(Ordering,a)] -- conditions
                -> [a] -- values
                -> Bool
check f g cs = f . map (g . makeList) where
  makeList j = map (\(ord,i) -> compare j i == ord) cs

-- generation functions
nothing :: Category
nothing = makeCategory $ Fix EmptyFormula

everything :: Category
everything = MkC (error "Error: Trying to access universal formula!") (DNF [[]])

itemLevel :: Ordering -> Int -> Category
dropLevel :: Ordering -> Int -> Category
quality :: Ordering -> Int -> Category
rarity :: Ordering -> RarityLevel -> Category
itemClass :: ItemClass -> Category
baseType :: String -> Category
baseTypes :: [String] -> Category
sockets :: Ordering -> Int -> Category
linkedSockets :: Ordering -> Int -> Category
socketGroup :: ColorList -> Category
height :: Ordering -> Int -> Category
width :: Ordering -> Int -> Category
itemLevel o n = primitive $ ItemLevel o n
dropLevel o n = primitive $ DropLevel o n
quality o n = primitive $ Quality o n
rarity o r = primitive $ Rarity o r
itemClass c = primitive $ Class c
baseType t = primitive $ BaseType [t]
baseTypes ts = primitive $ BaseType ts
sockets o n = primitive $ Sockets o n
linkedSockets o n = primitive $ LinkedSockets o n
socketGroup cs = primitive $ SocketGroup cs
height o n = primitive $ Height o n
width o n = primitive $ Width o n

-- create filter code for category
-- each list element goes into an extra Show-block
implementCategory :: Category -> [String]
implementCategory = map (concatMap implementPrimCat) . conditions . optimize' . dnf

implementPrimCat :: PrimCat -> String
implementPrimCat (ItemLevel ord n) = orderedParameter "ItemLevel" ord n
implementPrimCat (DropLevel ord n) = orderedParameter "DropLevel" ord n
implementPrimCat (Quality ord n) = orderedParameter "Quality" ord n
implementPrimCat (Rarity ord r) = orderedParameter "Rarity" ord r
implementPrimCat (Class c) = genericParameter "Class" (show c)
implementPrimCat (BaseType ts) = parameter "BaseType" $ unwords $ map show ts
implementPrimCat (Sockets ord n) = orderedParameter "Sockets" ord n
implementPrimCat (LinkedSockets ord n) = orderedParameter "LinkedSockets" ord n
implementPrimCat (SocketGroup cs) = genericParameter "SocketGroup" cs
implementPrimCat (Height ord n) = orderedParameter "Height" ord n
implementPrimCat (Width ord n) = orderedParameter "Width" ord n

genericParameter :: Show a => String -> a -> String
genericParameter keyword arg = parameter keyword (show arg)

orderedParameter :: Show a => String -> Ordering -> a -> String
orderedParameter keyword ord arg = parameter keyword (comperator ord ++ show arg)

parameter :: String -> String -> String
parameter keyword arg = keyword ++ " " ++ arg ++ newline

comperator :: Ordering -> String
comperator LT = "< "
comperator EQ = " "
comperator GT = "> "

newline :: String
newline = "\n"
